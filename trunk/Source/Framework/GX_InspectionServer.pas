unit GX_InspectionServer;

// TCP-based UI inspection server for GExperts.
// Allows an external CLI tool (GxInspect) to query form/control properties
// and take screenshots of the running IDE. Activated by placing a
// GxInspect.port file in the Binaries directory before launching the IDE.
//
// When GX_INSPECT_ENABLED is not defined, this unit compiles a dummy
// TGxInspectServer whose Create does nothing and Port returns 0.
//
// Protocol: line-based text over TCP (localhost only).
// Client sends one command line, server responds with lines terminated by "END".

{$I GX_CondDefine.inc}

interface

{$IFDEF GX_INSPECT_ENABLED}
uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, Graphics, SyncObjs;
{$ELSE}
uses
  SysUtils;
{$ENDIF}

{$IFDEF GX_INSPECT_ENABLED}
type
{$IFDEF WIN64}
  TSocketHandle = UInt64;
{$ELSE}
  TSocketHandle = Integer;
{$ENDIF}

  TGxInspectServer = class;

  TGxInspectThread = class(TThread)
  private
    FServer: TGxInspectServer;
    FSocket: TSocketHandle;
    FPort: Integer;
    procedure HandleClient(ClientSocket: TSocketHandle);
    function RecvLine(Sock: TSocketHandle; out Line: string): Boolean;
    procedure SendString(Sock: TSocketHandle; const S: string);
  protected
    procedure Execute; override;
  public
    constructor Create(AServer: TGxInspectServer; APort: Integer);
  end;

  TGxInspectServer = class
  private
    FPort: Integer;
    FThread: TGxInspectThread;
    FHiddenWnd: HWND;
    FPendingCommand: string;
    FPendingResult: string;
    FResultEvent: TEvent;
    FCritSect: TCriticalSection;
    procedure HiddenWndProc(var Msg: TMessage);
    function ProcessCommand(const Cmd: string): string;
    function CmdVersion: string;
    function CmdForms: string;
    function CmdControls(const FormId: string): string;
    function CmdScreenshot(const FormId, FilePath: string): string;
    function CmdBounds(const FormId: string): string;
    function CmdCheck(const FormId: string): string;
    function FindForm(const FormId: string): TCustomForm;
    procedure EnumControls(AParent: TWinControl; ALines: TStrings; AIndent: Integer);
    function GetControlCaption(AControl: TControl): string;
    procedure CheckOverlaps(AParent: TWinControl; ALines: TStrings);
    procedure CheckClipping(AParent: TWinControl; ALines: TStrings);
    procedure CheckOutside(AParent: TWinControl; ALines: TStrings);
    procedure CheckZeroSize(AParent: TWinControl; ALines: TStrings);
  public
    constructor Create(APort: Integer);
    destructor Destroy; override;
    function ExecuteOnMainThread(const Cmd: string): string;
    property Port: Integer read FPort;
  end;
{$ELSE}
type
  TGxInspectServer = class
  private
    FPort: Integer;
  public
    constructor Create(APort: Integer);
    property Port: Integer read FPort;
  end;
{$ENDIF}

const
  PortFileName = 'GxInspect.port';

function ReadPortFile: Integer;

implementation

{$IFDEF GX_INSPECT_ENABLED}

uses
  TypInfo, Jpeg, GX_GetIdeVersion, GX_GenericUtils;

type
  TControlCracker = class(TControl);

function GetDllDir: string;
begin
  Result := ExtractFilePath(GetModuleName(HInstance));
end;

function ReadPortFile: Integer;
var
  FileName: string;
  F: TextFile;
  S: string;
begin
  Result := 0;
  FileName := GetDllDir + PortFileName;
  if not FileExists(FileName) then
    Exit;
  AssignFile(F, FileName);
  try
    Reset(F);
    try
      if not Eof(F) then begin
        ReadLn(F, S);
        S := Trim(S);
        Result := StrToIntDef(S, 0);
      end;
    finally
      CloseFile(F);
    end;
  except
    Result := 0;
  end;
end;


const
  WM_GX_INSPECT = WM_USER + $0F01;
  SOCKET_ERROR_VALUE = -1;

// WinSock types and functions (inline declarations for Delphi 6 compatibility)
type
  TWSAData = packed record
    wVersion: Word;
    wHighVersion: Word;
    szDescription: array[0..256] of AnsiChar;
    szSystemStatus: array[0..128] of AnsiChar;
    iMaxSockets: Word;
    iMaxUdpDg: Word;
    lpVendorInfo: PAnsiChar;
  end;

  TSockAddrIn = packed record
    sin_family: Word;
    sin_port: Word;
    sin_addr: Cardinal;
    sin_zero: array[0..7] of AnsiChar;
  end;

const
  INVALID_SOCKET_VALUE = TSocketHandle(not 0);
  AF_INET = 2;
  SOCK_STREAM = 1;
  IPPROTO_TCP = 6;
  INADDR_LOOPBACK = $7F000001;
  SOL_SOCKET_ = $FFFF;
  SO_REUSEADDR_ = $0004;

function WSAStartup(wVersionRequired: Word; var lpWSAData: TWSAData): Integer; stdcall; external 'ws2_32.dll';
function WSACleanup: Integer; stdcall; external 'ws2_32.dll';
function ws_socket(af, type_, protocol: Integer): TSocketHandle; stdcall; external 'ws2_32.dll' name 'socket';
function ws_bind(s: TSocketHandle; var name: TSockAddrIn; namelen: Integer): Integer; stdcall; external 'ws2_32.dll' name 'bind';
function ws_listen(s: TSocketHandle; backlog: Integer): Integer; stdcall; external 'ws2_32.dll' name 'listen';
function ws_accept(s: TSocketHandle; addr: Pointer; addrlen: Pointer): TSocketHandle; stdcall; external 'ws2_32.dll' name 'accept';
function ws_closesocket(s: TSocketHandle): Integer; stdcall; external 'ws2_32.dll' name 'closesocket';
function ws_send(s: TSocketHandle; const buf; len, flags: Integer): Integer; stdcall; external 'ws2_32.dll' name 'send';
function ws_recv(s: TSocketHandle; var buf; len, flags: Integer): Integer; stdcall; external 'ws2_32.dll' name 'recv';
function ws_setsockopt(s: TSocketHandle; level, optname: Integer; const optval; optlen: Integer): Integer; stdcall; external 'ws2_32.dll' name 'setsockopt';
function ws_htons(hostshort: Word): Word; stdcall; external 'ws2_32.dll' name 'htons';
function ws_htonl(hostlong: Cardinal): Cardinal; stdcall; external 'ws2_32.dll' name 'htonl';
// PrintWindow from user32.dll (not declared in older Delphi versions)
function PrintWindow(hwnd: HWND; hdcBlt: HDC; nFlags: UINT): BOOL; stdcall; external 'user32.dll';
function GetWindowDC(hWnd: HWND): HDC; stdcall; external 'user32.dll';
function ReleaseDC(hWnd: HWND; hDC: HDC): Integer; stdcall; external 'user32.dll';

function RectToStr(const R: TRect): string;
begin
  Result := Format('[%d,%d %dx%d]', [R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top]);
end;

function BoundsToStr(ALeft, ATop, AWidth, AHeight: Integer): string;
begin
  Result := Format('[%d,%d %dx%d]', [ALeft, ATop, AWidth, AHeight]);
end;

function RectsIntersect(const R1, R2: TRect): Boolean;
begin
  Result := (R1.Left < R2.Right) and (R1.Right > R2.Left) and
            (R1.Top < R2.Bottom) and (R1.Bottom > R2.Top);
end;

function GetCaptionViaRtti(AControl: TControl): string;
var
  PropInfoPtr: PPropInfo;
begin
  Result := '';
  PropInfoPtr := GetPropInfo(AControl, 'Caption');
  if PropInfoPtr <> nil then
    Result := GetStrProp(AControl, PropInfoPtr);
end;

function FormDpiStr(AForm: TCustomForm): string;
var
  PropInfoPtr: PPropInfo;
begin
  PropInfoPtr := GetPropInfo(AForm, 'PixelsPerInch');
  if PropInfoPtr <> nil then
    Result := ' DPI=' + IntToStr(GetOrdProp(AForm, PropInfoPtr))
  else
    Result := '';
end;

{ TGxInspectThread }

constructor TGxInspectThread.Create(AServer: TGxInspectServer; APort: Integer);
begin
  FServer := AServer;
  FPort := APort;
  FSocket := INVALID_SOCKET_VALUE;
  FreeOnTerminate := False;
  inherited Create(False);
end;

procedure TGxInspectThread.Execute;
var
  Addr: TSockAddrIn;
  OptVal: Integer;
  ClientSock: TSocketHandle;
begin
  FSocket := ws_socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if FSocket = INVALID_SOCKET_VALUE then
    Exit;
  try
    OptVal := 1;
    ws_setsockopt(FSocket, SOL_SOCKET_, SO_REUSEADDR_, OptVal, SizeOf(OptVal));

    FillChar(Addr, SizeOf(Addr), 0);
    Addr.sin_family := AF_INET;
    Addr.sin_port := ws_htons(Word(FPort));
    Addr.sin_addr := ws_htonl(INADDR_LOOPBACK);

    if ws_bind(FSocket, Addr, SizeOf(Addr)) = SOCKET_ERROR_VALUE then
      Exit;
    if ws_listen(FSocket, 5) = SOCKET_ERROR_VALUE then
      Exit;

    while not Terminated do begin
      ClientSock := ws_accept(FSocket, nil, nil);
      if ClientSock = INVALID_SOCKET_VALUE then
        Break;
      try
        HandleClient(ClientSock);
      finally
        ws_closesocket(ClientSock);
      end;
    end;
  finally
    if FSocket <> INVALID_SOCKET_VALUE then begin
      ws_closesocket(FSocket);
      FSocket := INVALID_SOCKET_VALUE;
    end;
  end;
end;

procedure TGxInspectThread.HandleClient(ClientSocket: TSocketHandle);
var
  Line: string;
  Response: string;
begin
  while RecvLine(ClientSocket, Line) and not Terminated do begin
    Line := Trim(Line);
    if Line = '' then
      Continue;
    Response := FServer.ExecuteOnMainThread(Line);
    SendString(ClientSocket, Response);
    SendString(ClientSocket, 'END'#13#10);
  end;
end;

function TGxInspectThread.RecvLine(Sock: TSocketHandle; out Line: string): Boolean;
var
  Ch: AnsiChar;
  Ret: Integer;
  Buf: string;
begin
  Result := False;
  Buf := '';
  while True do begin
    Ret := ws_recv(Sock, Ch, 1, 0);
    if Ret <= 0 then
      Exit;
    if Ch = #10 then begin
      Line := Buf;
      Result := True;
      Exit;
    end;
    if Ch <> #13 then
      Buf := Buf + Char(Ch);
  end;
end;

procedure TGxInspectThread.SendString(Sock: TSocketHandle; const S: string);
var
  AnsiStr: AnsiString;
begin
  AnsiStr := AnsiString(S);
  if Length(AnsiStr) > 0 then
    ws_send(Sock, AnsiStr[1], Length(AnsiStr), 0);
end;

{ TGxInspectServer }

constructor TGxInspectServer.Create(APort: Integer);
var
  WsaData: TWSAData;
begin
  inherited Create;
  FPort := APort;
  WSAStartup($0202, WsaData);
  FCritSect := TCriticalSection.Create;
  FResultEvent := TEvent.Create(nil, True, False, '');
  FHiddenWnd := Classes.AllocateHWnd(HiddenWndProc);
  FThread := TGxInspectThread.Create(Self, FPort);
end;

destructor TGxInspectServer.Destroy;
begin
  if Assigned(FThread) then begin
    FThread.Terminate;
    // Close the listening socket to unblock the accept() call
    if FThread.FSocket <> INVALID_SOCKET_VALUE then
      ws_closesocket(FThread.FSocket);
    FThread.WaitFor;
    FThread.Free;
  end;
  if FHiddenWnd <> 0 then
    Classes.DeallocateHWnd(FHiddenWnd);
  FreeAndNil(FResultEvent);
  FreeAndNil(FCritSect);
  WSACleanup;
  inherited;
end;

function TGxInspectServer.ExecuteOnMainThread(const Cmd: string): string;
begin
  FCritSect.Enter;
  try
    FPendingCommand := Cmd;
    FResultEvent.ResetEvent;
    PostMessage(FHiddenWnd, WM_GX_INSPECT, 0, 0);
    if FResultEvent.WaitFor(10000) = wrSignaled then
      Result := FPendingResult
    else
      Result := 'ERROR Timeout waiting for main thread'#13#10;
  finally
    FCritSect.Leave;
  end;
end;

procedure TGxInspectServer.HiddenWndProc(var Msg: TMessage);
begin
  if Msg.Msg = WM_GX_INSPECT then begin
    try
      FPendingResult := ProcessCommand(FPendingCommand);
    except
      on E: Exception do
        FPendingResult := 'ERROR ' + E.Message + #13#10;
    end;
    FResultEvent.SetEvent;
  end else
    Msg.Result := DefWindowProc(FHiddenWnd, Msg.Msg, Msg.WParam, Msg.LParam);
end;

function TGxInspectServer.ProcessCommand(const Cmd: string): string;
var
  SpacePos: Integer;
  Command, Args: string;
  SecondSpace: Integer;
  Arg1, Arg2: string;
begin
  SpacePos := Pos(' ', Cmd);
  if SpacePos > 0 then begin
    Command := UpperCase(Copy(Cmd, 1, SpacePos - 1));
    Args := Copy(Cmd, SpacePos + 1, MaxInt);
  end else begin
    Command := UpperCase(Cmd);
    Args := '';
  end;

  Args := Trim(Args);

  if Command = 'FORMS' then
    Result := CmdForms
  else if Command = 'CONTROLS' then
    Result := CmdControls(Args)
  else if Command = 'SCREENSHOT' then begin
    // Support quoted form-id: SCREENSHOT "Form Caption" C:\path
    if (Length(Args) > 0) and (Args[1] = '"') then begin
      SecondSpace := Pos('"', Copy(Args, 2, MaxInt));
      if SecondSpace > 0 then begin
        Arg1 := Copy(Args, 2, SecondSpace - 1);
        Arg2 := Trim(Copy(Args, SecondSpace + 2, MaxInt));
      end else begin
        Arg1 := Args;
        Arg2 := '';
      end;
    end else begin
      SecondSpace := Pos(' ', Args);
      if SecondSpace > 0 then begin
        Arg1 := Copy(Args, 1, SecondSpace - 1);
        Arg2 := Copy(Args, SecondSpace + 1, MaxInt);
      end else begin
        Arg1 := Args;
        Arg2 := '';
      end;
    end;
    Result := CmdScreenshot(Arg1, Arg2)
  end else if Command = 'BOUNDS' then
    Result := CmdBounds(Args)
  else if Command = 'CHECK' then
    Result := CmdCheck(Args)
  else if Command = 'PING' then
    Result := 'PONG'#13#10
  else if Command = 'VERSION' then
    Result := CmdVersion
  else
    Result := 'ERROR Unknown command: ' + Command + #13#10;
end;

function TGxInspectServer.FindForm(const FormId: string): TCustomForm;
var
  i: Integer;
  Form: TCustomForm;
  Id: string;
begin
  Result := nil;
  Id := Trim(FormId);
  // Strip surrounding quotes
  if (Length(Id) >= 2) and (Id[1] = '"') and (Id[Length(Id)] = '"') then
    Id := Copy(Id, 2, Length(Id) - 2);

  for i := 0 to Screen.CustomFormCount - 1 do begin
    Form := Screen.CustomForms[i];
    // Match by class name (exact, case-insensitive)
    if SameText(Form.ClassName, Id) then begin
      Result := Form;
      Exit;
    end;
    // Match by caption (substring, case-insensitive)
    if (Id <> '') and (Pos(UpperCase(Id), UpperCase(Form.Caption)) > 0) then begin
      Result := Form;
      Exit;
    end;
    // Match by Name property
    if SameText(Form.Name, Id) then begin
      Result := Form;
      Exit;
    end;
  end;
end;

function TGxInspectServer.GetControlCaption(AControl: TControl): string;
begin
  Result := '';
  if AControl is TCustomForm then
    Result := TCustomForm(AControl).Caption
  else if AControl is TWinControl then begin
    SetLength(Result, 256);
    SetLength(Result, SendMessage(TWinControl(AControl).Handle, WM_GETTEXT,
      256, LPARAM(PChar(Result))));
  end else
    Result := GetCaptionViaRtti(AControl);
end;

function TGxInspectServer.CmdVersion: string;
begin
  Result := 'IDE: ' + GetBorlandIdeVersionStr + #13#10;
{$IFDEF WIN64}
  Result := Result + 'Platform: Win64' + #13#10;
{$ELSE}
  Result := Result + 'Platform: Win32' + #13#10;
{$ENDIF}
  Result := Result + 'GExperts: ' + GetFileVersionString(ThisDllName, True, False) + #13#10;
end;

function TGxInspectServer.CmdForms: string;
var
  i: Integer;
  Form: TCustomForm;
  Lines: TStringList;
  VisStr: string;
begin
  Lines := TStringList.Create;
  try
    for i := 0 to Screen.CustomFormCount - 1 do begin
      Form := Screen.CustomForms[i];
      if Form.Visible then
        VisStr := ' Visible'
      else
        VisStr := ' Hidden';
      Lines.Add(Form.ClassName + ' "' + Form.Caption + '" ' +
        BoundsToStr(Form.Left, Form.Top, Form.Width, Form.Height) +
        VisStr + FormDpiStr(Form));
    end;
    Result := Lines.Text;
  finally
    Lines.Free;
  end;
end;

procedure TGxInspectServer.EnumControls(AParent: TWinControl; ALines: TStrings; AIndent: Integer);
var
  i: Integer;
  Ctrl: TControl;
  Prefix: string;
  Caption: string;
  VisStr, EnStr: string;
begin
  Prefix := StringOfChar(' ', AIndent);
  for i := 0 to AParent.ControlCount - 1 do begin
    Ctrl := AParent.Controls[i];
    Caption := GetControlCaption(Ctrl);
    if Ctrl.Visible then
      VisStr := ' Visible'
    else
      VisStr := ' Hidden';
    if Ctrl.Enabled then
      EnStr := ' Enabled'
    else
      EnStr := ' Disabled';
    ALines.Add(Prefix + Ctrl.ClassName + ' ' + Ctrl.Name + ' ' +
      BoundsToStr(Ctrl.Left, Ctrl.Top, Ctrl.Width, Ctrl.Height) +
      ' "' + Caption + '"' + VisStr + EnStr);
    if Ctrl is TWinControl then
      EnumControls(TWinControl(Ctrl), ALines, AIndent + 2);
  end;
end;

function TGxInspectServer.CmdControls(const FormId: string): string;
var
  Form: TCustomForm;
  Lines: TStringList;
begin
  Form := FindForm(FormId);
  if Form = nil then begin
    Result := 'ERROR Form not found: ' + FormId + #13#10;
    Exit;
  end;
  Lines := TStringList.Create;
  try
    EnumControls(Form, Lines, 0);
    Result := Lines.Text;
  finally
    Lines.Free;
  end;
end;

function TGxInspectServer.CmdScreenshot(const FormId, FilePath: string): string;
var
  Form: TCustomForm;
  Bmp: TBitmap;
  Jpg: TJPEGImage;
  WinDC: HDC;
  Ext: string;
begin
  if FilePath = '' then begin
    Result := 'ERROR Usage: SCREENSHOT <form-id> <filepath>'#13#10;
    Exit;
  end;
  Form := FindForm(FormId);
  if Form = nil then begin
    Result := 'ERROR Form not found: ' + FormId + #13#10;
    Exit;
  end;
  Bmp := TBitmap.Create;
  try
    Bmp.Width := Form.Width;
    Bmp.Height := Form.Height;
    Bmp.PixelFormat := pf24bit;
    // Use BitBlt from the window DC - more reliable than PrintWindow
    WinDC := GetWindowDC(Form.Handle);
    try
      BitBlt(Bmp.Canvas.Handle, 0, 0, Form.Width, Form.Height, WinDC, 0, 0, SRCCOPY);
    finally
      ReleaseDC(Form.Handle, WinDC);
    end;
    try
      Ext := LowerCase(ExtractFileExt(FilePath));
      if (Ext = '.jpg') or (Ext = '.jpeg') then begin
        Jpg := TJPEGImage.Create;
        try
          Jpg.Assign(Bmp);
          Jpg.CompressionQuality := 90;
          Jpg.SaveToFile(FilePath);
        finally
          Jpg.Free;
        end;
      end else
        Bmp.SaveToFile(FilePath);
      Result := 'OK ' + FilePath + #13#10;
    except
      on E: Exception do
        Result := 'ERROR ' + E.Message + #13#10;
    end;
  finally
    Bmp.Free;
  end;
end;

function TGxInspectServer.CmdBounds(const FormId: string): string;
var
  Form: TCustomForm;
  Lines: TStringList;
  MonRect: TRect;
begin
  Form := FindForm(FormId);
  if Form = nil then begin
    Result := 'ERROR Form not found: ' + FormId + #13#10;
    Exit;
  end;
  Lines := TStringList.Create;
  try
    Lines.Add('Class: ' + Form.ClassName);
    Lines.Add('Caption: ' + Form.Caption);
    Lines.Add('Left: ' + IntToStr(Form.Left));
    Lines.Add('Top: ' + IntToStr(Form.Top));
    Lines.Add('Width: ' + IntToStr(Form.Width));
    Lines.Add('Height: ' + IntToStr(Form.Height));
    Lines.Add('ClientWidth: ' + IntToStr(Form.ClientWidth));
    Lines.Add('ClientHeight: ' + IntToStr(Form.ClientHeight));
    if Form.Monitor <> nil then begin
      MonRect := Form.Monitor.BoundsRect;
      Lines.Add('Monitor: ' + IntToStr(Form.Monitor.MonitorNum) +
        ' ' + RectToStr(MonRect));
    end;
    if Form.Visible then
      Lines.Add('Visible: True')
    else
      Lines.Add('Visible: False');
    Lines.Add(FormDpiStr(Form));
    Result := Lines.Text;
  finally
    Lines.Free;
  end;
end;

function TGxInspectServer.CmdCheck(const FormId: string): string;
var
  Form: TCustomForm;
  Lines: TStringList;
begin
  Form := FindForm(FormId);
  if Form = nil then begin
    Result := 'ERROR Form not found: ' + FormId + #13#10;
    Exit;
  end;
  Lines := TStringList.Create;
  try
    CheckOverlaps(Form, Lines);
    CheckOutside(Form, Lines);
    CheckClipping(Form, Lines);
    CheckZeroSize(Form, Lines);
    if Lines.Count = 0 then
      Lines.Add('OK');
    Result := Lines.Text;
  finally
    Lines.Free;
  end;
end;

procedure TGxInspectServer.CheckOverlaps(AParent: TWinControl; ALines: TStrings);
var
  i, j: Integer;
  C1, C2: TControl;
  R1, R2: TRect;
begin
  for i := 0 to AParent.ControlCount - 2 do begin
    C1 := AParent.Controls[i];
    if not C1.Visible then Continue;
    R1 := Bounds(C1.Left, C1.Top, C1.Width, C1.Height);
    for j := i + 1 to AParent.ControlCount - 1 do begin
      C2 := AParent.Controls[j];
      if not C2.Visible then Continue;
      R2 := Bounds(C2.Left, C2.Top, C2.Width, C2.Height);
      if RectsIntersect(R1, R2) then
        ALines.Add('OVERLAP: ' + C1.Name + ' ' +
          BoundsToStr(C1.Left, C1.Top, C1.Width, C1.Height) +
          ' and ' + C2.Name + ' ' +
          BoundsToStr(C2.Left, C2.Top, C2.Width, C2.Height) +
          ' intersect');
    end;
  end;
  for i := 0 to AParent.ControlCount - 1 do
    if AParent.Controls[i] is TWinControl then
      CheckOverlaps(TWinControl(AParent.Controls[i]), ALines);
end;

procedure TGxInspectServer.CheckOutside(AParent: TWinControl; ALines: TStrings);
var
  i: Integer;
  Ctrl: TControl;
  ClientW, ClientH: Integer;
begin
  ClientW := AParent.ClientWidth;
  ClientH := AParent.ClientHeight;
  for i := 0 to AParent.ControlCount - 1 do begin
    Ctrl := AParent.Controls[i];
    if not Ctrl.Visible then Continue;
    if (Ctrl.Left + Ctrl.Width > ClientW) or (Ctrl.Top + Ctrl.Height > ClientH) or
       (Ctrl.Left < 0) or (Ctrl.Top < 0) then
      ALines.Add('OUTSIDE: ' + Ctrl.Name + ' ' +
        BoundsToStr(Ctrl.Left, Ctrl.Top, Ctrl.Width, Ctrl.Height) +
        ' extends beyond parent ' + AParent.Name +
        ' client ' + BoundsToStr(0, 0, ClientW, ClientH));
    if Ctrl is TWinControl then
      CheckOutside(TWinControl(Ctrl), ALines);
  end;
end;

procedure TGxInspectServer.CheckClipping(AParent: TWinControl; ALines: TStrings);
var
  i: Integer;
  Ctrl: TControl;
  Caption: string;
  TextW: Integer;
  Padding: Integer;
  Bmp: TBitmap;
begin
  Bmp := TBitmap.Create;
  try
    for i := 0 to AParent.ControlCount - 1 do begin
      Ctrl := AParent.Controls[i];
      if not Ctrl.Visible then Continue;
      Caption := GetControlCaption(Ctrl);
      if Caption <> '' then begin
        Bmp.Canvas.Font.Assign(TControlCracker(Ctrl).Font);
        TextW := Bmp.Canvas.TextWidth(Caption);
        if (Ctrl.ClassName = 'TButton') or (Pos('Button', Ctrl.ClassName) > 0) then begin
          Padding := 16;
          if TextW + Padding > Ctrl.Width then
            ALines.Add('CLIPPED: ' + Ctrl.Name +
              ' text "' + Caption + '" needs ' + IntToStr(TextW + Padding) +
              'px but control is ' + IntToStr(Ctrl.Width) + 'px wide');
        end else if (Ctrl.ClassName = 'TLabel') or (Pos('Label', Ctrl.ClassName) > 0) then begin
          Padding := 2;
          if TextW + Padding > Ctrl.Width then
            ALines.Add('CLIPPED: ' + Ctrl.Name +
              ' text "' + Caption + '" needs ' + IntToStr(TextW + Padding) +
              'px but control is ' + IntToStr(Ctrl.Width) + 'px wide');
        end;
      end;
      if Ctrl is TWinControl then
        CheckClipping(TWinControl(Ctrl), ALines);
    end;
  finally
    Bmp.Free;
  end;
end;

procedure TGxInspectServer.CheckZeroSize(AParent: TWinControl; ALines: TStrings);
var
  i: Integer;
  Ctrl: TControl;
  Dimension: string;
begin
  for i := 0 to AParent.ControlCount - 1 do begin
    Ctrl := AParent.Controls[i];
    if Ctrl.Visible and ((Ctrl.Width = 0) or (Ctrl.Height = 0)) then begin
      if Ctrl.Width = 0 then
        Dimension := 'width'
      else
        Dimension := 'height';
      ALines.Add('ZEROSIZE: ' + Ctrl.Name + ' ' +
        BoundsToStr(Ctrl.Left, Ctrl.Top, Ctrl.Width, Ctrl.Height) +
        ' is visible but has zero ' + Dimension);
    end;
    if Ctrl is TWinControl then
      CheckZeroSize(TWinControl(Ctrl), ALines);
  end;
end;

{$ELSE}

// Dummy implementation when GX_INSPECT_ENABLED is not defined

function ReadPortFile: Integer;
begin
  Result := 0;
end;

constructor TGxInspectServer.Create(APort: Integer);
begin
  inherited Create;
  FPort := 0;
end;

{$ENDIF}

end.
