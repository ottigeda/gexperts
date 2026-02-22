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
    FDeferredExpert: TObject;
    FDeferredAction: Integer;
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
    function FindControlByName(AForm: TCustomForm; const ControlName: string): TControl;
    function CmdExperts: string;
    function CmdExecute(const ExpertName: string): string;
    function CmdConfigure(const ExpertName: string): string;
    function CmdSetBounds(const Args: string): string;
    function CmdClose(const FormId: string): string;
    function CmdClick(const Args: string): string;
    function CmdGetProp(const Args: string): string;
    function CmdSetProp(const Args: string): string;
    function CmdQuit: string;
    function CmdOpenFile(const Args: string): string;
    function CmdShowDesigner: string;
    function CmdDesignScreenshot(const FilePath: string): string;
    function CmdGxConfig: string;
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
  TypInfo, Jpeg, ToolsAPI, GX_GetIdeVersion, GX_GenericUtils,
  GX_GExperts, GX_Experts, GX_EditorExpertManager, GX_EditorExpert,
  GX_OtaUtils, GX_Configure;

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
  WM_GX_INSPECT_DEFERRED = WM_USER + $0F02;
  DA_EXECUTE = 1;
  DA_CONFIGURE = 2;
  DA_QUIT = 3;
  DA_GXCONFIG = 4;
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

function SplitNextArg(const S: string; var Arg, Rest: string): Boolean;
var
  i: Integer;
  Src: string;
begin
  Src := S; // local copy in case S aliases with Arg or Rest
  Result := False;
  Arg := '';
  Rest := '';
  i := 1;
  while (i <= Length(Src)) and (Src[i] = ' ') do
    Inc(i);
  if i > Length(Src) then
    Exit;
  Result := True;
  if Src[i] = '"' then begin
    Inc(i);
    while (i <= Length(Src)) and (Src[i] <> '"') do begin
      Arg := Arg + Src[i];
      Inc(i);
    end;
    if i <= Length(Src) then
      Inc(i); // skip closing quote
  end else begin
    while (i <= Length(Src)) and (Src[i] <> ' ') do begin
      Arg := Arg + Src[i];
      Inc(i);
    end;
  end;
  Rest := Trim(Copy(Src, i, MaxInt));
end;

function ResolvePropertyPath(Obj: TObject; const PropPath: string;
  out FinalObj: TObject; out FinalProp: PPropInfo): Boolean;
var
  DotPos: Integer;
  Head, Tail: string;
  PropInfoPtr: PPropInfo;
begin
  Result := False;
  Tail := PropPath;
  FinalObj := Obj;
  FinalProp := nil;
  while Tail <> '' do begin
    DotPos := Pos('.', Tail);
    if DotPos > 0 then begin
      Head := Copy(Tail, 1, DotPos - 1);
      Tail := Copy(Tail, DotPos + 1, MaxInt);
    end else begin
      Head := Tail;
      Tail := '';
    end;
    PropInfoPtr := GetPropInfo(FinalObj, Head);
    if PropInfoPtr = nil then
      Exit;
    if Tail = '' then begin
      // Last segment — this is the leaf property
      FinalProp := PropInfoPtr;
      Result := True;
    end else begin
      // Intermediate segment — must be an object property
      if PropInfoPtr^.PropType^^.Kind <> tkClass then
        Exit;
      FinalObj := GetObjectProp(FinalObj, PropInfoPtr);
      if FinalObj = nil then
        Exit;
    end;
  end;
end;

function PropValueToStr(Obj: TObject; PropInfo: PPropInfo): string;
var
  TypeInfoPtr: PTypeInfo;
begin
  TypeInfoPtr := PropInfo^.PropType^;
  case TypeInfoPtr^.Kind of
    tkInteger, tkChar, tkWChar:
      Result := IntToStr(GetOrdProp(Obj, PropInfo));
    tkEnumeration:
      Result := GetEnumName(TypeInfoPtr, GetOrdProp(Obj, PropInfo));
    tkFloat:
      Result := FloatToStr(GetFloatProp(Obj, PropInfo));
    tkString, tkLString, tkWString:
      Result := GetStrProp(Obj, PropInfo);
{$IFDEF GX_DELPHI2009_UP}
    tkUString:
      Result := GetStrProp(Obj, PropInfo);
{$ENDIF}
    tkSet:
      Result := GetSetProp(Obj, PropInfo, True);
    tkInt64:
      Result := IntToStr(GetInt64Prop(Obj, PropInfo));
  else
    Result := '(unsupported)';
  end;
end;

procedure SetPropFromStr(Obj: TObject; PropInfo: PPropInfo; const Value: string);
var
  TypeInfoPtr: PTypeInfo;
begin
  TypeInfoPtr := PropInfo^.PropType^;
  case TypeInfoPtr^.Kind of
    tkInteger, tkChar, tkWChar:
      SetOrdProp(Obj, PropInfo, StrToInt(Value));
    tkEnumeration:
      SetOrdProp(Obj, PropInfo, GetEnumValue(TypeInfoPtr, Value));
    tkFloat:
      SetFloatProp(Obj, PropInfo, StrToFloat(Value));
    tkString, tkLString, tkWString:
      SetStrProp(Obj, PropInfo, Value);
{$IFDEF GX_DELPHI2009_UP}
    tkUString:
      SetStrProp(Obj, PropInfo, Value);
{$ENDIF}
    tkSet:
      SetSetProp(Obj, PropInfo, Value);
    tkInt64:
      SetInt64Prop(Obj, PropInfo, StrToInt64(Value));
  else
    raise Exception.Create('Unsupported property type');
  end;
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
  end else if Msg.Msg = WM_GX_INSPECT_DEFERRED then begin
    try
      case FDeferredAction of
        DA_EXECUTE: begin
          if FDeferredExpert is TGX_Expert then
            TGX_Expert(FDeferredExpert).Execute(nil)
          else if FDeferredExpert is TEditorExpert then
            TEditorExpert(FDeferredExpert).Execute(nil);
        end;
        DA_CONFIGURE: begin
          if FDeferredExpert is TGX_Expert then
            TGX_Expert(FDeferredExpert).Configure(nil)
          else if FDeferredExpert is TEditorExpert then
            TEditorExpert(FDeferredExpert).Configure(nil);
        end;
        DA_QUIT: begin
          if Application.MainForm <> nil then
            Application.MainForm.Close
          else
            Application.Terminate;
        end;
        DA_GXCONFIG: begin
          TfmConfiguration.Execute(nil);
        end;
      end;
    finally
      FDeferredAction := 0;
      FDeferredExpert := nil;
    end;
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
  else if Command = 'EXPERTS' then
    Result := CmdExperts
  else if Command = 'EXECUTE' then
    Result := CmdExecute(Args)
  else if Command = 'CONFIGURE' then
    Result := CmdConfigure(Args)
  else if Command = 'SETBOUNDS' then
    Result := CmdSetBounds(Args)
  else if Command = 'CLOSE' then
    Result := CmdClose(Args)
  else if Command = 'CLICK' then
    Result := CmdClick(Args)
  else if Command = 'GETPROP' then
    Result := CmdGetProp(Args)
  else if Command = 'SETPROP' then
    Result := CmdSetProp(Args)
  else if Command = 'QUIT' then
    Result := CmdQuit
  else if Command = 'OPENFILE' then
    Result := CmdOpenFile(Args)
  else if Command = 'SHOWDESIGNER' then
    Result := CmdShowDesigner
  else if Command = 'DESIGNSCREENSHOT' then
    Result := CmdDesignScreenshot(Args)
  else if Command = 'GXCONFIG' then
    Result := CmdGxConfig
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

function TGxInspectServer.FindControlByName(AForm: TCustomForm; const ControlName: string): TControl;
var
  BaseName: string;
  ChildIndex: Integer;
  BracketPos: Integer;

  function SearchChildren(AParent: TWinControl; const AName: string): TControl;
  var
    i: Integer;
    Ctrl: TControl;
  begin
    Result := nil;
    for i := 0 to AParent.ControlCount - 1 do begin
      Ctrl := AParent.Controls[i];
      if SameText(Ctrl.Name, AName) then begin
        Result := Ctrl;
        Exit;
      end;
      if Ctrl is TWinControl then begin
        Result := SearchChildren(TWinControl(Ctrl), AName);
        if Result <> nil then
          Exit;
      end;
    end;
  end;

begin
  if (ControlName = '') or (ControlName = '.') then begin
    Result := AForm;
    Exit;
  end;

  // Support "ControlName[Index]" syntax to access child controls by index.
  // This is useful for unnamed children like TGroupButton inside TRadioGroup.
  BracketPos := Pos('[', ControlName);
  if (BracketPos > 1) and (ControlName[Length(ControlName)] = ']') then begin
    BaseName := Copy(ControlName, 1, BracketPos - 1);
    ChildIndex := StrToIntDef(Copy(ControlName, BracketPos + 1,
      Length(ControlName) - BracketPos - 1), -1);
    if ChildIndex >= 0 then begin
      Result := SearchChildren(AForm, BaseName);
      if (Result <> nil) and (Result is TWinControl) then begin
        if ChildIndex < TWinControl(Result).ControlCount then
          Result := TWinControl(Result).Controls[ChildIndex]
        else
          Result := nil;
      end else
        Result := nil;
      Exit;
    end;
  end;

  Result := SearchChildren(AForm, ControlName);
end;

function TGxInspectServer.CmdExperts: string;
var
  GxInst: TGExperts;
  Lines: TStringList;
  i: Integer;
  Expert: TGX_Expert;
  EdExpert: TEditorExpert;
  ActiveStr: string;
begin
  GxInst := GExpertsInst(True);
  Lines := TStringList.Create;
  try
    for i := 0 to GxInst.ExpertCount - 1 do begin
      Expert := GxInst.ExpertList[i];
      if Expert.Active then
        ActiveStr := 'Active'
      else
        ActiveStr := 'Inactive';
      Lines.Add(Expert.GetName + ' "' + Expert.GetDisplayName + '" ' + ActiveStr);
    end;
    if GxInst.EditorExpertManager <> nil then begin
      for i := 0 to GxInst.EditorExpertManager.EditorExpertCount - 1 do begin
        EdExpert := GxInst.EditorExpertManager.EditorExpertList[i];
        if EdExpert.Active then
          ActiveStr := 'Active'
        else
          ActiveStr := 'Inactive';
        Lines.Add('[Editor] ' + EdExpert.GetName + ' "' + EdExpert.GetDisplayName + '" ' + ActiveStr);
      end;
    end;
    Result := Lines.Text;
  finally
    Lines.Free;
  end;
end;

function TGxInspectServer.CmdExecute(const ExpertName: string): string;
var
  GxInst: TGExperts;
  Expert: TGX_Expert;
  EdIdx: Integer;
  EdExpert: TEditorExpert;
begin
  if ExpertName = '' then begin
    Result := 'ERROR Usage: EXECUTE <expert-name>'#13#10;
    Exit;
  end;
  GxInst := GExpertsInst(True);
  if GxInst.FindExpert(ExpertName, Expert) then begin
    if not Expert.Active then begin
      Result := 'ERROR Expert is inactive: ' + ExpertName + #13#10;
      Exit;
    end;
    FDeferredExpert := Expert;
    FDeferredAction := DA_EXECUTE;
    PostMessage(FHiddenWnd, WM_GX_INSPECT_DEFERRED, 0, 0);
    Result := 'OK ' + Expert.GetName + #13#10;
  end else if (GxInst.EditorExpertManager <> nil) and
              GxInst.EditorExpertManager.FindExpert(ExpertName, EdIdx) then begin
    EdExpert := GxInst.EditorExpertManager.EditorExpertList[EdIdx];
    if not EdExpert.Active then begin
      Result := 'ERROR Expert is inactive: ' + ExpertName + #13#10;
      Exit;
    end;
    FDeferredExpert := EdExpert;
    FDeferredAction := DA_EXECUTE;
    PostMessage(FHiddenWnd, WM_GX_INSPECT_DEFERRED, 0, 0);
    Result := 'OK ' + EdExpert.GetName + #13#10;
  end else
    Result := 'ERROR Expert not found: ' + ExpertName + #13#10;
end;

function TGxInspectServer.CmdConfigure(const ExpertName: string): string;
var
  GxInst: TGExperts;
  Expert: TGX_Expert;
  EdIdx: Integer;
  EdExpert: TEditorExpert;
begin
  if ExpertName = '' then begin
    Result := 'ERROR Usage: CONFIGURE <expert-name>'#13#10;
    Exit;
  end;
  GxInst := GExpertsInst(True);
  if GxInst.FindExpert(ExpertName, Expert) then begin
    if not Expert.HasConfigOptions then begin
      Result := 'ERROR Expert has no configuration: ' + ExpertName + #13#10;
      Exit;
    end;
    FDeferredExpert := Expert;
    FDeferredAction := DA_CONFIGURE;
    PostMessage(FHiddenWnd, WM_GX_INSPECT_DEFERRED, 0, 0);
    Result := 'OK ' + Expert.GetName + #13#10;
  end else if (GxInst.EditorExpertManager <> nil) and
              GxInst.EditorExpertManager.FindExpert(ExpertName, EdIdx) then begin
    EdExpert := GxInst.EditorExpertManager.EditorExpertList[EdIdx];
    if not EdExpert.HasConfigOptions then begin
      Result := 'ERROR Expert has no configuration: ' + ExpertName + #13#10;
      Exit;
    end;
    FDeferredExpert := EdExpert;
    FDeferredAction := DA_CONFIGURE;
    PostMessage(FHiddenWnd, WM_GX_INSPECT_DEFERRED, 0, 0);
    Result := 'OK ' + EdExpert.GetName + #13#10;
  end else
    Result := 'ERROR Expert not found: ' + ExpertName + #13#10;
end;

function TGxInspectServer.CmdSetBounds(const Args: string): string;
var
  Form: TCustomForm;
  FormId, Rest, SL, ST, SW, SH: string;
  L, T, W, H: Integer;
begin
  if not SplitNextArg(Args, FormId, Rest) then begin
    Result := 'ERROR Usage: SETBOUNDS <form-id> <L> <T> <W> <H>'#13#10;
    Exit;
  end;
  if not SplitNextArg(Rest, SL, Rest) or
     not SplitNextArg(Rest, ST, Rest) or
     not SplitNextArg(Rest, SW, Rest) or
     not SplitNextArg(Rest, SH, Rest) then begin
    Result := 'ERROR Usage: SETBOUNDS <form-id> <L> <T> <W> <H>'#13#10;
    Exit;
  end;
  Form := FindForm(FormId);
  if Form = nil then begin
    Result := 'ERROR Form not found: ' + FormId + #13#10;
    Exit;
  end;
  L := StrToInt(SL);
  T := StrToInt(ST);
  W := StrToInt(SW);
  H := StrToInt(SH);
  Form.SetBounds(L, T, W, H);
  Result := 'OK ' + Form.ClassName + ' ' + BoundsToStr(L, T, W, H) + #13#10;
end;

function TGxInspectServer.CmdClose(const FormId: string): string;
var
  Form: TCustomForm;
begin
  if FormId = '' then begin
    Result := 'ERROR Usage: CLOSE <form-id>'#13#10;
    Exit;
  end;
  Form := FindForm(FormId);
  if Form = nil then begin
    Result := 'ERROR Form not found: ' + FormId + #13#10;
    Exit;
  end;
  Result := 'OK ' + Form.ClassName + #13#10;
  Form.Close;
end;

function TGxInspectServer.CmdClick(const Args: string): string;
var
  FormId, ControlName, Rest: string;
  Form: TCustomForm;
  Ctrl: TControl;
begin
  if not SplitNextArg(Args, FormId, Rest) or
     not SplitNextArg(Rest, ControlName, Rest) then begin
    Result := 'ERROR Usage: CLICK <form-id> <control-name>'#13#10;
    Exit;
  end;
  Form := FindForm(FormId);
  if Form = nil then begin
    Result := 'ERROR Form not found: ' + FormId + #13#10;
    Exit;
  end;
  Ctrl := FindControlByName(Form, ControlName);
  if Ctrl = nil then begin
    Result := 'ERROR Control not found: ' + ControlName + #13#10;
    Exit;
  end;
  TControlCracker(Ctrl).Click;
  Result := 'OK ' + ControlName + #13#10;
end;

function TGxInspectServer.CmdGetProp(const Args: string): string;
var
  FormId, ControlName, PropName, Rest: string;
  Form: TCustomForm;
  Ctrl: TControl;
  PropInfoPtr: PPropInfo;
  TargetObj: TObject;
begin
  if not SplitNextArg(Args, FormId, Rest) or
     not SplitNextArg(Rest, ControlName, Rest) or
     not SplitNextArg(Rest, PropName, Rest) then begin
    Result := 'ERROR Usage: GETPROP <form-id> <control-name> <property>'#13#10;
    Exit;
  end;
  Form := FindForm(FormId);
  if Form = nil then begin
    Result := 'ERROR Form not found: ' + FormId + #13#10;
    Exit;
  end;
  Ctrl := FindControlByName(Form, ControlName);
  if Ctrl = nil then begin
    Result := 'ERROR Control not found: ' + ControlName + #13#10;
    Exit;
  end;
  if not ResolvePropertyPath(Ctrl, PropName, TargetObj, PropInfoPtr) then begin
    Result := 'ERROR Property not found: ' + PropName + #13#10;
    Exit;
  end;
  Result := PropName + '=' + PropValueToStr(TargetObj, PropInfoPtr) + #13#10;
end;

function TGxInspectServer.CmdSetProp(const Args: string): string;
var
  FormId, ControlName, PropName, Value, Rest: string;
  Form: TCustomForm;
  Ctrl: TControl;
  PropInfoPtr: PPropInfo;
  TargetObj: TObject;
begin
  if not SplitNextArg(Args, FormId, Rest) or
     not SplitNextArg(Rest, ControlName, Rest) or
     not SplitNextArg(Rest, PropName, Value) then begin
    Result := 'ERROR Usage: SETPROP <form-id> <control-name> <property> <value>'#13#10;
    Exit;
  end;
  Form := FindForm(FormId);
  if Form = nil then begin
    Result := 'ERROR Form not found: ' + FormId + #13#10;
    Exit;
  end;
  Ctrl := FindControlByName(Form, ControlName);
  if Ctrl = nil then begin
    Result := 'ERROR Control not found: ' + ControlName + #13#10;
    Exit;
  end;
  if not ResolvePropertyPath(Ctrl, PropName, TargetObj, PropInfoPtr) then begin
    Result := 'ERROR Property not found: ' + PropName + #13#10;
    Exit;
  end;
  SetPropFromStr(TargetObj, PropInfoPtr, Value);
  Result := 'OK ' + ControlName + '.' + PropName + '=' + Value + #13#10;
end;

function TGxInspectServer.CmdQuit: string;
begin
  FDeferredAction := DA_QUIT;
  PostMessage(FHiddenWnd, WM_GX_INSPECT_DEFERRED, 0, 0);
  Result := 'OK Closing IDE'#13#10;
end;

function TGxInspectServer.CmdGxConfig: string;
begin
  FDeferredAction := DA_GXCONFIG;
  PostMessage(FHiddenWnd, WM_GX_INSPECT_DEFERRED, 0, 0);
  Result := 'OK Opening GExperts configuration'#13#10;
end;

function TGxInspectServer.CmdOpenFile(const Args: string): string;
var
  FilePath: string;
  i: Integer;
begin
  FilePath := Trim(Args);
  if FilePath = '' then begin
    Result := 'ERROR Usage: OPENFILE <filepath>'#13#10;
    Exit;
  end;
  // Strip quotes if present
  if (Length(FilePath) >= 2) and (FilePath[1] = '"') and (FilePath[Length(FilePath)] = '"') then
    FilePath := Copy(FilePath, 2, Length(FilePath) - 2);
  // Normalize forward slashes to backslashes for Windows
  for i := 1 to Length(FilePath) do
    if FilePath[i] = '/' then
      FilePath[i] := '\';
  try
    if GxOtaOpenFile(FilePath) then
      Result := 'OK ' + FilePath + #13#10
    else
      Result := 'ERROR Could not open file: ' + FilePath + #13#10;
  except
    on E: Exception do
      Result := 'ERROR ' + E.Message + #13#10;
  end;
end;

function TGxInspectServer.CmdShowDesigner: string;
var
  FormEditor: IOTAFormEditor;
begin
  if not GxOtaTryGetCurrentFormEditor(FormEditor) then begin
    Result := 'ERROR No form editor active (open a unit with a form first)'#13#10;
    Exit;
  end;
  try
    FormEditor.Show;
    Result := 'OK Designer shown'#13#10;
  except
    on E: Exception do
      Result := 'ERROR ' + E.Message + #13#10;
  end;
end;

function TGxInspectServer.CmdDesignScreenshot(const FilePath: string): string;
var
  DesignForm: TCustomForm;
  Bmp: TBitmap;
  Jpg: TJPEGImage;
  WinDC: HDC;
  Ext: string;
  WndHandle: HWND;
  R: TRect;
  W, H: Integer;
begin
  if FilePath = '' then begin
    Result := 'ERROR Usage: DESIGNSCREENSHOT <filepath>'#13#10;
    Exit;
  end;
  DesignForm := GxOtaGetCurrentDesignForm;
  if DesignForm = nil then begin
    Result := 'ERROR No form currently in design mode'#13#10;
    Exit;
  end;
  // Use the designer container (parent of designed form) if available,
  // otherwise fall back to the designed form itself
  if (DesignForm.Parent <> nil) and (DesignForm.Parent.Handle <> 0) then begin
    WndHandle := DesignForm.Parent.Handle;
    GetWindowRect(WndHandle, R);
    W := R.Right - R.Left;
    H := R.Bottom - R.Top;
  end else begin
    WndHandle := DesignForm.Handle;
    W := DesignForm.Width;
    H := DesignForm.Height;
  end;
  Bmp := TBitmap.Create;
  try
    Bmp.Width := W;
    Bmp.Height := H;
    Bmp.PixelFormat := pf24bit;
    WinDC := GetWindowDC(WndHandle);
    try
      BitBlt(Bmp.Canvas.Handle, 0, 0, W, H, WinDC, 0, 0, SRCCOPY);
    finally
      ReleaseDC(WndHandle, WinDC);
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
      Result := 'OK ' + DesignForm.Name + ' ' + FilePath + #13#10;
    except
      on E: Exception do
        Result := 'ERROR ' + E.Message + #13#10;
    end;
  finally
    Bmp.Free;
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
