unit GX_Logging;

{$I GX_CondDefine.inc}

{$IFOPT D+}
{$DEFINE GX_LOGGING}
{$ENDIF}
{.$DEFINE GX_LOGGING}

interface

uses
  Windows,
  SysUtils,
  Classes,
  Dialogs; // We need "Dialogs" for TMsgDlgType

type
  ///<summary>
  /// Object oriented interface to the GExperts debug window </summary>
  IGxLogger = interface
    procedure SendBoolean(const Identifier: string; const Value: Boolean);
    procedure SendDateTime(const Identifier: string; const Value: TDateTime);
    procedure doSend(const Msg: string; MType: TMsgDlgType);
    procedure Error(const Msg: string);
    procedure ErrorFmt(const Msg: string; const Args: array of const);
    procedure Exception(_e: Exception);
    procedure Warning(const Msg: string);
    procedure WarningFmt(const Msg: string; const Args: array of const);
    procedure Clear;
    procedure SendInteger(const Identifier: string; const Value: Integer);
    ///<summary>
    /// @returns an interface which will automatically add a method exit entry when it goes out of scope </summary>
    function MethodEnter(const MethodName: string): IInterface; overload;
    function MethodEnter(const MethodName: string; const Args: array of const): IInterface; overload;
    procedure Indent;
    procedure UnIndent;
    procedure Separator;
    procedure Info(const Msg: string);
    procedure InfoFmt(const Msg: string; const Args: array of const);
    procedure Pause;
    procedure Resume;
  end;

var
  Logger: IGxLogger = nil;

function CreateModuleLogger(const _Module: string): IGxLogger;

implementation

uses
  Messages,
  Forms,
  Registry,
  Variants,
  u_dzVariantUtils;

type
  ///<summary>
  /// Implements most methods with the exception of actually sending data or starting the GExperts
  /// Debug window </summary>
  TGxBaseLogger = class(TInterfacedObject)
  private
    // this should probably also be a threadvar
    FIsPaused: Boolean;
  protected
    procedure doSend(const Msg: string; MType: TMsgDlgType); virtual; abstract;
    procedure doSendFmt(const Msg: string; const Args: array of const; MType: TMsgDlgType);
    // partial implementation of IGXLogger interface
    procedure SendBoolean(const Identifier: string; const Value: Boolean);
    procedure SendDateTime(const Identifier: string; const Value: TDateTime);
    procedure SendInteger(const Identifier: string; const Value: Integer);
    procedure Info(const Msg: string);
    procedure InfoFmt(const Msg: string; const Args: array of const);
    procedure Error(const Msg: string);
    procedure ErrorFmt(const Msg: string; const Args: array of const);
    procedure Exception(_e: Exception);
    procedure Warning(const Msg: string);
    procedure WarningFmt(const Msg: string; const Args: array of const);
    procedure Clear;
    ///<summary>
    /// @returns an interface which will automatically add a method exit entry when it goes out of scope </summary>
    function MethodEnter(const MethodName: string): IInterface; overload;
    function MethodEnter(const MethodName: string; const Args: array of const): IInterface; overload;
    procedure Indent;
    procedure UnIndent;
    procedure Separator;
    procedure SendFmtEx(const Msg: string; const Args: array of const; MType: TMsgDlgType);
    procedure Pause;
    procedure Resume;
  end;

type
  ///<summary>
  /// prefixes all messages with the module name given in the constructor </summary>
  TGxModuleLogger = class(TGxBaseLogger, IGxLogger)
  private
    FModule: string;
  protected
    procedure doSend(const Msg: string; MType: TMsgDlgType); override;
  public
    constructor Create(const _Module: string);
  end;

type
  TGXMethodExitIntfImplementer = class(TInterfacedObject, IInterface)
  private
    FLogger: TGxBaseLogger;
    FMethodName: string;
  public
    constructor Create(_Logger: TGxBaseLogger; const _MethodName: string);
    destructor Destroy; override;
  end;

// This needs to be a threadvar since otherwise the class is not thread safe.
threadvar
  MsgPrefix: string;

const
  chrStringCommand: AnsiChar = {$IFDEF UNICODE}#4{$ELSE}#1{$ENDIF};
  chrClearCommand: AnsiChar = #3;
  chrNull: AnsiChar = #0;

const
  Indentation = '    ';

procedure TGxBaseLogger.doSendFmt(const Msg: string; const Args: array of const; MType: TMsgDlgType);
begin
  doSend(Format(Msg, Args), MType);
end;

procedure TGxBaseLogger.Clear;
begin
  Info(string(chrClearCommand));
end;

procedure TGxBaseLogger.Info(const Msg: string);
begin
  doSend(Msg, mtInformation);
end;

procedure TGxBaseLogger.SendBoolean(const Identifier: string; const Value: Boolean);
begin
  // Note: We deliberately leave "True" and "False" as
  // hard-coded string constants, since these are
  // technical terminology which should not be localised.
  if Value then
    doSend(Identifier + ' = True', mtInformation)
  else
    doSend(Identifier + ' = False', mtInformation);
end;

procedure TGxBaseLogger.SendDateTime(const Identifier: string; const Value: TDateTime);
begin
  doSend(Identifier + ' = ' + DateTimeToStr(Value), mtInformation);
end;

procedure TGxBaseLogger.Error(const Msg: string);
begin
  doSend(Msg, mtError);
end;

procedure TGxBaseLogger.ErrorFmt(const Msg: string; const Args: array of const);
begin
  doSendFmt(Msg, Args, mtError);
end;

procedure TGxBaseLogger.Exception(_e: Exception);
begin
  Error(_e.ClassName + ': ' + _e.Message);
end;

procedure TGxBaseLogger.InfoFmt(const Msg: string; const Args: array of const);
begin
  doSend(Format(Msg, Args), mtInformation);
end;

procedure TGxBaseLogger.SendFmtEx(const Msg: string; const Args: array of const; MType: TMsgDlgType);
begin
  doSend(Format(Msg, Args), MType);
end;

procedure TGxBaseLogger.Indent;
begin
  MsgPrefix := MsgPrefix + Indentation;
end;

procedure TGxBaseLogger.SendInteger(const Identifier: string; const Value: Integer);
begin
  doSend(Format('%s = %d', [Identifier, Value]), mtInformation);
end;

function TGxBaseLogger.MethodEnter(const MethodName: string): IInterface;
begin
  doSend('Entering ' + MethodName, mtInformation);
  Indent;
  Result := TGXMethodExitIntfImplementer.Create(Self, MethodName);
end;

function TGxBaseLogger.MethodEnter(const MethodName: string; const Args: array of const): IInterface;
var
  s: string;
begin
  s := ArrayOfConst2String(Args);
  if s <> '' then
    s := '(' + s + ')';
  doSend('Entering ' + MethodName + s, mtInformation);
  Indent;
  Result := TGXMethodExitIntfImplementer.Create(Self, MethodName);
end;

constructor TGXMethodExitIntfImplementer.Create(_Logger: TGxBaseLogger; const _MethodName: string);
begin
  inherited Create;
  FMethodName := _MethodName;
  FLogger := _Logger;
end;

destructor TGXMethodExitIntfImplementer.Destroy;
begin
  FLogger.UnIndent;
  FLogger.Info('Leaving ' + FMethodName);
end;

procedure TGxBaseLogger.Pause;
begin
  FIsPaused := True;
end;

procedure TGxBaseLogger.Resume;
begin
  FIsPaused := False;
end;

procedure TGxBaseLogger.Separator;
const
  SeparatorString = '------------------------------';
begin
  doSend(SeparatorString, mtInformation);
end;

procedure TGxBaseLogger.UnIndent;
begin
  Delete(MsgPrefix, 1, Length(Indentation));
end;

procedure TGxBaseLogger.Warning(const Msg: string);
begin
  doSend(Msg, mtWarning);
end;

procedure TGxBaseLogger.WarningFmt(const Msg: string; const Args: array of const);
begin
  doSendFmt(Msg, Args, mtWarning);
end;

type
  ///<summary>
  /// This actually sends data to the GExperts debug window </summary>
  TGxActualLogger = class(TGxBaseLogger, IGxLogger)
  private
    FPastFailedAttemptToStartDebugWin: Boolean;
    function StartWin: hWnd;
  protected
    procedure doSend(const Msg: string; MType: TMsgDlgType); override;
  end;

{ TGxActualLogger }

procedure TGxActualLogger.doSend(const Msg: string; MType: TMsgDlgType);
var
  CDS: TCopyDataStruct;
  DebugWin: hWnd;
  MessageString: string;
  MsgBytes: array of Byte;
  MsgType: AnsiChar;
  ByteIndex: Integer;
{$IFDEF GX_DEBUGLOG}
{$DEFINE NEEDMTYPESTR}
{$ENDIF GX_DEBUGLOG}

  procedure AddByte(B: Byte);
  begin
    MsgBytes[ByteIndex] := B;
    Inc(ByteIndex);
  end;

{$IFNDEF UNICODE}
  procedure AddStringBytes(const Str: AnsiString); overload;
  var
    i: Integer;
  begin
    for i := 1 to Length(Str) do
      AddByte(Byte(Str[i]));
  end;

{$ELSE}
  procedure AddStringBytes(const Str: string); overload;
  var
    i: Integer;
    c: WideChar;
  begin
    for i := 1 to Length(Str) do begin
      c := Str[i];
      AddByte(Word(c) and $FF);
      AddByte(Word(c) shr 8);
    end;
  end;
{$ENDIF}

{$IFDEF NEEDMTYPESTR}
const
  MTypeStr: array[TMsgDlgType] of string =
    ('Warning: ', 'Error: ', 'Information: ', 'Confirmation: ', 'Custom: ');
{$ENDIF NEEDMTYPESTR}
begin
  if FIsPaused then
    Exit;

{$IFDEF GX_DEBUGLOG}
  GxAddToDebugLog(MTypeStr[MType] + Msg);
{$ENDIF GX_DEBUGLOG}
{$IFDEF MSWINDOWS}
  DebugWin := FindWindow('TfmDebug', nil);

  if DebugWin = 0 then
    DebugWin := StartWin;

  if DebugWin <> 0 then begin
    ByteIndex := 0;
    MessageString := MsgPrefix + Msg;
    SetLength(MsgBytes, 1 + 1 + (Length(MessageString) * SizeOf(Char)) + 1); // Payload, type, message, null
    CDS.cbData := Length(MsgBytes);
    CDS.dwData := 0;
    MsgType := AnsiChar(Ord(MType) + 1);
    if Msg = string(chrClearCommand) then
      AddByte(Byte(chrClearCommand))
    else
      AddByte(Byte(chrStringCommand));
    AddByte(Byte(MsgType));
    AddStringBytes(MessageString);
    AddByte(Byte(chrNull));
    CDS.lpData := Pointer(MsgBytes);
    SendMessage(DebugWin, WM_COPYDATA, WPARAM(Application.Handle), LPARAM(@CDS));
  end;
{$ENDIF MSWINDOWS}
end;

function TGxActualLogger.StartWin: hWnd;
var
  DebugFileName: string;
  Buf: array[0..MAX_PATH + 1] of Char;
  si: TStartupInfo;
  pi: TProcessInformation;
begin
  MsgPrefix := '';

  Result := 0;
  if FPastFailedAttemptToStartDebugWin then
    Exit;

  with TRegIniFile.Create('\Software\GExperts') do // Do not localize.
    try
      DebugFileName := ReadString('Debug', 'FilePath', ''); // Do not localize.
    finally
      Free;
    end;

  if Trim(DebugFileName) = '' then begin
    GetModuleFileName(HINSTANCE, Buf, SizeOf(Buf) - 1);
    DebugFileName := ExtractFilePath(StrPas(Buf)) + 'GExpertsDebugWindow.exe'; // Do not localize.
  end;

  if (Trim(DebugFileName) = '') or not FileExists(DebugFileName) then begin
    FPastFailedAttemptToStartDebugWin := True;
    Exit;
  end;

  FillChar(si, SizeOf(si), #0);
  si.cb := SizeOf(si);
  si.dwFlags := STARTF_USESHOWWINDOW;
  si.wShowWindow := SW_SHOW;
  if not CreateProcess(PChar(DebugFileName), nil, nil, nil,
    False, 0, nil, nil, si, pi) then begin
    FPastFailedAttemptToStartDebugWin := True;
    Exit;
  end;

  try
    WaitForInputIdle(pi.hProcess, 3 * 1000); // wait for 3 seconds to get idle
  finally
    CloseHandle(pi.hThread);
    CloseHandle(pi.hProcess);
  end;

  Result := FindWindow('TfmDebug', nil);
end;

type
  ///<summary>
  /// This implementation does nothing, it's here only to make the code more readable elsewere
  /// by removing the need for {$IFOPT D+} ... {$ENDIF} there. </summary>
  TGxNullLogger = class(TGxBaseLogger, IGxLogger)
  protected
    procedure doSend(const Msg: string; MType: TMsgDlgType); override;
  end;

{ TGxNullLogger }

procedure TGxNullLogger.doSend(const Msg: string; MType: TMsgDlgType);
begin
  // do nothing
end;

{ TGxModuleLogger }

constructor TGxModuleLogger.Create(const _Module: string);
begin
  inherited Create;
  FModule := _Module;
end;

procedure TGxModuleLogger.doSend(const Msg: string; MType: TMsgDlgType);
begin
  Logger.doSend(FModule + ': ' + Msg, MType);
end;

function CreateModuleLogger(const _Module: string): IGxLogger;
begin
{$IFDEF GX_LOGGING}
  Result := TGxModuleLogger.Create(_Module);
{$ELSE}
  // if logging is turned off, return the global TGxNullLogger instance
  Result := Logger;
{$ENDIF}
end;

initialization

{$IFDEF GX_LOGGING}
  Logger := TGxActualLogger.Create;
{$ELSE}
  Logger := TGxNullLogger.Create;
{$ENDIF}
finalization
  Logger := nil;
end.
