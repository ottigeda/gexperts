unit GX_FilterExceptionsNotification;

{$I GX_CondDefine.inc}

{$IFNDEF GX_DELPHI2005_UP}
// Delphi 6 and 7 don't have the packages we hook here, the debugger is probably part of the Delphi32.exe,
'This only works for Delphi 2005 and newer'
{$ENDIF}

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  ActnList,
  Actions,
  Forms,
  Dialogs,
  StdCtrls,
  GX_BaseForm,
  GX_FilterExceptionsEdit;

type
  TOnCheckExceptionEx = procedure(_Sender: TObject; const _Project, _Exception, _Message: string;
    var _Action: TExceptionFilterAction) of object;

procedure InstallHook(_OnCheckException, _OnFilterButtonClick: TOnCheckExceptionEx);
procedure UninstallHook();

type
  TfmExceptionNotification = class(TfmBaseForm)
    l_Message: TLabel;
    b_Break: TButton;
    b_Continue: TButton;
    b_Ignore: TButton;
    TheActionList: TActionList;
    act_Filter: TAction;
    act_CopyToClipboard: TAction;
    b_AllThisSession: TButton;
    act_IgnoreAll: TAction;
    procedure act_IgnoreAllExecute(Sender: TObject);
    procedure act_FilterExecute(Sender: TObject);
    procedure act_CopyToClipboardExecute(Sender: TObject);
  private
    FProject: string;
    FException: string;
    FMessage: string;
    FOnAddException: TOnCheckExceptionEx;
    procedure SetData(_OnAddException: TOnCheckExceptionEx;
      const _Project, _Exception, _Message: string);
  public
    class function Execute(_Owner: TWinControl; _OnAddException: TOnCheckExceptionEx;
      const _Project, _Exception, _Message: string): Boolean;
    constructor Create(_Owner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  Clipbrd,
  SyncObjs,
  ToolsApi,
  u_dzVclUtils,
  u_dzCriticalSection,
  DDetours,
  LegacyTypes,
{$IFOPT D+}GX_DbugIntf,
{$ENDIF}
  GX_VerDepConst,
  GX_OtaUtils,
  GX_GenericUtils;

{ TfmExceptionNotification }

class function TfmExceptionNotification.Execute(_Owner: TWinControl; _OnAddException: TOnCheckExceptionEx;
  const _Project, _Exception, _Message: string): Boolean;
var
  frm: TfmExceptionNotification;
begin
  frm := TfmExceptionNotification.Create(_Owner);
  try
    TForm_CenterOn(frm, _Owner);
    frm.SetData(_OnAddException, _Project, _Exception, _Message);
    Result := (frm.ShowModal = mrok);
  finally
    FreeAndNil(frm);
  end;
end;

constructor TfmExceptionNotification.Create(_Owner: TComponent);
begin
  inherited Create(_Owner);
  TControl_SetMinConstraints(Self);
end;

procedure TfmExceptionNotification.SetData(_OnAddException: TOnCheckExceptionEx;
  const _Project, _Exception, _Message: string);
begin
  FOnAddException := _OnAddException;
  FProject := _Project;
  FException := _Exception;
  FMessage := _Message;
  l_Message.Caption := Format('Project %s.exe raised exception class %s with message ''%s''.',
    [_Project, _Exception, _Message]);
  b_Ignore.Visible := Assigned(FOnAddException);
end;

procedure TfmExceptionNotification.act_CopyToClipboardExecute(Sender: TObject);
begin
  Clipboard.AsText := '---------------------------'#13#10
    + Caption + #13#10
    + '---------------------------'#13#10
    + l_Message.Caption + #13#10
    + '---------------------------'#13#10
    + '[' + b_Ignore.Caption + '] [' + b_AllThisSession.Caption + '] [' + b_Break.Caption
    + '] [' + b_Continue.Caption + ']'#13#10
    + '---------------------------';
end;

procedure TfmExceptionNotification.act_FilterExecute(Sender: TObject);
var
  Action: TExceptionFilterAction;
begin
  if Assigned(FOnAddException) then begin
    Action := efaIgnore;
    FOnAddException(Self, FProject, FException, FMessage, Action);
    case Action of
      efaIgnore: ModalResult := mrIgnore;
      efaBreak: ModalResult := mrok;
    end;
  end;
end;

procedure TfmExceptionNotification.act_IgnoreAllExecute(Sender: TObject);
var
  Action: TExceptionFilterAction;
begin
  if Assigned(FOnAddException) then begin
    Action := efaIgnore;
    FOnAddException(Self, '', '', '', Action);
    ModalResult := mrIgnore;
  end;
end;

// The following code ist mostly from Mahdi Safsafi
// I'm not sure I have really understood it all, so probably any bugs were introduced by myself.
// -- 2020-07-26 twm
// NOTE: It hooks into BPLs that come with Delphi and may need to be adapted whenever these BPLs change,
// that is not only for new Delphi versions but also when Embarcadero releases an update or patch.

// Older delphi versions that support only Win32 must define IS_WIN32_ONLY, because they must use
// the WaitForDebugEvent method. This method is simple/documented but it only works with Win32.
{$IFNDEF GX_DELPHIXE2_UP}
{$DEFINE IS_WIN32_ONLY}
{$ENDIF}

type
  TDebugger = class(TObject)
  end;
  TThread = class(TObject)
  end;
  TProcess = class(TObject)
  end;
{$IF not declared(TOTAAddress)}
  TOTAAddress = UInt64;
{$IFEND}
  TAddress = TOTAAddress;
  TVmtOffset = (voParent, voClassName);

  TArrayOfShortInt = array[Byte] of ShortInt;
  PArrayOfShortInt = ^TArrayOfShortInt;

type
  TDoShowException = function(Debugger: TDebugger): Boolean;
  TGetExceptionMessage = procedure(Debugger: TDebugger; var Msg: string);
  TPostDebugMessage = procedure(Debugger: TDebugger; Msg: ShortInt; Process: TProcess);
  TGetExceptionName = procedure(Debugger: TDebugger; var Msg: string);
  TGetExceptionDisplayName = procedure(Debugger: TDebugger; var Msg: string);
  TFindExceptionAddress = function(Debugger: TDebugger): Integer;

  TGetExceptionClass = function(Thread: TThread): TAddress;
  TReadInteger = function(Thread: TThread; Undoc: Pointer; Address: TAddress): Int64;
  TGetExceptionAddress = procedure(Thread: TThread; var Address);

  TThreadOsInfoArray = array[0..512] of Byte;
  TParsedThreadOsInfoArray = array[0..512] of Byte;

  TGetThreadOsInfo = function(Thread: TThread; var Buffer: TThreadOsInfoArray): Integer;
  TParseThreadOsInfo = function(Thread: TThread; var SrcBuffer: TThreadOsInfoArray; var OutBuffer: TParsedThreadOsInfoArray): Integer;

  // redeclare WaitForDebugEvent
  // (WinApi.Windows declares it as:
  // function WaitForDebugEvent(var lpDebugEvent: TDebugEvent; dwMilliseconds: DWORD): ByteBool; stdcall;
  // but we need to allow for lpDebugEvent to be NIL
  TWaitForDebugEvent = function(lpDebugEvent: PDebugEvent; dwMilliseconds: DWORD): ByteBool; stdcall;

var
  // requires synchronization because it is set in InterceptWaitForDebugEvent by a different
  // thread than the one that uses it in --> use TCriticalSection
  FDebugEvent: TDebugEvent;
  FDebugEventCritSect: TdzCriticalSection;

  OnCheckException: TOnCheckExceptionEx = nil;
  OnIgnoreButtonClick: TOnCheckExceptionEx = nil;

  WaitForDebugEvent: TWaitForDebugEvent = nil;
  ParseThreadOsInfo: TParseThreadOsInfo = nil;
  GetThreadOsInfo: TGetThreadOsInfo = nil;

  FindExceptionAddress: TFindExceptionAddress = nil;
  GetExceptionClass: TGetExceptionClass = nil;
  GetExceptionMessage: TGetExceptionMessage = nil;
  GetExceptionDisplayNameProc: TGetExceptionDisplayName = nil;
  GetExceptionName: TGetExceptionName = nil;
  PostDebugMessage: TPostDebugMessage = nil;
  GetExceptionAddress: TGetExceptionAddress = nil;

  Hooked: Boolean = False;

function TryGetExceptionDisplayName(Debugger: TDebugger; out _DisplayName: string): Boolean;
begin
  Result := Assigned(GetExceptionDisplayNameProc);
  if Result then
    GetExceptionDisplayNameProc(Debugger, _DisplayName)
end;

const
  // disasm @Debug@TDebugger@GetExceptionName$qqrv function to get the offsets for
  // other Delphi versions:
  // ; ebx = Debugger.
  // mov     eax, [ebx+40h]  ;   40h = offset for Process.
  // mov     eax, [eax+50h]  ;   50h = offset for Thread.
  // call    @Debug@TThread@GetExceptionName$qqrv
{$IFDEF GX_DELPHI2007_UP}
  ProcessOffset = $40; // verified for Delphi 2007 to 10.4
{$ELSE}
  ProcessOffset = $3C; // verified for Delphi 2005 and 2006
{$ENDIF}

  // ThreadOffset = $50 verified for Delphi 2005 to 10.4
  ThreadOffset = $50;

function GetPocessFromDebugger(Debugger: TDebugger): TProcess;
asm
  mov eax, [eax + ProcessOffset]
end;

function GetThreadFromProcess(Process: TProcess): TThread;
asm
  mov eax, [eax + ThreadOffset]
end;

var
  TrampolineDoShowException: TDoShowException = nil;
  TrampolineWaitForDebugEvent: TWaitForDebugEvent = nil;

function InterceptWaitForDebugEvent(lpDebugEvent: PDebugEvent; dwMilliseconds: DWORD): ByteBool; stdcall;
begin
  // WaitForDebugEvent  will only work for win32 process
  // For win64, ... you need to use different way => see GetExceptionObjectNew.

  //Trampoline must be always called. Otherwise => unpredictable behaviour.
  Result := TrampolineWaitForDebugEvent(lpDebugEvent, dwMilliseconds);

  if Assigned(lpDebugEvent) then begin
    try
      if lpDebugEvent^.dwDebugEventCode = EXCEPTION_DEBUG_EVENT then begin
        // When a system fires a debug event, it blocks all threads of the process being debugged,
        // and only a call to ContinueDebugEvent resumes these suspended threads.
        // However, DoShowException is called from another Delphi IDE thread than this code
        // (not the thread that is debugging) meaning, you need to sync access using
        // monitor/critical_section.
        // Moreover, implementing a stack of TDebugEvent in a way you push here and you pop
        // DoShowException so you don't miss any event.
        FDebugEventCritSect.Enter;
        FDebugEvent := lpDebugEvent^;
        FDebugEventCritSect.Leave;
      end;
    except
      Exit; //==>
    end;
  end;
end;

function GetVmtOffsetWin32(Offset: TVmtOffset): Int64;
begin
  // vmtX for win32.
  case Offset of
    voParent:
      Result := vmtParent;
    voClassName:
      Result := vmtClassName;
  else
    raise Exception.Create('Unkown offset');
  end;
end;

function GetVmtOffsetWin64(Offset: TVmtOffset): Int64;
begin
  // vmtX for win64.
  case Offset of
    voParent:
      Result := -120;
    voClassName:
      Result := -136;
  else
    raise Exception.Create('Unkown offset');
  end;
end;

function GetVmtOffset(Process: IOTAProcess; Offset: TVmtOffset): Int64;
begin
{$IFDEF IS_WIN32_ONLY}
  Result := GetVmtOffsetWin32(Offset);
{$ELSE}
  { Each platform should have corresponding constant value. }
  case Process.GetProcessType of
    optWin32:
      Result := GetVmtOffsetWin32(Offset);
    optWin64:
      Result := GetVmtOffsetWin64(Offset);
    // implement others...
  else
    raise Exception.Create('Please implement me.');
  end;
{$ENDIF}
end;

function ReadPointer(Process: IOTAProcess; Address: TAddress): TAddress;
begin
  try
    Result := 0;
{$IFDEF IS_WIN32_ONLY}
    Process.ReadProcessMemory(Address, SizeOf(Integer), Result);
{$ELSE}
    case Process.GetProcessType of
{$IF declared(optiOS32)}
      optiOS32,
{$IFEND}
{$IF declared(optAndroid)}
      optAndroid,
{$IFEND}
{$IF declared(optOSX32)}
      optOSX32,
{$IFEND}
      optWin32:
        Process.ReadProcessMemory(Address, SizeOf(Integer), Result);
{$IF declared(optOSX64)}
      optOSX64,
{$IFEND}
{$IF declared(optLinux64)}
      optLinux64,
{$IFEND}
{$IF declared(optiOS64)}
      optiOS64,
{$IFEND}
{$IF declared(optAndroid64)}
      optAndroid64,
{$IFEND}
      optWin64:
        Process.ReadProcessMemory(Address, SizeOf(Int64), Result);
    else
      raise Exception.Create('Please implement me.');
    end;
{$ENDIF}
  except
{$IFOPT D+}
    on e: Exception do
      SendDebugError(e.Message);
{$ENDIF}
  end;
end;

function ReadClassParent(Process: IOTAProcess; AClass: TAddress): TAddress;
begin
  { return parent class. }
  Result := ReadPointer(Process, TAddress(Int64(AClass) + GetVmtOffset(Process, voParent)));
  if 0 <> Result then
    Result := ReadPointer(Process, Result);
end;

function ReadClassName(Process: IOTAProcess; AClass: TAddress): string;
var
  P: TAddress;
  U: ShortString;
begin
  { return class name. }
  P := ReadPointer(Process, TAddress(Int64(AClass) + GetVmtOffset(Process, voClassName)));
  Process.ReadProcessMemory(P, SizeOf(U), U);
  Result := UTF8ToUnicodeString(U)
end;

function GetClasses(Process: IOTAProcess; AClass: TAddress): string;
var
  LClass: TAddress;
  s: string;
  sl: TStringList;
begin
  { get a list of all descendant class. }
  LClass := AClass;
  sl := TStringList.Create();
  try
    sl.Delimiter := ',';
    while 0 <> LClass do begin
      s := ReadClassName(Process, LClass);
      LClass := ReadClassParent(Process, LClass);
      if s <> '' then
        sl.Add(s);
    end;
  finally
    Result := sl.DelimitedText;
    sl.Free();
  end;
end;

function GetCurrentExceptionAddress(Debugger: TDebugger; Process: TProcess; Thread: TThread;
  out _AddressHex: string): TAddress; overload;
type
  {
    this is undocumented struct. You should not use it.
    use OTAThreadContextEx instead.
  }
  TAddressForGetExceptionAddress = record
    Code: Integer;

    AddressRec: packed record

      case Integer of
        1:
        (Address: Pointer
          );
        3:
        (Address64: UInt64
          );
    end;

    Unkown: Integer;
  end;
var
  LAddressRec: TAddressForGetExceptionAddress;
  IThread: IOTAThread;
  IProcess: IOTAProcess;
  SizeOfResult: Integer;
begin
  Supports(Thread, IOTAThread, IThread);
  Supports(Process, IOTAProcess, IProcess);

{$IFDEF IS_WIN32_ONLY}
  // older delphi versions use the old OTAThreadContext struct
  // we could use FDebugEvent.Exception.ExceptionRecord.ExceptionAddress
  //  Result := DWord(FDebugEvent.Exception.ExceptionRecord.ExceptionAddress);
  // But for consistency we use the Eip here too.
  Result := IThread.OTAThreadContext.Eip;
  SizeOfResult := SizeOf(IThread.OTAThreadContext.Eip);
{$ELSE}
  SizeOfResult := 0;
  // thread is suspended so we can get current address from instruction pointer register
  case IProcess.GetProcessType of
    optWin32: begin // x86
        Result := IThread.OTAThreadContextEx.win32.Eip;
        SizeOfResult := SizeOf(IThread.OTAThreadContextEx.win32.Eip);
      end;
    optWin64: begin // x64
        Result := IThread.OTAThreadContextEx.win64.Rip;
        SizeOfResult := SizeOf(IThread.OTAThreadContextEx.win64.Rip);
      end;
{$IF declared(optOSX32)}
    optOSX32: begin // aarch32
{$IFDEF GX_DELPHIXE3_UP}
        Result := IThread.OTAThreadContextEx.arm32.Pc;
        SizeOfResult := SizeOf(IThread.OTAThreadContextEx.arm32.Pc);
{$ELSE}
        Result := IThread.OTAThreadContextEx.osx32.Eip;
        SizeOfResult := SizeOf(IThread.OTAThreadContextEx.osx32.Eip);
{$ENDIF}
      end;
{$IFEND}
{$IF declared(optOSX64)}
    optOSX64: begin // aarch64
        Result := IThread.OTAThreadContextEx.arm64.Pc;
        SizeOfResult := SizeOf(IThread.OTAThreadContextEx.arm64.Pc);
      end;
{$IFEND}
  // todo: Support other platforms
  else
    Result := 0;
  end;
{$ENDIF}

  if 0 <> Result then begin
    if SizeOfResult <> 0 then
      _AddressHex := IntToHex(Result, SizeOfResult * 2);
    Exit; //==>
  end;

  // using undocumented GetExceptionAddress function
  if FindExceptionAddress(Debugger) = 1 then begin
    FillChar(LAddressRec, SizeOf(LAddressRec), #00);
    GetExceptionAddress(Thread, LAddressRec);
    if LAddressRec.Code = 1 then begin
      Result := UInt64(LAddressRec.AddressRec.Address);
      SizeOfResult := SizeOf(LAddressRec.AddressRec.Address);
    end else begin
      Result := LAddressRec.AddressRec.Address64;
      SizeOfResult := SizeOf(LAddressRec.AddressRec.Address64);
    end;
    _AddressHex := IntToHex(Result, SizeOfResult * 2);
  end;
end;

function GetCurrentExceptionAddress(Debugger: TDebugger; Process: TProcess; Thread: TThread): TAddress; overload;
var
  AddressHex: string;
begin
  Result := GetCurrentExceptionAddress(Debugger, Process, Thread, AddressHex);
end;

type
  // These are UInt64 and not NativeUInt as RTL !
  TExceptionInformation = array[0..EXCEPTION_MAXIMUM_PARAMETERS - 1] of UInt64;

{$IFNDEF IS_WIN32_ONLY}

function GetExceptionObjectNew(Thread: TThread; out _ExceptionInformation: TExceptionInformation): TAddress;
type
  PExceptionInformation = ^TExceptionInformation;
var
  I: Integer;
  Src: TThreadOsInfoArray;
  Parsed: TParsedThreadOsInfoArray;
  P: PByte;
  C: Cardinal;
  PE: PExceptionRecord;
  Params: PExceptionInformation;
begin
  // this function should be used only with new delphi versions
  Result := 0;
  ZeroMemory(@Src, SizeOf(Src));
  ZeroMemory(@Parsed, SizeOf(Parsed));
  I := GetThreadOsInfo(Thread, Src);
  if I <> 0 then begin
    case I of
      4, 6, 8, 7, 9, 10:
        Exit; // ==>
    end;
    ParseThreadOsInfo(Thread, Src, Parsed);

    // disasm TNativeThread::DoGetExceptionName
    P := @Parsed[0];
    Inc(P, $A8);
    P := PPointer(P)^;
    PE := PExceptionRecord(Integer(P) + $18);
    C := PE.ExceptionCode;
    Params := Pointer(Integer(P) + $30);
    _ExceptionInformation := Params^;
{$IFOPT D+}
    for I := Low(_ExceptionInformation) to High(_ExceptionInformation) do
      SendDebugWarning('_ExceptionInformation[' + IntToStr(I) + '] = ' + IntToHex(_ExceptionInformation[I], SizeOf(_ExceptionInformation[I]) * 2));
{$ENDIF}

    { !!! don't optimize me !!! }

    if (C <> $0EEDFAE6 { cCppBuilderException }) then begin
      if C = $0EEDFADE { cDelphiException } then begin
        Inc(P, $38);
        Result := PUInt64(P)^;
      end else if C <> $0EEDFAE4 { cNonDelphiException } then begin
        Exit; // ==>
      end else begin
        Inc(P, $38);
        Result := PUInt64(P)^;
      end;
    end else begin
      C := PCardinal(Integer(P) + $34)^;
      if C <> 0 then begin
        Inc(P, $48);
        Result := PUInt64(P)^;
      end else begin
        C := PCardinal(Integer(P) + $30)^;
        if C <> 1 then begin
          Inc(P, $48);
          Result := PUInt64(P)^;
        end else begin
          Exit;
        end;
      end;
    end;
  end;
end;
{$ENDIF}

function GetExceptionObjectLegacy(Thread: TThread; out _ExceptionInformation: TExceptionInformation): TAddress;
var
  I: Integer;
begin
  Result := 0;
  // This function should only be used with old Delphi versions, where GetExceptionObjectNew does
  // not work, that is, ParseThreadOsInfo does not exist.
  FDebugEventCritSect.Enter;
  if FDebugEvent.Exception.ExceptionRecord.NumberParameters > 1 then begin
    // Param[1] = Exception object.
    // FDebugEvent.dwProcessId = process id.
    // FDebugEvent.dwThreadId = dwThreadId id.
    // FDebugEvent.Exception.ExceptionRecord.ExceptionAddress = exception address.
    // see  TExceptionRecord for more info.
    if FDebugEvent.Exception.ExceptionRecord.ExceptionCode = $0EEDFADE { cDelphiException } then
      Result := FDebugEvent.Exception.ExceptionRecord.ExceptionInformation[1];
    for I := 0 to FDebugEvent.Exception.ExceptionRecord.NumberParameters - 1 do
      _ExceptionInformation[I] := FDebugEvent.Exception.ExceptionRecord.ExceptionInformation[I];
  end;
  FDebugEventCritSect.Leave;
end;

function GetExceptionObject(Thread: TThread; out _ExceptionInformation: TExceptionInformation): TAddress;
begin
{$IFNDEF IS_WIN32_ONLY}
  if Assigned(ParseThreadOsInfo) then
    Result := GetExceptionObjectNew(Thread, _ExceptionInformation)
  else
{$ENDIF}begin
    // Win32-Delphi-Version or ParseThreadOsInfo does not exist
    Result := GetExceptionObjectLegacy(Thread, _ExceptionInformation);
  end;
end;

function DoShowExceptionHooked(Debugger: TDebugger): Boolean;
const
{$IFDEF GX_DELPHIXE2_UP}
  ParamOffset = $A1;
{$ELSE}
  ParamOffset = $99;
{$ENDIF}
var
  Msg: string;
  P: PByte;
  Action: TExceptionFilterAction;
  Process: TProcess;
  Thread: TThread;
  IThread: IOTAThread;
  IProcess: IOTAProcess;

  ExceptionName: string;
  Projectname: string;
  sl: TStringList;
  s: string;
  ExceptionAddress: TAddress;
  ExceptionObject: TAddress;
{$IFDEF GX_DELPHI2006_UP}
  LineNo: Integer;
{$ENDIF}
  LClass: TAddress;
  AccessOp: string;
  AccessAddress: UInt64;
  ExceptionInformation: TExceptionInformation;
begin
  Process := GetPocessFromDebugger(Debugger);

  Thread := GetThreadFromProcess(Process);

  { get official iota interfaces: }
  Assert(Supports(Thread, IOTAThread, IThread));
  Assert(Supports(Process, IOTAProcess, IProcess));

  // these is the information currently used for filtering
  GetExceptionMessage(Debugger, Msg);
  GetExceptionName(Debugger, ExceptionName);
  ExceptionAddress := GetCurrentExceptionAddress(Debugger, Process, Thread, s);
  ExceptionObject := GetExceptionObject(Thread, ExceptionInformation);
  if SameText(ExceptionName, 'ERangeError') then begin
    // The RTL does not add the exception address for range check errors to the message, so we
    // do it here in order to allow filtering for it
    // Unfortunately there seems to be no way to get the same info for other exception types,
    Msg := Msg + ' at address ' + IntToHex(ExceptionInformation[0], Length(s));
  end else if SameText(ExceptionName, '$C0000005' { STATUS_ACCESS_VIOLATION }) then begin
    ExceptionName := 'EAccessViolation';
    Msg := 'Access Violation at address ' + s;

    case ExceptionInformation[0] of
      0:
        AccessOp := 'Read';
      1:
        AccessOp := 'Write';
      8:
        AccessOp := 'Execution';
    else
      AccessOp := 'Invalid access';
    end;
    AccessAddress := ExceptionInformation[1];
    Msg := Format('Access violation at address %s. %s of address ',
      [s, AccessOp]) + IntToHex(AccessAddress, Length(s));
  end else if SameText(ExceptionName, '$C0000090' { STATUS_FLOAT_INVALID_OPERATION})
    or SameText(ExceptionName, '$C000008F' { STATUS_FLOAT_INEXACT_RESULT })
    or SameText(ExceptionName, '$C0000092' { STATUS_FLOAT_STACK_CHECK }) then begin
    ExceptionName := 'EInvalidOp';
    Msg := 'Invalid floating point operation at address ' + s;
  end else if SameText(ExceptionName, '$C0000094' { STATUS_INTEGER_DIVIDE_BY_ZERO }) then begin
    ExceptionName := 'EDivByZero';
    Msg := 'Division by zero at address ' + s;
  end else if SameText(ExceptionName, '$C000008C' { STATUS_ARRAY_BOUNDS_EXCEEDED }) then begin
    ExceptionName := 'ERangeError';
    Msg := 'Range check error at address ' + s;
  end else if SameText(ExceptionName, '$C0000095' { STATUS_INTEGER_OVERFLOW }) then begin
    ExceptionName := 'EIntOverflow';
    Msg := 'Integer overflow at address ' + s;
  end else if SameText(ExceptionName, '$C000008E' { STATUS_FLOAT_DIVIDE_BY_ZERO }) then begin
    ExceptionName := 'EZeroDivide';
    Msg := 'Floating point division by zero at address ' + s;
  end else if SameText(ExceptionName, '$C0000091' { STATUS_FLOAT_OVERFLOW }) then begin
    ExceptionName := 'EOverflow';
    Msg := 'Floating point overflow at address ' + s;
  end else if SameText(ExceptionName, '$C0000093' { STATUS_FLOAT_UNDERFLOW })
    or SameText(ExceptionName, '$C000008D' { STATUS_FLOAT_DENORMAL_OPERAND }) then begin
    ExceptionName := 'EUnderflow';
    Msg := 'Floating point underflow at address ' + s;
  end else if SameText(ExceptionName, '$C0000096' { STATUS_PRIVILEGED_INSTRUCTION }) then begin
    ExceptionName := 'EPrivInstruction';
    Msg := 'Privileged instruction at address ' + s;
  end else if SameText(ExceptionName, '$C000013A' { STATUS_CONTROL_C_EXIT }) then begin
    ExceptionName := 'EControlBreak';
    Msg := 'Control-C hit at address ' + s;
  end else if SameText(ExceptionName, '$C00000FD' { STATUS_STACK_OVERFLOW }) then begin
    ExceptionName := 'EStackOverflow';
    Msg := 'Stack overflow at address ' + s;
  end;
//      Result := System.reExternalException;

  Projectname := GxOtaGetCurrentProjectName;

  // For now we only fill the string list with the additional information,
  // but this information will also be used for filtering later.
  sl := TStringList.Create();
  try
    s := '';

    sl.Add(Format('ThreadId=%d', [IThread.OSThreadID]));
    sl.Add(Format('ProcessId=%d', [IProcess.ProcessId]));
{$IFDEF GX_DELPHI2010_UP}
    sl.Add(Format('ThreadName="%s"', [IThread.ThreadName]));
{$ELSE}
    sl.Add('ThreadName=<not available>');
{$ENDIF}

    sl.Add(Format('ExceptionMessage="%s"', [Msg]));

    sl.Add(Format('ExceptionName="%s"', [ExceptionName]));

    if TryGetExceptionDisplayName(Debugger, s) then
      sl.Add(Format('ExceptionDisplayName="%s"', [s]))
    else
      sl.Add('ExceptionDisplayName=<not available>');

    if ExceptionAddress <> 0 then begin
      sl.Add(Format('ExceptionAddress=%s', [IntToHex(ExceptionAddress, 8)]));

{$IFDEF GX_DELPHI2006_UP}
      // I have yet to see this actually working
      // -- 2020-07-26 twm
      IProcess.SourceLocationFromAddress(ExceptionAddress, s, LineNo);
      if s <> '' then begin
        sl.Add(Format('FileName="%s"', [s]));
        sl.Add(Format('LineNumber=%d', [LineNo]));
      end else
{$ENDIF}begin
        // SourceLocationFromAddress does not exist or does not
        // return a file name
        sl.Add('FileName=<not available>');
        sl.Add('LineNumber=<not available>');
      end;
    end;

    // list of exception classes
    if 0 <> ExceptionObject then begin
      sl.Add(Format('ExceptionObject=%s', [IntToHex(ExceptionObject, 8)]));
      // read class from object.
      LClass := ReadPointer(IProcess, ExceptionObject);
      // get all classes.
      s := GetClasses(IProcess, LClass);
      sl.Add(Format('Classes=[%s]', [s]));
    end;
{$IFOPT D+}
    for s in sl do
      SendDebugWarning(s);
{$ENDIF}
    s := sl.Text;
  finally
    sl.Free();
  end;

  Action := efaDisabled;
  if Assigned(OnCheckException) then begin
    OnCheckException(nil, Projectname, ExceptionName, Msg, Action);
  end;

  if Action = efaDisabled then begin
    if TfmExceptionNotification.Execute(nil, OnIgnoreButtonClick, Projectname, ExceptionName, Msg) then
      Action := efaBreak
    else
      Action := efaIgnore;
  end;

  case Action of
    efaBreak: begin
        Result := False;
      end;
  else
    Result := True;

    // this will resume running
    P := Pointer(Process);
    PByte(GXNativeInt(P) + ParamOffset)^ := 1; // resume = true.
    PostDebugMessage(Debugger, 1, Process);
  end;
end;

const
  // the packagen names are version specific, we can get it from the MajorVersionNumber
{$IFDEF GX_DELPHI2007_UP}
{$IFDEF GX_DELPHI2009_UP}
  // Delphi 2009 and later
  VerString = MajorVersionNumberChar + '0';
{$ELSE}
  // Delphi 2007
  VerString = '100';
{$ENDIF}
{$ELSE}
  // Delphi 2005 and 2006
  VerString = MajorVersionNumberChar + '0';
{$ENDIF}
  Win32DebugIdePackage = 'win32debugide' + VerString + '.bpl';
  DbkDebugIdePackage = 'dbkdebugide' + VerString + '.bpl';
{$IFDEF GX_DELPHIXE_UP}
  // the name of the debugger and thread object changed with Delphi XE
  Win32Debugger = 'TNativeDebugger';
{$ELSE}
  Win32Debugger = 'TWin32Debugger';
{$ENDIF}
  DoShowExceptionName = '@Win32debug@' + Win32Debugger + '@DoShowException$qqrv';
  GetExceptionMessageName = '@Debug@TDebugger@GetExceptionMessage$qqrv';
  GetExceptionNameName = '@Debug@TDebugger@GetExceptionName$qqrv';
  GetFilenameName = '@Debug@TDebugger@GetFilename$qqrv';
  PostDebugMessageName = '@Debug@TDebugger@PostDebugMessage$qqr15Debug@TDebugMsgpv';
  ParseThreadOsInfoName = '@Win32debug@TNativeThread@ParseThreadOsInfo$qqrrx19Dbk@DbkThreadOsInfor31Win32debug@TExceptionRecordInfo';

procedure InstallHook(_OnCheckException, _OnFilterButtonClick: TOnCheckExceptionEx);
var
  Win32DebugHandle: HMODULE;
  DbkDebugIdeHandle: HMODULE;
{$IFDEF IS_WIN32_ONLY}
  Kernel32Handle: HMODULE;
  WaitForDebugEventPtr: Pointer;
{$ENDIF}
  DoShowExceptionPtr: Pointer;
  TransactionHandle: THandle;
begin
  if Hooked then
    Exit; //==>

  OnCheckException := _OnCheckException;
  OnIgnoreButtonClick := _OnFilterButtonClick;

  Win32DebugHandle := GetModuleHandle(Win32DebugIdePackage);
  DbkDebugIdeHandle := GetModuleHandle(DbkDebugIdePackage);

{$IFDEF IS_WIN32_ONLY}
  Kernel32Handle := GetModuleHandle(PChar(kernel32));
  WaitForDebugEventPtr := GetProcAddress(Kernel32Handle, 'WaitForDebugEvent');
  Assert(Assigned(WaitForDebugEventPtr), 'WaitForDebugEventPtr not assigned');
{$ENDIF}
  DoShowExceptionPtr := GetProcAddress(Win32DebugHandle, DoShowExceptionName);

  GetThreadOsInfo := GetProcAddress(DbkDebugIdeHandle, '@Debug@TThread@GetThreadOsInfo$qqrr19Dbk@DbkThreadOsInfo');

  // this may fail, but we check whether it is assigned before we call it
  ParseThreadOsInfo := GetProcAddress(Win32DebugHandle, ParseThreadOsInfoName);

  FindExceptionAddress := GetProcAddress(DbkDebugIdeHandle, '@Debug@TDebugger@FindExceptionAddress$qqrv');
  GetExceptionClass := GetProcAddress(Win32DebugHandle, '@Win32debug@TNativeThread@GetExceptionClass$qqrv');
  GetExceptionAddress := GetProcAddress(DbkDebugIdeHandle, '@Debug@TThread@GetExceptionAddress$qqrv');

  GetExceptionMessage := GetProcAddress(DbkDebugIdeHandle, GetExceptionMessageName);
  PostDebugMessage := GetProcAddress(DbkDebugIdeHandle, PostDebugMessageName);
  GetExceptionDisplayNameProc := GetProcAddress(DbkDebugIdeHandle, '@Debug@TDebugger@GetExceptionDisplayName$qqrv');
  GetExceptionName := GetProcAddress(DbkDebugIdeHandle, GetExceptionNameName);

  Assert(Assigned(GetThreadOsInfo), 'GetThreadOsInfo not assigned.');

  Assert(Assigned(DoShowExceptionPtr), 'DoShowExceptionPtr not assigned.');
  Assert(Assigned(FindExceptionAddress), 'FindExceptionAddress not assigned.');
  Assert(Assigned(GetExceptionAddress), 'GetExceptionAddress not assigned.');
  Assert(Assigned(GetExceptionMessage), 'GetExceptionMessage not assigned.');
  Assert(Assigned(GetExceptionName), 'GetExceptionName not assigned.');

  if Assigned(DoShowExceptionPtr) and Assigned(GetExceptionMessage)
    and Assigned(PostDebugMessage) and Assigned(GetExceptionName) then begin

    TransactionHandle := BeginTransaction();
{$IFDEF IS_WIN32_ONLY}
// todo: We need this also in the case that ParseThreadOsInfo could not be determined

// you need to be carefully when hooking WaitForDebugEvent as its a critical function.
// its highly recommended that you only hook it before the debugger is started.
// ==> hook it when your plugin is loaded and unhook it when your plugin is unloaded.
// ==> don't unhook when user disable exception expert.
// ==> always hook/unhook inside BeginTransaction/EndTransaction.
    @TrampolineWaitForDebugEvent := InterceptCreate(WaitForDebugEventPtr, @InterceptWaitForDebugEvent);
{$ENDIF}
    @TrampolineDoShowException := InterceptCreate(DoShowExceptionPtr, @DoShowExceptionHooked);
    EndTransaction(TransactionHandle);

    Hooked := True;
  end;
end;

procedure UninstallHook();
var
  TransactionHandle: THandle;
begin
  if not Hooked then
    Exit; //==>

  TransactionHandle := DDetours.BeginTransaction();
  if Assigned(TrampolineDoShowException) then
    InterceptRemove(Pointer(@TrampolineDoShowException));
  Hooked := False;
  OnCheckException := nil;
  OnIgnoreButtonClick := nil;
  DDetours.EndTransaction(TransactionHandle);
end;

initialization
  FDebugEventCritSect := TdzCriticalSection.Create;
finalization
  FreeAndNil(FDebugEventCritSect);
end.
