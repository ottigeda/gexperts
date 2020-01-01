unit u_dzErrorThread;

{$INCLUDE 'dzlibjedi.inc'}

interface

uses
  Windows,
  SysUtils,
  Classes,
  u_dzNamedThread;

type
  ///<summary>
  /// A thread that handles exceptions and provides the error message as well as has a HasFinished
  /// property.
  /// Do not override Execute, override doExecute instead. </summary>
  TErrorThread = class(TNamedThread)
  private
    FExceptionClass: TClass;
    FErrorMessage: string;
    FHasFinished: Boolean;
{$IF not Declared(SyncEvent)}
    // In Delphi 6 these are private in TThread so we can't simply access them but must implement
    // them ourselves.
    procedure CheckThreadError(ErrCode: Integer); overload;
    procedure CheckThreadError(Success: Boolean); overload;
{$IFEND}
  protected
    ///<summary>
    /// Calls inherited to set the thread name and then the doExecute method.
    /// Any Exceptions are caught and their Message stored in ErrorMessage.
    /// After the doExecute method has finished the HasFinished property is set to true. </summary>
    procedure Execute; override;
    ///<summary>
    /// empty method that must be overriden by descendants to do anything </summary>
    procedure doExecute; virtual;
  public
    ///<summary>
    /// Overloaded version of TThread.WaitFor which uses a timeout.
    /// @param TimeoutMsecs is the desired timeout in milliseconds
    /// @param ReturnValue is the result of the thread procecedure (the ReturnValue property
    ///                    of TThread). Only valid if Result = True.
    /// @returns True, if the thread has terminated, False otherwise. </summary>
    function WaitFor(_TimeoutMsecs: DWORD; out _ReturnValue: DWORD): Boolean; overload;
    function WaitFor(_TimeoutMsecs: DWORD): Boolean; overload;
    ///<summary>
    /// Is true, when the thread has finished executing </summary>
    property HasFinished: Boolean read FHasFinished;
    ///<summary>
    /// If an unhandled exception occurred in the callback, its message is stored in ErrorMessage.
    /// Only valid after the thread has finished executing (that is HasFinished = true).
    /// see also TThread.FatalException (which is never set if you are using TErrorThread!) </summary>
    property ErrorMessage: string read FErrorMessage;
    ///<summary>
    /// Class of exception whose message was stored in ErrorMessage </summary>
    property ExceptionClass: TClass read FExceptionClass;
  end;

implementation

uses
  RTLConsts;

{ TErrorThread }

{$IF not Declared(SyncEvent)}
procedure TErrorThread.CheckThreadError(ErrCode: Integer);
begin
  if ErrCode <> 0 then
    raise EThread.CreateFmt(SThreadError, [SysErrorMessage(ErrCode), ErrCode]);
end;

procedure TErrorThread.CheckThreadError(Success: Boolean);
begin
  if not Success then
    CheckThreadError(GetLastError);
end;
{$IFEND}

procedure TErrorThread.doExecute;
begin
  // does nothing
end;

procedure TErrorThread.Execute;
begin
  try
    try
      inherited;
      doExecute;
    except
      on e: Exception do begin
        FExceptionClass := e.ClassType;
        FErrorMessage := e.Message;
        UniqueString(FErrorMessage);
      end;
    end;
  finally
    FHasFinished := True;
  end;
end;

function TErrorThread.WaitFor(_TimeoutMsecs: DWORD): Boolean;
var
  Dummy: DWORD;
begin
  Result := WaitFor(_TimeoutMsecs, Dummy);
end;

{$IF Declared(SyncEvent)}

function TErrorThread.WaitFor(_TimeoutMsecs: DWORD; out _ReturnValue: DWORD): Boolean;
var
  H: array[0..1] of THandle;
  WaitResult: Cardinal;
  Msg: TMsg;
begin
  H[0] := Handle;
  if GetCurrentThreadID = MainThreadID then begin
    WaitResult := 0;
    H[1] := SyncEvent;
    repeat
      { This prevents a potential deadlock if the background thread
        does a SendMessage to the foreground thread }
      if WaitResult = WAIT_OBJECT_0 + 2 then
        PeekMessage(Msg, 0, 0, 0, PM_NOREMOVE);
      if _TimeoutMsecs = INFINITE then begin
        WaitResult := MsgWaitForMultipleObjects(2, H, False, 1000, QS_SENDMESSAGE);
      end else begin
        WaitResult := MsgWaitForMultipleObjects(2, H, False, _TimeoutMsecs, QS_SENDMESSAGE);
      end;
      CheckThreadError(WaitResult <> WAIT_FAILED);
      if WaitResult = WAIT_OBJECT_0 + 1 then
        CheckSynchronize;
      Result := (WaitResult = WAIT_OBJECT_0);
    until Result or (_TimeoutMsecs <> INFINITE);
  end else begin
    WaitResult := WaitForSingleObject(H[0], _TimeoutMsecs);
    if WaitResult = WAIT_FAILED then
      RaiseLastOSError;
    Result := (WaitResult <> WAIT_TIMEOUT);
  end;
  if Result then
    CheckThreadError(GetExitCodeThread(H[0], _ReturnValue));
end;

{$ELSE}
// Delphi 6 did not have a SyncEvent variable (later versions declare it in Classes)

function TErrorThread.WaitFor(_TimeoutMsecs: DWORD; out _ReturnValue: DWORD): Boolean;
var
  H: THandle;
  WaitResult: Cardinal;
  Msg: TMsg;
begin
  H := Handle;
  if GetCurrentThreadID = MainThreadID then begin
    WaitResult := 0;
    repeat
      { This prevents a potential deadlock if the background thread
        does a SendMessage to the foreground thread }
      if WaitResult = WAIT_OBJECT_0 + 1 then
        PeekMessage(Msg, 0, 0, 0, PM_NOREMOVE);
      if _TimeoutMsecs = INFINITE then begin
        WaitResult := MsgWaitForMultipleObjects(2, H, False, 1000, QS_SENDMESSAGE)
      end else begin
        WaitResult := MsgWaitForMultipleObjects(1, H, False, _TimeoutMsecs, QS_SENDMESSAGE);
      end;
      CheckThreadError(WaitResult <> WAIT_FAILED);
      if WaitResult = WAIT_OBJECT_0 + 1 then
        CheckSynchronize;
      Result := (WaitResult = WAIT_OBJECT_0);
    until Result or (_TimeoutMsecs <> INFINITE);
  end else begin
    WaitResult := WaitForSingleObject(H, _TimeoutMsecs);
    Result := True;
    if WaitResult = WAIT_FAILED then
      RaiseLastOSError;
    Result := (WaitResult <> WAIT_TIMEOUT);
  end;
  if Result then
    CheckThreadError(GetExitCodeThread(H, _ReturnValue));
end;
{$IFEND}

end.
