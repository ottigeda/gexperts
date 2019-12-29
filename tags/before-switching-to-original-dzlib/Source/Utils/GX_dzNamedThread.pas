///<summary>
/// If this unit is included in a program, it will set the name for the main thread to 'Main'.
/// Also it declares a TNamedThread type that uses its class name for the thread
/// name. Make sure to override this if there are multiple instances of this class.
/// If you do not want to derive from TNamedThread, just call SetThreadName from
/// your thread's execute procedure. </summary>
unit GX_dzNamedThread;

{$I 'GX_CondDefine.inc'}

interface

uses
  Classes,
  Windows;

type
  ///<summary> This record must be filled to set the name of a thread </summary>
  TThreadNameInfo = record
    FType: LongWord; // must be 0x1000
    FName: PAnsiChar; // pointer to name (in user address space)
    FThreadID: LongWord; // thread ID (-1 indicates caller thread)
    FFlags: LongWord; // reserved for future use, must be zero
  end;

type
  /// <summary>
  /// A TThread that sets its name to its class name. Make sure you call
  /// inherited Execute in descendants! </summary>
  TNamedThread = class(TThread)
  protected
    FThreadName: string;
    FHasFinished: Boolean;
    ///<summary>
    /// Calls SetThreadName with FThreadName </summary>
    procedure SetName; virtual;
    ///<summary>
    /// Calls SetName and doExecute
    /// @NOTE that this method is not virtual, override doExecute instead </summary>
{$IFDEF GX_DELPHI2005_UP}
    // only Delphi 2005 and later support the final keyword
    procedure Execute; override; final;
{$ELSE}
    procedure Execute; override;
{$ENDIF}
    ///<summary>
    /// Does nothing, override to do the actual work </summary>
    procedure doExecute; virtual;
    function GetThreadName: string;
  public
    ///<summary>
    /// Set the name (displayed in the debugger) for the current thread
    /// @param Name is a string with the name to use </summary>
    class procedure SetThreadName(const _Name: AnsiString);
    ///<summary>
    /// @param CreateSuspended (see TThread.Create)
    /// @param Name is the name to set for the thread, if empty, the ClassName will be used </summary>
    constructor Create(_CreateSuspended: Boolean; const _Name: string = '');
    ///<summary>
    /// Is set to true when the doExecute method exits </summary>
    property HasFinished: Boolean read FHasFinished;
  end;

implementation

{$IFDEF GX_VER150_up}
{$WARN UNSAFE_CODE OFF}
{$ENDIF}

class procedure TNamedThread.SetThreadName(const _Name: AnsiString);
var
  ThreadNameInfo: TThreadNameInfo;
begin
  ThreadNameInfo.FType := $1000;
  ThreadNameInfo.FName := PAnsiChar(_Name);
  ThreadNameInfo.FThreadID := $FFFFFFFF;
  ThreadNameInfo.FFlags := 0;
  try
    RaiseException($406D1388, 0, SizeOf(ThreadNameInfo) div SizeOf(LongWord), Pointer(@ThreadNameInfo));
  except
    // ignore
  end;
end;

{ TNamedThread }

constructor TNamedThread.Create(_CreateSuspended: Boolean; const _Name: string = '');
begin
  if _Name <> '' then
    FThreadName := _Name
  else
    FThreadName := ClassName;

  inherited Create(_CreateSuspended);
end;

procedure TNamedThread.doExecute;
begin
  // does nothing
end;

procedure TNamedThread.Execute;
begin
  try
    SetName;
    doExecute;
  finally
    FHasFinished := True;
  end;
end;

function TNamedThread.GetThreadName: string;
begin
  Result := FThreadName;
end;

procedure TNamedThread.SetName;
begin
  SetThreadName(AnsiString(FThreadName));
end;

initialization
  // set the name for the main thread to 'Main'
  TNamedThread.SetThreadName('Main');
end.
