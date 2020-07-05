unit GX_ExceptionNotification;

{$I GX_CondDefine.inc}

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  GX_BaseForm,
  GX_EditExceptionNotification;

type
  TOnCheckExceptionEx = procedure(_Sender: TObject; const _Project, _Exception, _Message: string;
    var _Action: TExceptionNotificationAction) of object;

var
  Hooked: Boolean = False;
  OnCheckException: TOnCheckExceptionEx = nil;
  OnIgnoreButtonClick: TOnCheckExceptionEx = nil;

type
  TfmExceptionNotification = class(TfmBaseForm)
    l_Message: TLabel;
    b_Break: TButton;
    b_Continue: TButton;
    b_Ignore: TButton;
  private
{$IFDEF GX_DELPHI2005_UP}
    FProject: string;
    FException: string;
    FMessage: string;
    FOnAddException: TOnCheckExceptionEx;
    procedure b_IgnoreClick(Sender: TObject);
    procedure SetData(_OnAddException: TOnCheckExceptionEx;
      const _Project, _Exception, _Message: string);
  public
    class function Execute(_Owner: TWinControl; _OnAddException: TOnCheckExceptionEx;
      const _Project, _Exception, _Message: string): Boolean;
    constructor Create(_Owner: TComponent); override;
{$ENDIF}
  end;

implementation

{$R *.dfm}

{$IFDEF GX_DELPHI2005_UP}

uses
{$IFOPT D+}GX_DbugIntf,
{$ENDIF}
  u_dzVclUtils,
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
  b_Ignore.OnClick := b_IgnoreClick;
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

procedure TfmExceptionNotification.b_IgnoreClick(Sender: TObject);
var
  Action: TExceptionNotificationAction;
begin
  if Assigned(FOnAddException) then begin
    Action := enaDisabled;
    FOnAddException(Self, FProject, FException, FMessage, Action);
    case Action of
      enaIgnore: ModalResult := mrIgnore;
      enaBreak: ModalResult := mrok;
    end;
  end;
end;

type
  TDoShowException = function(Obj: TObject): Boolean;
  TGetExceptionMessage = procedure(Obj: TObject; var Msg: string);
  TGetExceptionName = procedure(Obj: TObject; var Msg: string);
  TPostDebugMessage = procedure(Obj: TObject; Msg: ShortInt; Param: Pointer);

var
  GetExceptionMessage: TGetExceptionMessage = nil;
  GetExceptionName: TGetExceptionName = nil;
  PostDebugMessage: TPostDebugMessage = nil;

function GetParam(Obj: Pointer): Pointer;
asm
  mov eax, [eax + $40]
end;

function DoShowExceptionHooked(Obj: TObject): Boolean;
var
  Msg: string;
  P: PByte;
  Action: TExceptionNotificationAction;
  ExceptionName: string;
  Projectname: string;
begin
{$IFOPT D+}SendDebug('DoShowExceptionHooked was called');
{$ENDIF}

  GetExceptionMessage(Obj, Msg);
{$IFOPT D+}SendDebugFmt('Exception message is %s', [Msg]);
{$ENDIF}
  GetExceptionName(Obj, ExceptionName);
{$IFOPT D+}SendDebugFmt('Exception name is %s', [ExceptionName]);
{$ENDIF}
  Projectname := GxOtaGetCurrentProjectName;
{$IFOPT D+}SendDebugFmt('Project name is %s', [Projectname]);
{$ENDIF}

  Action := enaDisabled;
  if Assigned(OnCheckException) then begin
    OnCheckException(nil, Projectname, ExceptionName, Msg, Action);
  end;

  if Action = enaDisabled then begin
    if TfmExceptionNotification.Execute(nil, OnIgnoreButtonClick, Projectname, ExceptionName, Msg) then
      Action := enaBreak
    else
      Action := enaIgnore;
  end;

  case Action of
    enaBreak: begin
        Result := False;
      end;
  else
    Result := True;

    // this will resume running
    P := GetParam(Obj);
    PByte(GXNativeInt(P) + $A1)^ := 1; // resume = true.
    PostDebugMessage(Obj, 1, P);
  end;
end;

procedure PatchCode(Address: Pointer; const NewCode; Size: Integer);
var
  OldProtect: DWORD;
begin
  if VirtualProtect(Address, Size, PAGE_EXECUTE_READWRITE, OldProtect) then begin
    Move(NewCode, Address^, Size);
    FlushInstructionCache(GetCurrentProcess, Address, Size);
    VirtualProtect(Address, Size, OldProtect, @OldProtect);
  end;
end;

type
  PInstruction = ^TInstruction;
  TInstruction = packed record
    Opcode: Byte;
    Offset: Integer;
  end;

///<summary>
/// Hooking fuer Imports des Executables
/// Quelle: https://stackoverflow.com/a/8978266/49925 </summary>
procedure RedirectProcedure(OldAddress, NewAddress: Pointer);
var
  NewCode: TInstruction;
begin
  NewCode.Opcode := $E9; //jump relative
  NewCode.Offset := GXNativeInt(NewAddress) - GXNativeInt(OldAddress) - SizeOf(NewCode);
  PatchCode(OldAddress, NewCode, SizeOf(NewCode));
end;

const
  // Delphi 6 and 7 don't have these packages, the debugger is probably part of the Delphi32.exe,
  // so we can't hook the functions.
{$IFDEF GX_DELPHI2007_UP}
{$IFDEF GX_DELPHI2009_UP}
  VerString = MajorVersionNumberChar + '0';
{$ELSE}
  VerString = '100';
{$ENDIF}
{$ELSE}
  VerString = MajorVersionNumberChar + '0';
{$ENDIF}
  Win32DebugIdePackage = 'win32debugide' + VerString + '.bpl';
  DbkDebugIdePackage = 'dbkdebugide' + VerString + '.bpl';
{$IFDEF GX_DELPHIXE_UP}
  Win32Debugger = 'TNativeDebugger';
{$ELSE}
  Win32Debugger = 'TWin32Debugger';
{$ENDIF}
  DoShowExceptionName = '@Win32debug@' + Win32Debugger + '@DoShowException$qqrv';
  GetExceptionMessageName = '@Debug@TDebugger@GetExceptionMessage$qqrv';
  GetExceptionNameName = '@Debug@TDebugger@GetExceptionName$qqrv';
  GetFilenameName = '@Debug@TDebugger@GetFilename$qqrv';
  PostDebugMessageName = '@Debug@TDebugger@PostDebugMessage$qqr15Debug@TDebugMsgpv';

procedure InstallHook();
var
  Win32DebugHandle: HMODULE;
  DbkDebugIdeHandle: HMODULE;
  DoShowExceptionPtr: Pointer;
begin
  if Hooked then
    Exit;

  Win32DebugHandle := GetModuleHandle(Win32DebugIdePackage);
  DbkDebugIdeHandle := GetModuleHandle(DbkDebugIdePackage);

  DoShowExceptionPtr := GetProcAddress(Win32DebugHandle, DoShowExceptionName);
  GetExceptionMessage := GetProcAddress(DbkDebugIdeHandle, GetExceptionMessageName);
  GetExceptionName := GetProcAddress(DbkDebugIdeHandle, GetExceptionNameName);
  PostDebugMessage := GetProcAddress(DbkDebugIdeHandle, PostDebugMessageName);

  if Assigned(DoShowExceptionPtr) and Assigned(GetExceptionMessage)
    and Assigned(PostDebugMessage) and Assigned(GetExceptionName) then begin
    RedirectProcedure(DoShowExceptionPtr, @DoShowExceptionHooked);
    Hooked := True;
  end;
end;

initialization
  InstallHook;
{$ENDIF}
end.
