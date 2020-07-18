unit GX_FilterExceptionsNotification;

{$I GX_CondDefine.inc}

{$IFNDEF GX_DELPHI2005_UP}
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
    act_Ignore: TAction;
    act_CopyToClipboard: TAction;
  private
    FProject: string;
    FException: string;
    FMessage: string;
    FOnAddException: TOnCheckExceptionEx;
    procedure act_IgnoreExecute(Sender: TObject);
    procedure act_CopyToClipboardExecute(Sender: TObject);
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
  u_dzVclUtils,
  DDetours,
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
  act_Ignore.OnExecute := act_IgnoreExecute;
  act_CopyToClipboard.OnExecute := act_CopyToClipboardExecute;
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
    + '[' + b_Ignore.Caption + ']  [' + b_Break.Caption + ']  [' + b_Continue.Caption + ']'#13#10
    + '---------------------------';
end;

procedure TfmExceptionNotification.act_IgnoreExecute(Sender: TObject);
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

var
  Hooked: Boolean = False;
  OnCheckException: TOnCheckExceptionEx = nil;
  OnIgnoreButtonClick: TOnCheckExceptionEx = nil;

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

var
  TrampolineDoShowException: TDoShowException;

function DoShowExceptionHooked(Obj: TObject): Boolean;
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
  ExceptionName: string;
  Projectname: string;
begin
{$IFOPT D+}SendDebug('DoShowExceptionHooked was called');
{$ENDIF}

  GetExceptionMessage(Obj, Msg);
{$IFOPT D+}SendDebugFmt('Exception message is "%s"', [Msg]);
{$ENDIF}
  GetExceptionName(Obj, ExceptionName);
{$IFOPT D+}SendDebugFmt('Exception name is "%s"', [ExceptionName]);
{$ENDIF}
  Projectname := GxOtaGetCurrentProjectName;
{$IFOPT D+}SendDebugFmt('Project name is "%s"', [Projectname]);
{$ENDIF}

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
    P := GetParam(Obj);
    PByte(GXNativeInt(P) + ParamOffset)^ := 1; // resume = true.
    PostDebugMessage(Obj, 1, P);
  end;
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

procedure InstallHook(_OnCheckException, _OnFilterButtonClick: TOnCheckExceptionEx);
var
  Win32DebugHandle: HMODULE;
  DbkDebugIdeHandle: HMODULE;
  DoShowExceptionPtr: Pointer;
begin
  if Hooked then
    Exit;

  OnCheckException := _OnCheckException;
  OnIgnoreButtonClick := _OnFilterButtonClick;

  Win32DebugHandle := GetModuleHandle(Win32DebugIdePackage);
  DbkDebugIdeHandle := GetModuleHandle(DbkDebugIdePackage);

  DoShowExceptionPtr := GetProcAddress(Win32DebugHandle, DoShowExceptionName);
  GetExceptionMessage := GetProcAddress(DbkDebugIdeHandle, GetExceptionMessageName);
  GetExceptionName := GetProcAddress(DbkDebugIdeHandle, GetExceptionNameName);
  PostDebugMessage := GetProcAddress(DbkDebugIdeHandle, PostDebugMessageName);

  if Assigned(DoShowExceptionPtr) and Assigned(GetExceptionMessage)
    and Assigned(PostDebugMessage) and Assigned(GetExceptionName) then begin

    @TrampolineDoShowException := InterceptCreate(DoShowExceptionPtr, @DoShowExceptionHooked);

    Hooked := True;
  end;
end;

procedure UninstallHook();
begin
  if not Hooked then
    Exit; //==>

  InterceptRemove(@TrampolineDoShowException);
  Hooked := False;
  OnCheckException := nil;
  OnIgnoreButtonClick := nil;
end;

end.
