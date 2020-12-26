///<sumamry>
/// This expert prevents the ExplicitLeft/Right/Widht/Height properties of TControl
/// to be written to the dfm file
/// @Authors  : Thomas Mueller, with code from Achim Kalwa and Andreas Hausladen </summary>
unit GX_ExplicitFilterExpert;

{$I GX_CondDefine.inc}

interface

{$IFDEF GX_DELPHI2006_UP}

uses
  Windows,
  SysUtils,
  Classes,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  GX_Experts,
  GX_BaseForm;

type
  TExplicitProperties = (epLeft, epTop, epWidth, epHeight);
  TExplicitPropertiesSet = set of TExplicitProperties;

type
  TfmGxExplicitFilter = class(TfmBaseForm)
    btnOK: TButton;
    btnCancel: TButton;
    chk_WriteExplicitLeft: TCheckBox;
    chk_WriteExplicitTop: TCheckBox;
    chk_WriteExplicitWidth: TCheckBox;
    chk_WriteExplicitHeight: TCheckBox;
  private
  public
    procedure SetData(_Value: TExplicitPropertiesSet);
    procedure GetData(out _Value: TExplicitPropertiesSet);
  end;

{$ENDIF GX_DELPHI2006_UP}

implementation

{$IFDEF GX_DELPHI2006_UP}

{$R *.dfm}

uses
{$IFOPT D+}GX_DbugIntf,
{$ENDIF}
  Registry,
  Menus,
  GX_GExperts,
  GX_ConfigurationInfo,
  GX_GenericUtils;

var
  // we can't make this a field because it must be accessible from the hooked code
  // (actually we could but that would mean a global variable with the expert instance which isn't any better)
  gblWriteExplicitPropSet: TExplicitPropertiesSet = [];

{ Hooking - from Andy Hausladen's VCLFixpack }

type
  TJumpOfs = Integer;
  PPointer = ^Pointer;
  PXRedirCode = ^TXRedirCode;
  TXRedirCode = packed record
    Jump: Byte;
    Offset: TJumpOfs;
  end;

  PAbsoluteIndirectJmp = ^TAbsoluteIndirectJmp;
  TAbsoluteIndirectJmp = packed record
    OpCode: Word; //$FF25(Jmp, FF /4)
    Addr: PPointer;
  end;

function GetActualAddr(Proc: Pointer): Pointer;
begin
  if Proc <> nil then begin
    if (PAbsoluteIndirectJmp(Proc).OpCode = $25FF) then
      Result := PAbsoluteIndirectJmp(Proc).Addr^
    else
      Result := Proc;
  end else
    Result := nil;
end;

type
{$IFDEF GX_DELPHIXE2_UP}
  NumberOfBytesType = NativeUInt;
{$ELSE}
  NumberOfBytesType = Cardinal;
{$ENDIF}

procedure HookProc(Proc, Dest: Pointer; var BackupCode: TXRedirCode);
var
  n: NumberOfBytesType;
  Code: TXRedirCode;
begin
  Proc := GetActualAddr(Proc);
  Assert(Proc <> nil);
  if ReadProcessMemory(GetCurrentProcess, Proc, @BackupCode, SizeOf(BackupCode), n) then begin
    Code.Jump := $E9;
    Code.Offset := PAnsiChar(Dest) - PAnsiChar(Proc) - SizeOf(Code);
    WriteProcessMemory(GetCurrentProcess, Proc, @Code, SizeOf(Code), n);
  end;
end;

procedure UnhookProc(Proc: Pointer; var BackupCode: TXRedirCode);
var
  n: NumberOfBytesType;
begin
  if (BackupCode.Jump <> 0) and (Proc <> nil) then begin
    Proc := GetActualAddr(Proc);
    Assert(Proc <> nil);
    WriteProcessMemory(GetCurrentProcess, Proc, @BackupCode, SizeOf(BackupCode), n);
    BackupCode.Jump := 0;
  end;
end;

{ TControlExplicitFix - by Achim Kalwa }

type
  TControlHack = class(TControl)
  end;

var
  Control_DefineProperties: Pointer;
  BackupDefineProperties: TXRedirCode;

type
  TControlExplicitFix = class(TControl)
  private
    procedure ReadExplicitHeight(_Reader: TReader);
    procedure ReadExplicitLeft(_Reader: TReader);
    procedure ReadExplicitTop(_Reader: TReader);
    procedure ReadExplicitWidth(_Reader: TReader);
    procedure ReadIsControl(_Reader: TReader);

    procedure WriteExplicitHeight(_Writer: TWriter);
    procedure WriteExplicitLeft(_Writer: TWriter);
    procedure WriteExplicitTop(_Writer: TWriter);
    procedure WriteExplicitWidth(_Writer: TWriter);
    procedure WriteIsControl(_Writer: TWriter);
  protected
    procedure DefineProperties(_Filer: TFiler); override;
  end;

procedure TControlExplicitFix.WriteExplicitTop(_Writer: TWriter);
begin
  _Writer.WriteInteger(FExplicitTop);
end;

procedure TControlExplicitFix.WriteExplicitHeight(_Writer: TWriter);
begin
  _Writer.WriteInteger(FExplicitHeight);
end;

procedure TControlExplicitFix.WriteExplicitLeft(_Writer: TWriter);
begin
  _Writer.WriteInteger(FExplicitLeft);
end;

procedure TControlExplicitFix.ReadExplicitWidth(_Reader: TReader);
begin
  FExplicitWidth := _Reader.ReadInteger;
end;

procedure TControlExplicitFix.WriteExplicitWidth(_Writer: TWriter);
begin
  _Writer.WriteInteger(FExplicitWidth);
end;

procedure TControlExplicitFix.ReadExplicitTop(_Reader: TReader);
begin
  FExplicitTop := _Reader.ReadInteger;
end;

procedure TControlExplicitFix.ReadExplicitHeight(_Reader: TReader);
begin
  FExplicitHeight := _Reader.ReadInteger;
end;

procedure TControlExplicitFix.ReadExplicitLeft(_Reader: TReader);
begin
  FExplicitLeft := _Reader.ReadInteger;
end;

procedure TControlExplicitFix.ReadIsControl(_Reader: TReader);
begin
  IsControl := _Reader.ReadBoolean;
end;

procedure TControlExplicitFix.WriteIsControl(_Writer: TWriter);
begin
  _Writer.WriteBoolean(IsControl);
end;

procedure TControlExplicitFix.DefineProperties(_Filer: TFiler);

  function DoWriteIsControl: Boolean;
  begin
    if _Filer.Ancestor <> nil then
      Result := TControlHack(_Filer.Ancestor).IsControl <> IsControl
    else
      Result := IsControl;
  end;

  function DoWriteExplicit(_Prop: TExplicitProperties): Boolean;
  begin
    Result := _Prop in gblWriteExplicitPropSet;
  end;

begin
  // no inherited! See comment in Controls.pas
  _Filer.DefineProperty('IsControl', ReadIsControl, WriteIsControl, DoWriteIsControl);
  _Filer.DefineProperty('ExplicitLeft', ReadExplicitLeft, WriteExplicitLeft, not (csReading in ComponentState) and DoWriteExplicit(epLeft));
  _Filer.DefineProperty('ExplicitTop', ReadExplicitTop, WriteExplicitTop, not (csReading in ComponentState) and DoWriteExplicit(epTop));
  _Filer.DefineProperty('ExplicitWidth', ReadExplicitWidth, WriteExplicitWidth, not (csReading in ComponentState) and DoWriteExplicit(epWidth));
  _Filer.DefineProperty('ExplicitHeight', ReadExplicitHeight, WriteExplicitHeight, not (csReading in ComponentState) and DoWriteExplicit(epHeight));
end;

procedure InitControlExplicitFix;
begin
  // InitializeCriticalSection(DialogsTaskModalDialogCritSect);
  Control_DefineProperties := @TControlHack.DefineProperties;
  HookProc(Control_DefineProperties, @TControlExplicitFix.DefineProperties, BackupDefineProperties);

{$IFOPT D+}GX_DbugIntf.SendDebug('InitControlExplicitFix');
{$ENDIF}
end;

procedure FinalizeControlExplicitFix;
begin
  UnhookProc(Control_DefineProperties, BackupDefineProperties);
  Control_DefineProperties := nil;
end;

type
  ///<summary>
  /// Prevent the ExplicitXxxx properties to be written to the dfm files </summary>
  TExplicitFilterExpert = class(TGX_Expert)
  private
  protected
    procedure SetActive(_Value: Boolean); override;
  public
    function CanHaveShortCut: Boolean; override;
    constructor Create; override;
    destructor Destroy; override;
    procedure Configure; override;
    function GetDisplayName: string; override;
    function GetHelpString: string; override;
    function HasMenuItem: Boolean; override;
    procedure InternalLoadSettings(_Settings: IExpertSettings); override;
    procedure InternalSaveSettings(_Settings: IExpertSettings); override;
    function IsDefaultActive: Boolean; override;
    procedure Execute(Sender: TObject); override;
  end;

{ TExplicitFilterExpert }

procedure TExplicitFilterExpert.Execute(Sender: TObject);
begin
  // do nothing
end;

function TExplicitFilterExpert.CanHaveShortCut: Boolean;
begin
  // we don't need a shortcut
  Result := False;
end;

procedure TExplicitFilterExpert.Configure;
var
  frm: TfmGxExplicitFilter;
begin
  frm := TfmGxExplicitFilter.Create(nil);
  try
    frm.SetData(gblWriteExplicitPropSet);
    if frm.ShowModal = mrOk then begin
      frm.GetData(gblWriteExplicitPropSet);
      SaveSettings;
    end;
  finally
    FreeAndNil(frm);
  end;
end;

constructor TExplicitFilterExpert.Create;
begin
  inherited Create;

  // Don't write any of the ExplicitXxxx properties
  // (If this expert is active. By default it isn't, see IsDefaultActive.)
  gblWriteExplicitPropSet := [];
end;

destructor TExplicitFilterExpert.Destroy;
begin
  if Assigned(Control_DefineProperties) then
    FinalizeControlExplicitFix;
  inherited Destroy;
end;

function TExplicitFilterExpert.GetDisplayName: string;
begin
  Result := 'Filter Explicit Properties';
end;

function TExplicitFilterExpert.GetHelpString: string;
resourcestring
  SExplicitFilterExpertHelp =
    'This expert prevents the ExplicitXxxx properties from being written to the dfm files.'#13#10
    + 'NOTE: You should only enable this expert if you do not already use'#13#10
    + 'Andreas Hausladen''s DDevExtensions with the same functionality';
begin
  Result := SExplicitFilterExpertHelp;
end;

function TExplicitFilterExpert.HasMenuItem: Boolean;
begin
  // we need no menu item
  Result := False;
end;

procedure TExplicitFilterExpert.InternalLoadSettings(_Settings: IExpertSettings);
begin
  inherited;
  gblWriteExplicitPropSet := TExplicitPropertiesSet(Byte(_Settings.ReadInteger('FilterSet', 0)));
end;

procedure TExplicitFilterExpert.InternalSaveSettings(_Settings: IExpertSettings);
begin
  inherited;
  _Settings.WriteInteger('FilterSet', Byte(gblWriteExplicitPropSet));
end;

function TExplicitFilterExpert.IsDefaultActive: Boolean;
begin
  // Since many people have installed Andy Hausladen's DDevExtensions which have a similar functionality
  // we don't enable this by default.
  Result := False;
end;

procedure TExplicitFilterExpert.SetActive(_Value: Boolean);
begin
  if _Value <> Active then begin
    inherited SetActive(_Value);
    if _Value then begin
      if not Assigned(Control_DefineProperties) then
        InitControlExplicitFix;
    end else begin
      if Assigned(Control_DefineProperties) then
        FinalizeControlExplicitFix;
    end;
  end;
end;

procedure TfmGxExplicitFilter.GetData(out _Value: TExplicitPropertiesSet);
begin
  _Value := [];
  if chk_WriteExplicitLeft.Checked then
    Include(_Value, epLeft);
  if chk_WriteExplicitTop.Checked then
    Include(_Value, epTop);
  if chk_WriteExplicitWidth.Checked then
    Include(_Value, epWidth);
  if chk_WriteExplicitHeight.Checked then
    Include(_Value, epHeight);
end;

procedure TfmGxExplicitFilter.SetData(_Value: TExplicitPropertiesSet);
begin
  chk_WriteExplicitLeft.Checked := epLeft in _Value;
  chk_WriteExplicitTop.Checked := epTop in _Value;
  chk_WriteExplicitWidth.Checked := epWidth in _Value;
  chk_WriteExplicitHeight.Checked := epHeight in _Value;
end;

initialization
  RegisterGX_Expert(TExplicitFilterExpert);
{$ENDIF GX_DELPHI2006_UP}
end.
