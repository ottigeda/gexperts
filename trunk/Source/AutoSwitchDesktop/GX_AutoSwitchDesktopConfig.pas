unit GX_AutoSwitchDesktopConfig;

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
  GX_BaseForm;

type
  Tf_AutoSwitchDestkopConfig = class(TfmBaseForm)
    cmb_FormDesktop: TComboBox;
    cmb_CodeDesktop: TComboBox;
    l_FormDesktop: TLabel;
    l_CodeDesktop: TLabel;
    b_Ok: TButton;
    b_Cancel: TButton;
  private
    procedure SetData(const _FormDesktop, _CodeDesktop: string);
    procedure GetData(var _FormDesktop, _CodeDesktop: string);
  public
    class function Execute(_Owner: TWinControl; var _FormDesktop, _CodeDesktop: string): Boolean;
    constructor Create(_Owner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  u_dzVclUtils,
  GX_IdeUtils;

{ Tf_SwitchDestkopConfig }

class function Tf_AutoSwitchDestkopConfig.Execute(_Owner: TWinControl; var _FormDesktop, _CodeDesktop: string): Boolean;
var
  frm: Tf_AutoSwitchDestkopConfig;
begin
  frm := Tf_AutoSwitchDestkopConfig.Create(_Owner);
  try
    TForm_CenterOn(frm, _Owner);
    frm.SetData(_FormDesktop, _CodeDesktop);
    Result := (frm.ShowModal = mrOk);
    if Result then
      frm.GetData(_FormDesktop, _CodeDesktop);
  finally
    FreeAndNil(frm);
  end;
end;

constructor Tf_AutoSwitchDestkopConfig.Create(_Owner: TComponent);
begin
  inherited;
  TControl_SetMinConstraints(Self);
  InitDpiScaler;
end;

procedure Tf_AutoSwitchDestkopConfig.GetData(var _FormDesktop, _CodeDesktop: string);
var
  s: string;
begin
  if TComboBox_GetSelected(cmb_FormDesktop, s) then
    _FormDesktop := s;
  if TComboBox_GetSelected(cmb_CodeDesktop, s) then
    _CodeDesktop := s;
end;

procedure Tf_AutoSwitchDestkopConfig.SetData(const _FormDesktop, _CodeDesktop: string);
var
  sl: TStrings;
begin
  sl := GetViewDesktopEntries;
  if Assigned(sl) then begin
    cmb_FormDesktop.Items.Assign(sl);
    cmb_CodeDesktop.Items.Assign(sl);
    FreeAndNil(sl);
  end;
  TComboBox_Select(cmb_FormDesktop, _FormDesktop);
  TComboBox_Select(cmb_CodeDesktop, _CodeDesktop);
end;

end.

