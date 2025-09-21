unit GX_BackupOptions;

{$I GX_CondDefine.inc}

interface

uses
  Classes, Controls, Forms, StdCtrls, ExtCtrls, GX_BaseForm;

type
  TfmBackupOptions = class(TfmBaseForm)
    gbBackupOptions: TGroupBox;
    btnOK: TButton;
    btnCancel: TButton;
    cbPassword: TCheckBox;
    lPassword: TLabel;
    edPassword: TEdit;
    rgScope: TRadioGroup;
    cbSearchLibraryPath: TCheckBox;
    procedure cbPasswordClick(Sender: TObject);
    public
    constructor Create(_Owner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  Graphics,
  GX_GenericUtils,
  GX_IdeUtils,
  u_dzVclUtils;

procedure TfmBackupOptions.cbPasswordClick(Sender: TObject);
begin
  edPassword.Enabled := cbPassword.Checked;
  if edPassword.Enabled then
  begin
    edPassword.Color := GetThemedColor(clWindow);
    TryFocusControl(edPassword);
  end
  else
    edPassword.Color := clBtnface;
  edPassword.Refresh;
end;

constructor TfmBackupOptions.Create(_Owner: TComponent);
begin
  inherited;
  TControl_SetMinConstraints(Self);
  InitDpiScaler;
end;

end.
