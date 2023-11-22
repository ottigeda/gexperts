unit GX_GrepMenuConfig;

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
  Tf_GrepMenuConfig = class(TfmBaseForm)
    chk_ReplaceFind: TCheckBox;
    b_Ok: TButton;
    b_Cancel: TButton;
  private
    procedure SetData(_ReplaceFind: Boolean);
    procedure GetData(out _ReplaceFind: Boolean);
  public
    class function Execute(_Owner: TWinControl; var _ReplaceFind: Boolean): Boolean;
    constructor Create(_Owner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  u_dzVclUtils;

{ Tf_GrepMenuConfig }

class function Tf_GrepMenuConfig.Execute(_Owner: TWinControl; var _ReplaceFind: Boolean): Boolean;
var
  frm: Tf_GrepMenuConfig;
begin
  frm := Tf_GrepMenuConfig.Create(_Owner);
  try
    TForm_CenterOn(frm, _Owner);
    frm.SetData(_ReplaceFind);
    Result := (mrOk = frm.ShowModal);
    if Result then
      frm.GetData(_ReplaceFind);
  finally
    FreeAndNil(frm);
  end;
end;

constructor Tf_GrepMenuConfig.Create(_Owner: TComponent);
begin
  inherited;

end;

procedure Tf_GrepMenuConfig.GetData(out _ReplaceFind: Boolean);
begin
  _ReplaceFind := chk_ReplaceFind.Checked;
end;

procedure Tf_GrepMenuConfig.SetData(_ReplaceFind: Boolean);
begin
  chk_ReplaceFind.Checked := _ReplaceFind;
end;

end.

