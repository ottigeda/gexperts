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
    chk_ReplaceFindInFiles: TCheckBox;
    b_Ok: TButton;
    b_Cancel: TButton;
  private
    procedure SetData(_ReplaceFind, _ReplaceFindInFiles: Boolean);
    procedure GetData(out _ReplaceFind, _ReplaceFindInFiles: Boolean);
  public
    class function Execute(_Owner: TWinControl; var _ReplaceFind, _ReplaceFindInFiles: Boolean): Boolean;
    constructor Create(_Owner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  u_dzVclUtils;

{ Tf_GrepMenuConfig }

class function Tf_GrepMenuConfig.Execute(_Owner: TWinControl; var _ReplaceFind,
  _ReplaceFindInFiles: Boolean): Boolean;
var
  frm: Tf_GrepMenuConfig;
begin
  frm := Tf_GrepMenuConfig.Create(_Owner);
  try
    TForm_CenterOn(frm, _Owner);
    frm.SetData(_ReplaceFind, _ReplaceFindInFiles);
    Result := (mrOk = frm.ShowModal);
    if Result then
      frm.GetData(_ReplaceFind, _ReplaceFindInFiles);
  finally
    FreeAndNil(frm);
  end;
end;

constructor Tf_GrepMenuConfig.Create(_Owner: TComponent);
begin
  inherited;

end;

procedure Tf_GrepMenuConfig.GetData(out _ReplaceFind, _ReplaceFindInFiles: Boolean);
begin
  _ReplaceFind := chk_ReplaceFind.Checked;
  _ReplaceFindInFiles := chk_ReplaceFindInFiles.Checked;
end;

procedure Tf_GrepMenuConfig.SetData(_ReplaceFind, _ReplaceFindInFiles: Boolean);
begin
  chk_ReplaceFind.Checked := _ReplaceFind;
  chk_ReplaceFindInFiles.Checked := _ReplaceFindInFiles;
end;

end.

