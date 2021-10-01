unit GX_GotoConfig;

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
  Tf_GotoConfig = class(TfmBaseForm)
    chk_ReplaceSearchGoto: TCheckBox;
    b_OK: TButton;
    b_Cancel: TButton;
  private
  public
    class function Execute(var _ReplaceSearchGoto: Boolean): Boolean;
    constructor Create(_Owner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  u_dzVclUtils;

{ Tf_GotoConfig }

class function Tf_GotoConfig.Execute(var _ReplaceSearchGoto: Boolean): Boolean;
var
  frm: Tf_GotoConfig;
begin
  frm := Tf_GotoConfig.Create(nil);
  try
    frm.chk_ReplaceSearchGoto.Checked := _ReplaceSearchGoto;
    Result := (frm.ShowModal = mrOk);
    if Result then
      _ReplaceSearchGoto := frm.chk_ReplaceSearchGoto.Checked;
  finally
    FreeAndNil(frm);
  end;
end;

constructor Tf_GotoConfig.Create(_Owner: TComponent);
begin
  inherited;

  InitDpiScaler;
end;

end.
