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
  end;

implementation

{$R *.dfm}

uses
  u_dzVclUtils;

{ Tf_GotoConfig }

class function Tf_GotoConfig.Execute(var _ReplaceSearchGoto: Boolean): Boolean;
var
  frm: Tf_GotoConfig;
  Int: IInterface;
begin
  // This buys (me) some time with adapting forms for High DPI by temporarily turning off
  // High DPI awareness. Works only for forms that are shown modally and don't
  // call into the IDE before closing.
  // All this is only necessary for Delphi 11 and later.
  // It does nothing for older Delphi versions.
  int := TemporarilyDisableHighDpi;
  frm := Tf_GotoConfig.Create(nil);
  try
    frm.TemporarilyDisableHighDpiInterface := int;
    Int := nil;
    frm.chk_ReplaceSearchGoto.Checked := _ReplaceSearchGoto;
    Result := (frm.ShowModal = mrOk);
    if Result then
      _ReplaceSearchGoto := frm.chk_ReplaceSearchGoto.Checked;
  finally
    FreeAndNil(frm);
  end;
end;

end.
