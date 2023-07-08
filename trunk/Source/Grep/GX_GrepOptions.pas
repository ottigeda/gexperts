unit GX_GrepOptions;

{$I GX_CondDefine.inc}

interface

uses
  Windows,
  SysUtils,
  Classes,
  Controls,
  StdCtrls,
  Forms,
  Menus,
  ActnList,
  Actions,
  GX_BaseForm,
  GX_Experts;

type
  TfmGrepOptions = class(TfmBaseForm)
    btnOK: TButton;
    btnCancel: TButton;
    chkUseCurrentIdent: TCheckBox;
  private
    procedure SetData(_UseCurrentIdent: Boolean);
    procedure GetData(out _UseCurrentIdent: Boolean);
  protected
{$IFDEF GX_IDE_IS_HIDPI_AWARE}
    procedure ArrangeControls; override;
{$ENDIF}
  public
    class function Execute(_Owner: TWinControl;
      var _UseCurrentIdent: Boolean): Boolean;
    constructor Create(_Owner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  ComCtrls, Types, Registry,
  u_dzVclUtils, u_dzOsUtils,
  GX_GenericUtils, GX_IdeUtils, GX_GExperts, GX_ActionBroker, GX_OtaUtils;

{ TfmGrepOptions }

class function TfmGrepOptions.Execute(_Owner: TWinControl;
  var _UseCurrentIdent: Boolean): Boolean;
var
  frm: TfmGrepOptions;
begin
  frm := TfmGrepOptions.Create(_Owner);
  try
    TForm_CenterOn(frm, _Owner);
    frm.SetData(_UseCurrentIdent);
    Result := (frm.ShowModal = mrOk);
    if Result then begin
      frm.GetData(_UseCurrentIdent);
    end;
  finally
    FreeAndNil(frm);
  end;
end;

constructor TfmGrepOptions.Create(_Owner: TComponent);
begin
  inherited;

  InitDpiScaler;
end;

{$IFDEF GX_IDE_IS_HIDPI_AWARE}
procedure TfmGrepOptions.ArrangeControls;
begin
end;
{$ENDIF}

procedure TfmGrepOptions.GetData(out _UseCurrentIdent: Boolean);
begin
  _UseCurrentIdent := chkUseCurrentIdent.Checked;
end;

procedure TfmGrepOptions.SetData(_UseCurrentIdent: Boolean);
begin
  chkUseCurrentIdent.Checked := _UseCurrentIdent;
end;

end.
