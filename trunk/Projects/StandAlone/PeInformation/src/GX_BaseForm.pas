///<summary>
/// stripped down version of GX_BaseForm to be used in stand alone utitlities </summary>
unit GX_BaseForm;

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
  u_dzVclUtils;

type
  // All forms except docking forms must descend from this class.
  // Changes here must also be made to TfmIdeDockForm, since it must descend
  // from the IDE-internal TDockableForm class.
  TfmBaseForm = class(TForm)
  protected
    procedure InitDpiScaler;
  public
    class function Execute(_Owner: TComponent): Boolean; overload; virtual;
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  GX_GxUtils;

class function TfmBaseForm.Execute(_Owner: TComponent): Boolean;
var
  frm: TfmBaseForm;
begin
  frm := Self.Create(_Owner);
  try
    Result := (frm.ShowModal = mrOk);
  finally
    frm.Free;
  end;
end;

procedure TfmBaseForm.InitDpiScaler;
begin
end;

constructor TfmBaseForm.Create(AOwner: TComponent);
begin
  inherited;
  GxSetDefaultFont(Self);
end;

end.
