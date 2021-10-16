unit GX_ClassProp;

{$I GX_CondDefine.inc}

interface

uses
  StdCtrls, Controls, ComCtrls, Classes, Forms, GX_BaseForm,
  GX_MemoEscFix;

type
  TMemo = class(TMemoEscFix)
  end;

type
  TfmClassProp = class(TfmBaseForm)
    pgeProperties: TPageControl;
    tabProps: TTabSheet;
    lblClassName: TLabel;
    edtClassName: TEdit;
    lblDerivedFrom: TLabel;
    edtDerivedFrom: TEdit;
    lblFileName: TLabel;
    lblLineNumber: TLabel;
    edtLineNo: TEdit;
    btnOK: TButton;
    mmoFileName: TMemo;
    lblUnit: TLabel;
    edtUnit: TEdit;
  protected
{$IFDEF IDE_IS_HIDPI_AWARE}
    procedure ArrangeControls; override;
{$ENDIF}
  public
    constructor Create(_Owner: TComponent); override;
  end;


implementation

{$R *.dfm}

uses
  u_dzVclUtils;

{ TfmClassProp }

constructor TfmClassProp.Create(_Owner: TComponent);
begin
  inherited;
  InitDpiScaler;
end;

{$IFDEF IDE_IS_HIDPI_AWARE}
procedure TfmClassProp.ArrangeControls;
begin
  inherited;
  TLabel_AlignVerticallyTo(lblClassName, edtClassName);
  TLabel_AlignVerticallyTo(lblDerivedFrom, edtDerivedFrom);
  TLabel_AlignVerticallyTo(lblUnit, edtUnit);
  TLabel_AlignVerticallyTo(lblLineNumber, edtLineNo);
end;
{$ENDIF}

end.
