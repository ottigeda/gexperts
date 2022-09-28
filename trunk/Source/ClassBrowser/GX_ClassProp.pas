unit GX_ClassProp;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils, StdCtrls, Controls, ComCtrls, Classes, Forms,
  GX_BaseForm, GX_MemoEscFix, GX_ClassMgr;

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
  private
    procedure SetData(_Info: TBrowseClassInfoCollection);
  protected
{$IFDEF IDE_IS_HIDPI_AWARE}
    procedure ArrangeControls; override;
{$ENDIF}
  public
    class procedure Execute(_Owner: TWinControl; _Info: TBrowseClassInfoCollection);
    constructor Create(_Owner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  u_dzVclUtils;

{ TfmClassProp }

class procedure TfmClassProp.Execute(_Owner: TWinControl; _Info: TBrowseClassInfoCollection);
var
  frm: TfmClassProp;
begin
  frm := TfmClassProp.Create(_Owner);
  try
    TForm_CenterOn(frm, _Owner);
    frm.SetData(_Info);
    frm.ShowModal;
  finally
    FreeAndNil(frm);
  end;
end;

constructor TfmClassProp.Create(_Owner: TComponent);
begin
  inherited;
  InitDpiScaler;
end;

procedure TfmClassProp.SetData(_Info: TBrowseClassInfoCollection);
begin
  edtClassName.Text := _Info.Name;
  edtDerivedFrom.Text := _Info.DerivedFrom;
  mmoFileName.Text := _Info.FileName;
  edtLineNo.Text := IntToStr(_Info.LineNo);
  edtUnit.Text := _Info.SourceName;
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
