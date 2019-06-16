unit GX_ClassReport;

interface

uses
  SysUtils,
  Classes,
  StdCtrls,
  Controls,
  Forms,
  ComCtrls,
  GX_BaseForm;

type
  TfmClassReport = class(TfmBaseForm)
    gbxPageSettings: TGroupBox;
    lblFont: TLabel;
    lblFontSize: TLabel;
    lblBoxSize: TLabel;
    lblInCharacters: TLabel;
    btnOK: TButton;
    btnCancel: TButton;
    lblBoxSpacing: TLabel;
    lblInPixels: TLabel;
    cbxFont: TComboBox;
    spnFontSize: TEdit;
    spnBoxSize: TEdit;
    spnBoxSpacing: TEdit;
    udBoxSize: TUpDown;
    udBoxSpacing: TUpDown;
    udFontSize: TUpDown;
    procedure FormCreate(Sender: TObject);
  private
    procedure SetData(_FontSize, _BoxWidth, _BoxSpace: Integer; const _Font: string);
    procedure GetData(out _FontSize, _BoxWidth, _BoxSpace: Integer; out _Font: string);
  public
    class function Execute(_Owner: TWinControl; var _FontSize, _BoxWidth, _BoxSpace: Integer;
      var _Font: string): Boolean;
  end;

implementation

{$R *.dfm}

uses
  GX_dzVclUtils;

class function TfmClassReport.Execute(_Owner: TWinControl; var _FontSize, _BoxWidth,
  _BoxSpace: Integer; var _Font: string): Boolean;
var
  frm: TfmClassReport;
begin
  frm := TfmClassReport.Create(_Owner);
  try
    TForm_CenterOn(frm, _Owner);
    frm.SetData(_FontSize, _BoxWidth, _BoxSpace, _Font);
    Result := (frm.ShowModal = mrOk);
    if Result then
      frm.GetData(_FontSize, _BoxWidth, _BoxSpace, _Font);
  finally
    FreeAndNil(frm);
  end;
end;

procedure TfmClassReport.FormCreate(Sender: TObject);
begin
  cbxFont.Items.Assign(Screen.Fonts);
end;

procedure TfmClassReport.GetData(out _FontSize, _BoxWidth, _BoxSpace: Integer; out _Font: string);
begin
  _FontSize := udFontSize.Position;
  _BoxWidth := udBoxSize.Position;
  _BoxSpace := udBoxSpacing.Position;
  _Font := cbxFont.Text;
end;

procedure TfmClassReport.SetData(_FontSize, _BoxWidth, _BoxSpace: Integer; const _Font: string);
begin
  udFontSize.Position := _FontSize;
  udBoxSize.Position := _BoxWidth;
  udBoxSpacing.Position := _BoxSpace;
  cbxFont.ItemIndex := cbxFont.Items.IndexOf(_Font);
end;

end.
