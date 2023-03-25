unit GX_MacroTemplateEdit;

interface

uses
  SysUtils, Classes, Dialogs, Controls, Forms, StdCtrls, ComCtrls,
  GX_MacroFile, GX_BaseForm;

type
  TMacroTemplate = record
    Name: string;
    Description: string;
    ShortCut: TShortCut;
    InsertPos: TTemplateInsertPos;
  end;

  TfmMacroTemplateEdit = class(TfmBaseForm)
    edtName: TEdit;
    edtDescription: TEdit;
    lblName: TLabel;
    lblDescription: TLabel;
    btnOK: TButton;
    btnCancel: TButton;
    lblShortCut: TLabel;
    lblInsertPos: TLabel;
    cbxInsertPos: TComboBox;
    edtShortCut: THotKey;
    procedure btnOKClick(Sender: TObject);
    procedure GetData(_MacroObject: TMacroObject);
    procedure SetData(_MacroObject: TMacroObject);
  public
    class function Execute(_MacroObject: TMacroObject): Boolean;
    constructor Create(_Owner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  u_dzVclUtils;

{ TfmMacroTemplateEdit }

class function TfmMacroTemplateEdit.Execute(_MacroObject: TMacroObject): Boolean;
var
  frm: TfmMacroTemplateEdit;
begin
  frm := TfmMacroTemplateEdit.Create(Application);
  try
    frm.SetData(_MacroObject);
    Result := (frm.ShowModal = mrOk);
    if Result then
      frm.GetData(_MacroObject);
  finally
    FreeAndNil(frm);
  end;
end;

constructor TfmMacroTemplateEdit.Create(_Owner: TComponent);
begin
  inherited;

  InitDpiScaler;
end;

procedure TfmMacroTemplateEdit.GetData(_MacroObject: TMacroObject);
begin
  _MacroObject.Name := edtName.Text;
  _MacroObject.Desc := edtDescription.Text;
  _MacroObject.ShortCut := edtShortCut.HotKey;
  _MacroObject.InsertPos := TTemplateInsertPos(cbxInsertPos.ItemIndex);
end;

procedure TfmMacroTemplateEdit.SetData(_MacroObject: TMacroObject);
begin
  edtName.Text := _MacroObject.Name;
  edtDescription.Text := _MacroObject.Desc;
  edtShortCut.HotKey := _MacroObject.ShortCut;
  cbxInsertPos.ItemIndex := Ord(_MacroObject.InsertPos);
end;

procedure TfmMacroTemplateEdit.btnOKClick(Sender: TObject);
resourcestring
  NoTemplateName = 'All templates require a name';
  InvalidIdentTemplateName = 'Template names must be valid identifiers (no white space or special characters)';
begin
  if edtDescription.Text = '' then
    edtDescription.Text := edtName.Text;
  if (edtName.Text = '') then
    MessageDlg(NoTemplateName, mtError, [mbOK], 0)
  else if (not IsValidIdent(edtName.Text)) then
    MessageDlg(InvalidIdentTemplateName, mtError, [mbOK], 0)
  else
    ModalResult := mrOk;
end;

end.

