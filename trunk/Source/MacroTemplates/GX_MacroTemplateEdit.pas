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
  end;

function GetMacroTemplate(var VMacroTemplate: TMacroTemplate): Boolean;
function EditMacroObject(AMacroObject: TMacroObject): Boolean;

implementation

{$R *.dfm}

uses
  u_dzVclUtils;

function GetMacroTemplate(var VMacroTemplate: TMacroTemplate): Boolean;
var
  frm :TfmMacroTemplateEdit;
  Int: IInterface;
begin
  Result := False;
  // This buys (me) some time with adapting forms for High DPI by temporarily turning off
  // High DPI awareness. Works only for forms that are shown modally and don't
  // call into the IDE before closing.
  // All this is only necessary for Delphi 11 and later.
  // It does nothing for older Delphi versions.
  int := TemporarilyDisableHighDpi;
  frm := TfmMacroTemplateEdit.Create(Application);
  try
    frm.TemporarilyDisableHighDpiInterface := int;
    Int := nil;
    frm.edtName.Text := VMacroTemplate.Name;
    frm.edtDescription.Text := VMacroTemplate.Description;
    frm.edtShortCut.HotKey := VMacroTemplate.ShortCut;
    frm.cbxInsertPos.ItemIndex := Ord(VMacroTemplate.InsertPos);
    frm.ShowModal;
    if frm.ModalResult = mrOk then
    begin
      Result := True;
      VMacroTemplate.Name := frm.edtName.Text;
      VMacroTemplate.Description := frm.edtDescription.Text;
      VMacroTemplate.ShortCut := frm.edtShortCut.HotKey;
      VMacroTemplate.InsertPos := TTemplateInsertPos(frm.cbxInsertPos.ItemIndex);
    end;
  finally
    FreeAndNil(frm);
  end;
end;

function EditMacroObject(AMacroObject: TMacroObject): Boolean;
var
  frm :TfmMacroTemplateEdit;
  Int: IInterface;
begin
  Result := False;
  // This buys (me) some time with adapting forms for High DPI by temporarily turning off
  // High DPI awareness. Works only for forms that are shown modally and don't
  // call into the IDE before closing.
  // All this is only necessary for Delphi 11 and later.
  // It does nothing for older Delphi versions.
  int := TemporarilyDisableHighDpi;
  frm := TfmMacroTemplateEdit.Create(Application);
  try
    frm.TemporarilyDisableHighDpiInterface := int;
    Int := nil;
    frm.Text := AMacroObject.Name;
    frm.Text := AMacroObject.Desc;
    frm.edtShortCut.HotKey := AMacroObject.ShortCut;
    frm.cbxInsertPos.ItemIndex := Ord(AMacroObject.InsertPos);

    frm.ShowModal;
    if frm.ModalResult = mrOk then
    begin
      Result := True;
      AMacroObject.Name := frm.edtName.Text;
      AMacroObject.Desc := frm.edtDescription.Text;
      AMacroObject.ShortCut := frm.edtShortCut.HotKey;
      AMacroObject.InsertPos := TTemplateInsertPos(frm.cbxInsertPos.ItemIndex);
    end;
  finally
    FreeAndNil(frm);
  end;
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

