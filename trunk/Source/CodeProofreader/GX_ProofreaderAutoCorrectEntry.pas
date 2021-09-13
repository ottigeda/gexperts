unit GX_ProofreaderAutoCorrectEntry;

interface

uses
  Classes, Controls, Forms, StdCtrls,
  GX_ProofreaderUtils, GX_BaseForm;

type
  TfmProofreaderAutoCorrectEntry = class(TfmBaseForm)
    lblReplaceWhat: TLabel;
    edtReplaceWhat: TEdit;
    btnOK: TButton;
    btnCancel: TButton;
    lblLocation: TLabel;
    lblReplaceWith: TLabel;
    edtReplaceWith: TEdit;
    cbxLocation: TComboBox;
    procedure EditBoxChange(Sender: TObject);
  private
    procedure UpdateButtons;
  public
    class function Execute(var TypedString: string; var ReplaceWhere: TGXWhereReplace;
      var ReplaceWithString: string; Add: Boolean): Boolean;
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  SysUtils,
  u_dzVclUtils;

{ TfmProofreaderDictionaryEntry }

class function TfmProofreaderAutoCorrectEntry.Execute(var TypedString: string;
  var ReplaceWhere: TGXWhereReplace; var ReplaceWithString: string; Add: Boolean): Boolean;
resourcestring
  SDlgCaptionAdd = 'Add AutoCorrect Entry';
  SDlgCaptionEdit = 'Edit AutoCorrect Entry';
var
  frm: TfmProofreaderAutoCorrectEntry;
begin
  frm := TfmProofreaderAutoCorrectEntry.Create(nil);
  try
    if Add then
    begin
      frm.Caption := SDlgCaptionAdd;
      frm.edtReplaceWhat.Text := '';
      frm.cbxLocation.ItemIndex := Ord(wrAnywhere);
      frm.edtReplaceWith.Text := '';
    end
    else
    begin
      frm.Caption := SDlgCaptionEdit;
      frm.edtReplaceWhat.Text := TypedString;
      frm.cbxLocation.ItemIndex := Ord(ReplaceWhere);
      frm.edtReplaceWith.Text := ReplaceWithString;
    end;

    frm.UpdateButtons;
    Result := frm.ShowModal = mrOk;
    if Result then
    begin
      Result := Trim(frm.edtReplaceWhat.Text) <> '';
      if Result then
      begin
        TypedString := frm.edtReplaceWhat.Text;
        ReplaceWhere := TGXWhereReplace(frm.cbxLocation.ItemIndex);
        ReplaceWithString := frm.edtReplaceWith.Text;
      end;
    end;
  finally
    FreeAndNil(frm);
  end;
end;

constructor TfmProofreaderAutoCorrectEntry.Create(AOwner: TComponent);
var
  WhereReplace: TGXWhereReplace;
begin
  inherited Create(AOwner);
  for WhereReplace := Low(TGXWhereReplace) to High(TGXWhereReplace) do
    cbxLocation.Items.Add(GXWhereReplaceStrings[WhereReplace]);
end;

procedure TfmProofreaderAutoCorrectEntry.UpdateButtons;
begin
  btnOK.Enabled := Trim(edtReplaceWhat.Text) <> ''; // Allow replacing with nothing
end;

procedure TfmProofreaderAutoCorrectEntry.EditBoxChange(Sender: TObject);
begin
  UpdateButtons;
end;

end.

