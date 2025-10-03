unit GX_ToDoOptions;

// Original Author: AJ Banck <ajbanck@davilex.nl>

interface

{$I GX_CondDefine.inc}

uses
  Classes, Controls, StdCtrls, Forms, GX_BaseForm, Dialogs;

type
  TfmToDoOptions = class(TfmBaseForm)
    btnOK: TButton;
    btnCancel: TButton;
    gbxTokens: TGroupBox;
    gbxOptions: TGroupBox;
    cbShowTokens: TCheckBox;
    cbAddMessage: TCheckBox;
    cbHideOnGoto: TCheckBox;
    lblPriority: TLabel;
    lblToken: TLabel;
    lstTokens: TListBox;
    btnInsert: TButton;
    btnApply: TButton;
    btnRemove: TButton;
    edToken: TEdit;
    cboPriority: TComboBox;
    gbxSearchFiles: TGroupBox;
    btnBrowse: TButton;
    chkInclude: TCheckBox;
    cboDirectories: TComboBox;
    radScanProj: TRadioButton;
    radScanOpen: TRadioButton;
    radScanDir: TRadioButton;
    radScanProjGroup: TRadioButton;
    TheFontDialog: TFontDialog;
    btnFont: TButton;
    procedure btnInsertClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure edTokenChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lstTokensClick(Sender: TObject);
    procedure cboPriorityChange(Sender: TObject);
    procedure radScanDirClick(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure btnFontClick(Sender: TObject);
  private
    procedure UpdateButtonState;
    procedure DirEnable(New: Boolean);
    procedure HandleDirectoriesDropped(_Sender: TObject; _Files: TStrings);
  public
    constructor Create(_Owner: TComponent); override;
  end;

// #ToDo:4 Test2

implementation

{$R *.dfm}

uses
  SysUtils, Graphics, u_dzVclUtils, GX_GenericUtils, GX_ToDo;

procedure TfmToDoOptions.UpdateButtonState;
var
  HasTokenText: Boolean;
  TokenTextInList: Boolean;
  IsListItemSelected: Boolean;
  TextIsCurrentListItem: Boolean;
  Idx: Integer;
begin
  HasTokenText := (edToken.Text <> '');
  TokenTextInList := (lstTokens.Items.IndexOf(edToken.Text) > -1);
  IsListItemSelected := (lstTokens.ItemIndex > -1);

  Idx := lstTokens.ItemIndex;
  TextIsCurrentListItem := IsListItemSelected and (edToken.Text = lstTokens.Items[Idx]);

  btnInsert.Enabled := HasTokenText and not TokenTextInList;
  btnRemove.Enabled := IsListItemSelected;
  btnApply.Enabled := HasTokenText and IsListItemSelected and TokenTextInList;

  if TextIsCurrentListItem then
    if (cboPriority.ItemIndex = Ord(TTokenInfo(lstTokens.Items.Objects[Idx]).Priority)) then
      btnApply.Enabled := False;
end;

procedure TfmToDoOptions.btnBrowseClick(Sender: TObject);
var
  Temp: string;
begin
  Temp := cboDirectories.Text;
  if GetDirectory(Temp) then
    cboDirectories.Text := Temp;
end;

procedure TfmToDoOptions.DirEnable(New: Boolean);
begin
  cboDirectories.Enabled := New;
  chkInclude.Enabled   := New;
  btnBrowse.Enabled    := New;
  if not New then
    cboDirectories.Color := clBtnFace
  else
    cboDirectories.Color := clWindow;
end;

procedure TfmToDoOptions.btnInsertClick(Sender: TObject);
resourcestring
  SLeadingDollarNotAllowed = 'A leading "$" character is not allowed in tokens, '+
                             'as this can conflict with Object Pascal compiler options.' + sLineBreak +
                             sLineBreak +
                             'Please choose a different token.';

  SEmptyTokenTextError = 'You cannot insert a token that only consists of white space.';
var
  TokenInfo: TTokenInfo;
  TokenString: string;
begin
  TokenString := Trim(edToken.Text);
  if TokenString <> '' then
  begin
    if TokenString[1] = '$' then
    begin
      MessageDlg(SLeadingDollarNotAllowed, mtError, [mbOk], 0);
      Exit;
    end;

    TokenInfo := TTokenInfo.Create;
    TokenInfo.Token := TokenString;
    TokenInfo.Priority := TTodoPriority(cboPriority.ItemIndex);
    lstTokens.Items.AddObject(TokenInfo.Token, TokenInfo);
  end
  else
  begin
    // Warning message that an empty token is inserted
    MessageDlg(SEmptyTokenTextError, mtError, [mbOK], 0);
  end;
  UpdateButtonState;
end;

procedure TfmToDoOptions.btnRemoveClick(Sender: TObject);
var
  idx: Integer;
begin
  idx := lstTokens.ItemIndex;
  if Idx > -1 then begin
    lstTokens.Items.Objects[Idx].Free;
    lstTokens.Items.Delete(Idx);
  end;
  UpdateButtonState;
end;

procedure TfmToDoOptions.btnFontClick(Sender: TObject);
begin
  TheFontDialog.Font := btnFont.Font;
  if TheFontDialog.Execute then
    btnFont.Font := TheFontDialog.Font;
end;

procedure TfmToDoOptions.btnApplyClick(Sender: TObject);
var
  TokenText: string;
  ti: TTokenInfo;
  Idx: Integer;
begin
  TokenText := edToken.Text;
  Idx := lstTokens.ItemIndex;
  if (Idx > -1) and (TokenText <> '')
    and Assigned(lstTokens.Items.Objects[Idx]) then begin
    lstTokens.Items[Idx] := TokenText;
    ti := TTokenInfo(lstTokens.Items.Objects[Idx]);
    ti.Token := TokenText;
    ti.Priority := TTodoPriority(cboPriority.ItemIndex);
  end;
  UpdateButtonState;
end;

procedure TfmToDoOptions.edTokenChange(Sender: TObject);
begin
  UpdateButtonState;
end;

procedure TfmToDoOptions.FormShow(Sender: TObject);
begin
  cboPriority.ItemIndex := 1;
  UpdateButtonState;
end;

procedure TfmToDoOptions.lstTokensClick(Sender: TObject);
var
  Idx: Integer;
begin
  UpdateButtonState;
  Idx := lstTokens.ItemIndex;
  if Idx > -1 then begin
    cboPriority.ItemIndex := Ord(TTokenInfo(lstTokens.Items.Objects[Idx]).Priority);
    edToken.Text := lstTokens.Items[Idx]
  end;
end;

procedure TfmToDoOptions.cboPriorityChange(Sender: TObject);
begin
  UpdateButtonState;
end;

constructor TfmToDoOptions.Create(_Owner: TComponent);
var
  i: TToDoPriority;
begin
  inherited;

  TWinControl_ActivateDropFiles(cboDirectories, HandleDirectoriesDropped);

  DirEnable(radScanDir.Checked);
  for i := Low(PriorityText) to High(PriorityText) do
    cboPriority.Items.Add(PriorityText[i]);

  InitDpiScaler;
end;

procedure TfmToDoOptions.HandleDirectoriesDropped(_Sender: TObject; _Files: TStrings);
begin
  cboDirectories.Text := _Files[0];
end;

procedure TfmToDoOptions.radScanDirClick(Sender: TObject);
begin
  DirEnable(radScanDir.Checked);
end;

end.
