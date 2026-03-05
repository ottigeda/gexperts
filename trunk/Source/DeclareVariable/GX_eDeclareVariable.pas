unit GX_eDeclareVariable;

{$I GX_CondDefine.inc}

interface

uses
  Classes, StdCtrls, Controls, Forms, ExtCtrls, GX_BaseForm,
  GX_DeclareVariable;

type
  TfmDeclareVariable = class(TfmBaseForm)
    lblIdentifier: TLabel;
    lblIdentifierValue: TLabel;
    lblExisting: TLabel;
    lblDeclareAs: TLabel;
    rbLocalVar: TRadioButton;
    rbField: TRadioButton;
    rbProperty: TRadioButton;
    lblType: TLabel;
    edtType: TEdit;
    lblInsertAt: TLabel;
    cmbInsertAt: TComboBox;
    lblRead: TLabel;
    edtRead: TEdit;
    lblWrite: TLabel;
    edtWrite: TEdit;
    lblWarning: TLabel;
    btnOK: TButton;
    btnCancel: TButton;
  private
    FInfo: TDeclareInfo;
    procedure rbDeclKindClick(Sender: TObject);
    procedure edtTypeChange(Sender: TObject);
    procedure UpdateInsertAtCombo;
    procedure UpdatePropertyFields;
    procedure ValidateInput;
    function GetSelectedKind: TDeclKind;
  public
    constructor Create(_Owner: TComponent); override;
    procedure SetInfo(const _Info: TDeclareInfo);
    function GetTypeName: string;
    function GetSelectedInsertionPoint: TInsertionPoint;
    function GetPropertyRead: string;
    function GetPropertyWrite: string;
  end;

implementation

{$R *.dfm}

uses
  SysUtils, Windows, Menus, ToolsAPI, Dialogs,
  GX_GenericUtils, GX_OtaUtils, GX_EditReader, GX_EditorExpert;

type
  TDeclareVariableExpert = class(TEditorExpert)
  public
    class function GetName: string; override;
    function GetDisplayName: string; override;
    function GetHelpString: string; override;
    function GetDefaultShortCut: TShortCut; override;
    function HasConfigOptions: Boolean; override;
    procedure Execute(Sender: TObject); override;
  end;

{ TfmDeclareVariable }

constructor TfmDeclareVariable.Create(_Owner: TComponent);
begin
  inherited;
  InitDpiScaler;
  rbLocalVar.OnClick := rbDeclKindClick;
  rbField.OnClick := rbDeclKindClick;
  rbProperty.OnClick := rbDeclKindClick;
  edtType.OnChange := edtTypeChange;
end;

procedure TfmDeclareVariable.SetInfo(const _Info: TDeclareInfo);
var
  I: Integer;
  HasLocalVar, HasField: Boolean;
begin
  FInfo := _Info;
  lblIdentifierValue.Caption := _Info.IdentName;

  if _Info.ExistingDecl <> '' then begin
    lblExisting.Caption := 'Already declared: ' + _Info.ExistingDecl;
    lblExisting.Visible := True;
  end else
    lblExisting.Visible := False;

  if _Info.IsReservedWord then begin
    lblWarning.Caption := '''' + _Info.IdentName + ''' is a reserved word';
    lblWarning.Visible := True;
    btnOK.Enabled := False;
    Exit;
  end;

  // Check which kinds are available
  HasLocalVar := False;
  HasField := False;
  for I := 0 to High(_Info.InsertionPoints) do begin
    if _Info.InsertionPoints[I].Kind = dkLocalVar then
      HasLocalVar := True;
    if _Info.InsertionPoints[I].Kind = dkField then
      HasField := True;
  end;

  rbLocalVar.Enabled := HasLocalVar;
  rbField.Enabled := HasField;
  rbProperty.Enabled := HasField;

  // Set default radio button
  if (_Info.DefaultKindIndex >= 0) and (_Info.DefaultKindIndex <= High(_Info.InsertionPoints)) then begin
    case _Info.InsertionPoints[_Info.DefaultKindIndex].Kind of
      dkLocalVar: rbLocalVar.Checked := True;
      dkField: rbField.Checked := True;
      dkProperty: rbProperty.Checked := True;
    end;
  end else if HasLocalVar then
    rbLocalVar.Checked := True
  else if HasField then
    rbField.Checked := True;

  edtType.Text := _Info.InferredType;
  UpdateInsertAtCombo;
  UpdatePropertyFields;
  ValidateInput;
end;

function TfmDeclareVariable.GetSelectedKind: TDeclKind;
begin
  if rbField.Checked then
    Result := dkField
  else if rbProperty.Checked then
    Result := dkProperty
  else
    Result := dkLocalVar;
end;

procedure TfmDeclareVariable.UpdateInsertAtCombo;
var
  I: Integer;
  Kind: TDeclKind;
begin
  cmbInsertAt.Items.Clear;
  Kind := GetSelectedKind;

  for I := 0 to High(FInfo.InsertionPoints) do begin
    if FInfo.InsertionPoints[I].Kind = Kind then
      cmbInsertAt.Items.AddObject(FInfo.InsertionPoints[I].Description, TObject(I))
    else if (Kind = dkProperty) and (FInfo.InsertionPoints[I].Kind = dkField) then
      cmbInsertAt.Items.AddObject(FInfo.InsertionPoints[I].Description, TObject(I));
  end;

  if cmbInsertAt.Items.Count > 0 then
    cmbInsertAt.ItemIndex := 0;
end;

procedure TfmDeclareVariable.UpdatePropertyFields;
var
  IsProperty: Boolean;
  FieldName: string;
begin
  IsProperty := rbProperty.Checked;
  lblRead.Visible := IsProperty;
  edtRead.Visible := IsProperty;
  lblWrite.Visible := IsProperty;
  edtWrite.Visible := IsProperty;

  if IsProperty then begin
    if (Length(FInfo.IdentName) > 1) and (UpCase(FInfo.IdentName[1]) = 'F')
       and (FInfo.IdentName[2] >= 'A') and (FInfo.IdentName[2] <= 'Z') then
      FieldName := FInfo.IdentName
    else
      FieldName := 'F' + FInfo.IdentName;
    if edtRead.Text = '' then
      edtRead.Text := FieldName;
    if edtWrite.Text = '' then
      edtWrite.Text := FieldName;
  end;
end;

procedure TfmDeclareVariable.rbDeclKindClick(Sender: TObject);
begin
  UpdateInsertAtCombo;
  UpdatePropertyFields;
  ValidateInput;
end;

procedure TfmDeclareVariable.edtTypeChange(Sender: TObject);
begin
  ValidateInput;
end;

procedure TfmDeclareVariable.ValidateInput;
begin
  btnOK.Enabled := (Trim(edtType.Text) <> '') and (cmbInsertAt.Items.Count > 0)
    and not FInfo.IsReservedWord;
end;

function TfmDeclareVariable.GetTypeName: string;
begin
  Result := Trim(edtType.Text);
end;

function TfmDeclareVariable.GetSelectedInsertionPoint: TInsertionPoint;
var
  Idx: Integer;
begin
  if cmbInsertAt.ItemIndex >= 0 then begin
    Idx := Integer(cmbInsertAt.Items.Objects[cmbInsertAt.ItemIndex]);
    Result := FInfo.InsertionPoints[Idx];
  end else begin
    // Fallback — should not happen when OK is enabled
    Result := FInfo.InsertionPoints[0];
  end;
end;

function TfmDeclareVariable.GetPropertyRead: string;
begin
  Result := Trim(edtRead.Text);
end;

function TfmDeclareVariable.GetPropertyWrite: string;
begin
  Result := Trim(edtWrite.Text);
end;

{ TDeclareVariableExpert }

class function TDeclareVariableExpert.GetName: string;
begin
  Result := 'DeclareVariable';
end;

function TDeclareVariableExpert.GetDisplayName: string;
begin
  Result := 'Declare Variable';
end;

function TDeclareVariableExpert.GetHelpString: string;
begin
  Result :=
    '  This expert declares the identifier at the cursor position ' +
    'as a local variable, field, or property.' + sLineBreak +
    '  It infers the type from context (assignments, constructor calls) ' +
    'and lets you choose where to insert the declaration.' + sLineBreak +
    '  The change is undoable with a single Ctrl+Z.';
end;

function TDeclareVariableExpert.GetDefaultShortCut: TShortCut;
begin
  Result := 0;
end;

function TDeclareVariableExpert.HasConfigOptions: Boolean;
begin
  Result := False;
end;

procedure TDeclareVariableExpert.Execute(Sender: TObject);
var
  SourceEditor: IOTASourceEditor;
  EditRead: TEditReader;
  Src: string;
  CharPos: TOTACharPos;
  CursorCharIdx: Integer;
  DeclInfo: TDeclareInfo;
  Dlg: TfmDeclareVariable;
  InsertPt: TInsertionPoint;
  DeclText, ModifiedSource: string;
  Step: string;
begin
  Step := 'GetSourceEditor';
  try
    if not Ota.TryGetCurrentSourceEditor(SourceEditor) then
      Exit;

    Step := 'ReadSource';
    EditRead := TEditReader.Create(SourceEditor.FileName);
    try
      Src := EditRead.GetText;
      CharPos := EditRead.GetCurrentCharPos;
    finally
      FreeAndNil(EditRead);
    end;

    Step := 'ConvertCursorPos';
    Src := AdjustLineBreaks(Src, tlbsCRLF);
    CursorCharIdx := LinePosToCharPos(Point(CharPos.CharIndex + 1, CharPos.Line), Src);

    Step := 'AnalyzeForDeclaration';
    if not AnalyzeForDeclaration(Src, CursorCharIdx, DeclInfo) then begin
      MessageBeep($FFFFFFFF);
      Exit;
    end;

    if DeclInfo.IsReservedWord then begin
      ShowError(DeclInfo.ErrorMsg);
      Exit;
    end;

    if Length(DeclInfo.InsertionPoints) = 0 then begin
      ShowError(DeclInfo.ErrorMsg);
      Exit;
    end;

    Step := 'ShowDialog';
    Dlg := TfmDeclareVariable.Create(nil);
    try
      SetFormIcon(Dlg);
      Dlg.SetInfo(DeclInfo);
      if Dlg.ShowModal <> mrOk then
        Exit;

      InsertPt := Dlg.GetSelectedInsertionPoint;
      DeclText := BuildDeclarationText(
        DeclInfo.IdentName,
        Dlg.GetTypeName,
        Dlg.GetSelectedKind,
        InsertPt.Indent,
        Dlg.GetPropertyRead,
        Dlg.GetPropertyWrite
      );
    finally
      FreeAndNil(Dlg);
    end;

    Step := 'InsertDeclaration';
    ModifiedSource := InsertDeclaration(Src, InsertPt, DeclText);

    Step := 'ReplaceEditorText';
    Ota.ReplaceEditorText(SourceEditor, ModifiedSource);
    IncCallCount;
  except
    on E: Exception do
      ShowError('Declare Variable error at step: ' + Step + sLineBreak
        + E.ClassName + ': ' + E.Message);
  end;
end;

initialization
  RegisterEditorExpert(TDeclareVariableExpert);

end.
