unit GX_eDeclareVariable;

{$I GX_CondDefine.inc}

interface

uses
  Classes, Graphics, StdCtrls, Controls, Forms, ExtCtrls, GX_BaseForm,
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
    procedure UpdateLayout;
    function GetSelectedKind: TDeclKind;
    procedure SetData(const _Info: TDeclareInfo);
    procedure GetData(out _InsertPt: TInsertionPoint; out _DeclText: string);
  public
    constructor Create(_Owner: TComponent); override;
    class function Execute(_bmp: Graphics.TBitmap; const _Info: TDeclareInfo;
      out _InsertPt: TInsertionPoint; out _DeclText: string): Boolean; reintroduce;
  end;

implementation

{$R *.dfm}

uses
  SysUtils, Windows, Menus, ToolsAPI, Dialogs,
  GX_GenericUtils, GX_OtaUtils, GX_EditReader, GX_EditorExpert,
  mPasLex, mwPasParserTypes,
  GX_PascalTokenAnalyzer, GX_Experts,
  GX_GExperts, GX_UsesExpert, GX_UnitExportList,
  GX_UsesManager;

type
  /// Cached tokenization of an external .pas file's interface section
  TCachedUnitTokens = record
    FilePath: string;
    Tokens: TTokenArray;
    Classes: TClassScopeArray;
  end;
  TCachedUnitTokensArray = array of TCachedUnitTokens;

  TExternalTypeResolver = class
  private
    FIdentifiers: TUnitExportlist;
    FUsesUnits: TStringList;
    FSourceEditor: IOTASourceEditor;
    FCache: TCachedUnitTokensArray;
    FCacheCount: Integer;
    procedure InitUsesUnits;
    function IsUnitInUses(const UnitFileName: string): Boolean;
    function GetCachedTokens(const FilePath: string;
      out Tokens: TTokenArray; out Procs: TProcScopeArray;
      out Classes: TClassScopeArray): Boolean;
    function ParseFileForFuncReturn(const FilePath, FuncName: string): string;
    function ParseFileForParamType(const FilePath, FuncName: string;
      ParamIndex: Integer): string;
    function ParseFileForIdentType(const FilePath, IdentName: string): string;
  public
    constructor Create(Identifiers: TUnitExportlist;
      SourceEditor: IOTASourceEditor);
    destructor Destroy; override;
    function Lookup(const Name: string; Kind: TExternalLookupKind;
      ParamIndex: Integer): string;
  end;

  TDeclareVariableExpert = class(TEditorExpert)
  public
    class function GetName: string; override;
    function GetDisplayName: string; override;
    function GetHelpString: string; override;
    function GetDefaultShortCut: TShortCut; override;
    function HasConfigOptions: Boolean; override;
    procedure Execute(Sender: TObject); override;
  end;

{ TExternalTypeResolver }

constructor TExternalTypeResolver.Create(Identifiers: TUnitExportlist;
  SourceEditor: IOTASourceEditor);
begin
  inherited Create;
  FIdentifiers := Identifiers;
  FSourceEditor := SourceEditor;
  FUsesUnits := nil; // lazily initialized
  FCacheCount := 0;
  SetLength(FCache, 4);
end;

destructor TExternalTypeResolver.Destroy;
begin
  FreeAndNil(FUsesUnits);
  FCache := nil;
  inherited;
end;

procedure TExternalTypeResolver.InitUsesUnits;
var
  UsesMgr: TUsesManager;
  I: Integer;
  UnitName: string;
  UsesList: TUsesList;
begin
  if Assigned(FUsesUnits) then
    Exit;
  FUsesUnits := TStringList.Create;
  FUsesUnits.CaseSensitive := False;
  FUsesUnits.Duplicates := dupIgnore;
  FUsesUnits.Sorted := True;
  // System and SysUtils are always implicitly available
  FUsesUnits.Add('System');
  FUsesUnits.Add('SysUtils');
  try
    UsesMgr := TUsesManager.Create(FSourceEditor);
    try
      UsesList := UsesMgr.InterfaceUses;
      for I := 0 to UsesList.Count - 1 do begin
        UnitName := UsesList.Items[I].Name;
        // Handle dotted unit names: System.SysUtils -> SysUtils
        if Pos('.', UnitName) > 0 then
          FUsesUnits.Add(Copy(UnitName, LastDelimiter('.', UnitName) + 1, MaxInt));
        FUsesUnits.Add(UnitName);
      end;
      UsesList := UsesMgr.ImplementationUses;
      for I := 0 to UsesList.Count - 1 do begin
        UnitName := UsesList.Items[I].Name;
        if Pos('.', UnitName) > 0 then
          FUsesUnits.Add(Copy(UnitName, LastDelimiter('.', UnitName) + 1, MaxInt));
        FUsesUnits.Add(UnitName);
      end;
    finally
      FreeAndNil(UsesMgr);
    end;
  except
    // If uses clause parsing fails, allow all units
  end;
end;

function TExternalTypeResolver.IsUnitInUses(const UnitFileName: string): Boolean;
var
  UnitName: string;
begin
  InitUsesUnits;
  // Extract unit name from filename: "C:\Source\SysUtils.pas" -> "SysUtils"
  UnitName := ChangeFileExt(ExtractFileName(UnitFileName), '');
  Result := FUsesUnits.IndexOf(UnitName) >= 0;
end;

function TExternalTypeResolver.GetCachedTokens(const FilePath: string;
  out Tokens: TTokenArray; out Procs: TProcScopeArray;
  out Classes: TClassScopeArray): Boolean;
var
  I, ImplPos: Integer;
  Stream: TFileStream;
  Bytes: AnsiString;
  Source: string;
begin
  Result := False;

  // Check cache first
  for I := 0 to FCacheCount - 1 do begin
    if SameText(FCache[I].FilePath, FilePath) then begin
      Tokens := FCache[I].Tokens;
      Procs := nil;
      Classes := FCache[I].Classes;
      Result := Length(Tokens) > 0;
      Exit;
    end;
  end;

  // Read file
  if not FileExists(FilePath) then
    Exit;
  try
    Stream := TFileStream.Create(FilePath, fmOpenRead or fmShareDenyNone);
    try
      SetLength(Bytes, Stream.Size);
      if Stream.Size > 0 then
        Stream.ReadBuffer(Bytes[1], Stream.Size);
      Source := string(Bytes);
    finally
      Stream.Free;
    end;
  except
    Exit;
  end;
  if Source = '' then
    Exit;

  // Truncate at 'implementation' keyword to avoid parsing huge implementation sections.
  // Search for #10implementation followed by a non-identifier char (newline, space, CR).
  ImplPos := Pos(#10'implementation'#13, Source);
  if ImplPos = 0 then
    ImplPos := Pos(#10'implementation'#10, Source);
  if ImplPos = 0 then
    ImplPos := Pos(#10'implementation '#13, Source);
  if ImplPos > 0 then
    Source := Copy(Source, 1, ImplPos);

  Tokens := TokenizeSource(Source);
  // Only build class scopes (lightweight). Do NOT call BuildProcScopes here —
  // it causes O(N^2) behavior on interface-only code because ScanProcBody
  // treats every subsequent function as nested in the previous one.
  Classes := BuildClassScopes(Tokens);
  Procs := nil;

  // Store in cache
  if FCacheCount >= Length(FCache) then
    SetLength(FCache, Length(FCache) * 2);
  FCache[FCacheCount].FilePath := FilePath;
  FCache[FCacheCount].Tokens := Tokens;
  FCache[FCacheCount].Classes := Classes;
  Inc(FCacheCount);
  Result := Length(Tokens) > 0;
end;

/// Scans tokens for a function/procedure header matching FuncName and extracts
/// its return type.  Works directly on the token array without BuildProcScopes,
/// making it safe for interface-only code.
function ScanForFuncReturnType(const Tokens: TTokenArray;
  const FuncName: string): string;
var
  I, J, NI, ParenLevel: Integer;
begin
  Result := '';
  I := 0;
  while I <= High(Tokens) do begin
    if Tokens[I].Kind = tkFunction then begin
      NI := NextNonJunk(Tokens, I);
      if (NI <= High(Tokens)) and (Tokens[NI].Kind in IdentDirect) and
         SameText(Tokens[NI].Text, FuncName) then begin
        // Found matching function — skip past params to find ': ReturnType'
        J := NI + 1;
        ParenLevel := 0;
        while J <= High(Tokens) do begin
          if Tokens[J].Kind = tkRoundOpen then
            Inc(ParenLevel)
          else if Tokens[J].Kind = tkRoundClose then
            Dec(ParenLevel)
          else if (ParenLevel = 0) and (Tokens[J].Kind = tkColon) then begin
            Result := ExtractTypeAfterColon(Tokens, J);
            if Result <> '' then
              Exit;
            Break;
          end else if (ParenLevel = 0) and (Tokens[J].Kind = tkSemiColon) then
            Break;
          Inc(J);
        end;
      end;
    end;
    Inc(I);
  end;
end;

/// Scans tokens for a function/procedure header matching FuncName and extracts
/// the type of the parameter at the given 0-based index.
function ScanForParamType(const Tokens: TTokenArray;
  const FuncName: string; ParamIndex: Integer): string;
var
  I, J, NI, ParamCount, LastColon: Integer;
begin
  Result := '';
  I := 0;
  while I <= High(Tokens) do begin
    if IsProcKeyword(Tokens[I].Kind) then begin
      NI := NextNonJunk(Tokens, I);
      if (NI <= High(Tokens)) and (Tokens[NI].Kind in IdentDirect) and
         SameText(Tokens[NI].Text, FuncName) then begin
        // Find opening paren
        J := NI + 1;
        while (J <= High(Tokens)) and (Tokens[J].Kind <> tkRoundOpen) do begin
          if Tokens[J].Kind = tkSemiColon then
            Break;
          Inc(J);
        end;
        if (J > High(Tokens)) or (Tokens[J].Kind <> tkRoundOpen) then begin
          Inc(I);
          Continue;
        end;
        Inc(J); // skip '('
        ParamCount := 0;
        LastColon := -1;
        while J <= High(Tokens) do begin
          case Tokens[J].Kind of
            tkRoundClose: begin
              if (ParamCount = ParamIndex) and (LastColon >= 0) then
                Result := ExtractTypeAfterColon(Tokens, LastColon);
              Break;
            end;
            tkColon:
              LastColon := J;
            tkComma: begin
              if ParamCount = ParamIndex then begin
                if LastColon >= 0 then begin
                  Result := ExtractTypeAfterColon(Tokens, LastColon);
                  Exit;
                end;
              end;
              Inc(ParamCount);
            end;
            tkSemiColon: begin
              if ParamCount = ParamIndex then begin
                if LastColon >= 0 then begin
                  Result := ExtractTypeAfterColon(Tokens, LastColon);
                  Exit;
                end;
              end;
              Inc(ParamCount);
              LastColon := -1;
            end;
          end;
          Inc(J);
        end;
        if Result <> '' then
          Exit;
      end;
    end;
    Inc(I);
  end;
end;

function TExternalTypeResolver.ParseFileForFuncReturn(const FilePath, FuncName: string): string;
var
  Tokens: TTokenArray;
  Classes: TClassScopeArray;
  Procs: TProcScopeArray;
begin
  Result := '';
  if not GetCachedTokens(FilePath, Tokens, Procs, Classes) then
    Exit;
  Result := ScanForFuncReturnType(Tokens, FuncName);
end;

function TExternalTypeResolver.ParseFileForParamType(const FilePath, FuncName: string;
  ParamIndex: Integer): string;
var
  Tokens: TTokenArray;
  Classes: TClassScopeArray;
  Procs: TProcScopeArray;
begin
  Result := '';
  if not GetCachedTokens(FilePath, Tokens, Procs, Classes) then
    Exit;
  Result := ScanForParamType(Tokens, FuncName, ParamIndex);
end;

function TExternalTypeResolver.ParseFileForIdentType(const FilePath, IdentName: string): string;
var
  Tokens: TTokenArray;
  Classes: TClassScopeArray;
  Procs: TProcScopeArray;
begin
  Result := '';
  if not GetCachedTokens(FilePath, Tokens, Procs, Classes) then
    Exit;
  // Try enum value first
  Result := LookupEnumType(Tokens, IdentName);
  if Result <> '' then
    Exit;
  // Try typed constant
  Result := LookupConstType(Tokens, IdentName);
end;

function TExternalTypeResolver.Lookup(const Name: string; Kind: TExternalLookupKind;
  ParamIndex: Integer): string;
var
  Found: TList;
  I: Integer;
  Export: TUnitExport;
begin
  Result := '';
  if not Assigned(FIdentifiers) then
    Exit;

  Found := TList.Create;
  try
    if not FIdentifiers.FindExact(AnsiString(LowerCase(Name)), Found) then
      Exit;

    // Try each matching unit, prioritizing units in the uses clause
    for I := 0 to Found.Count - 1 do begin
      Export := TUnitExport(Found[I]);
      if not IsUnitInUses(Export.FileName) then
        Continue;

      case Kind of
        elkFuncReturn:
          Result := ParseFileForFuncReturn(Export.FileName, Name);
        elkParamType:
          Result := ParseFileForParamType(Export.FileName, Name, ParamIndex);
        elkIdentType:
          Result := ParseFileForIdentType(Export.FileName, Name);
      end;
      if Result <> '' then
        Exit;
    end;
  finally
    Found.Free;
  end;
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

procedure TfmDeclareVariable.UpdateLayout;
const
  Margin = 16;
  RowGap = 8;
  SectionGap = 12;
  ButtonHeight = 25;
  ButtonGap = 12;
var
  Y: Integer;
begin
  // Position all rows top-down based on visibility
  Y := Margin;

  // Row 1: Identifier
  lblIdentifier.Top := Y;
  lblIdentifierValue.Top := Y;
  Y := Y + lblIdentifierValue.Height + RowGap;

  // Row 2: Existing declaration warning (conditional)
  if lblExisting.Visible then begin
    lblExisting.Top := Y;
    Y := Y + lblExisting.Height + RowGap;
  end;

  // Row 3: Declare as (radio buttons)
  lblDeclareAs.Top := Y;
  rbLocalVar.Top := Y - 1;
  rbField.Top := Y - 1;
  rbProperty.Top := Y - 1;
  Y := Y + rbLocalVar.Height + SectionGap;

  // Row 4: Type
  lblType.Top := Y + 3;
  edtType.Top := Y;
  Y := Y + edtType.Height + RowGap;

  // Row 5: Insert at
  lblInsertAt.Top := Y + 3;
  cmbInsertAt.Top := Y;
  Y := Y + cmbInsertAt.Height + RowGap;

  // Row 6-7: Property read/write specifiers (conditional)
  if lblRead.Visible then begin
    lblRead.Top := Y + 3;
    edtRead.Top := Y;
    Y := Y + edtRead.Height + RowGap;
    lblWrite.Top := Y + 3;
    edtWrite.Top := Y;
    Y := Y + edtWrite.Height + RowGap;
  end;

  // Warning label (conditional)
  if lblWarning.Visible then begin
    Y := Y + SectionGap;
    lblWarning.Top := Y;
    Y := Y + lblWarning.Height;
  end;

  // Buttons at bottom
  Y := Y + ButtonGap;
  // Temporarily remove bottom anchors so we can resize without fighting them
  btnOK.Anchors := [akRight, akTop];
  btnCancel.Anchors := [akRight, akTop];
  btnOK.Top := Y;
  btnCancel.Top := Y;
  ClientHeight := Y + ButtonHeight + Margin;
  btnOK.Anchors := [akRight, akBottom];
  btnCancel.Anchors := [akRight, akBottom];
end;

procedure TfmDeclareVariable.SetData(const _Info: TDeclareInfo);
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
    UpdateLayout;
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
  UpdateLayout;
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

procedure TfmDeclareVariable.GetData(out _InsertPt: TInsertionPoint;
  out _DeclText: string);
var
  Idx: Integer;
begin
  if cmbInsertAt.ItemIndex >= 0 then begin
    Idx := Integer(cmbInsertAt.Items.Objects[cmbInsertAt.ItemIndex]);
    _InsertPt := FInfo.InsertionPoints[Idx];
  end else
    _InsertPt := FInfo.InsertionPoints[0];
  _DeclText := BuildDeclarationText(
    FInfo.IdentName,
    Trim(edtType.Text),
    GetSelectedKind,
    _InsertPt.Indent,
    Trim(edtRead.Text),
    Trim(edtWrite.Text)
  );
end;

class function TfmDeclareVariable.Execute(_bmp: Graphics.TBitmap; const _Info: TDeclareInfo;
  out _InsertPt: TInsertionPoint; out _DeclText: string): Boolean;
var
  Dlg: TfmDeclareVariable;
begin
  Dlg := Self.Create(nil);
  try
    ConvertBitmapToIcon(_bmp, Dlg.Icon);
    Dlg.SetData(_Info);
    Result := (Dlg.ShowModal = mrOk);
    if Result then
      Dlg.GetData(_InsertPt, _DeclText);
  finally
    FreeAndNil(Dlg);
  end;
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
  InsertPt: TInsertionPoint;
  DeclText, ModifiedSource: string;
  Step: string;
  GExpertsObj: TGExperts;
  UsesExpert: TGX_Expert;
  Identifiers: TUnitExportlist;
  Resolver: TExternalTypeResolver;
  ExternalLookup: TExternalTypeLookup;
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

    Step := 'SetupExternalLookup';
    Resolver := nil;
    ExternalLookup := nil;
    if TryGetGExpertsInst(GExpertsObj) then begin
      if GExpertsObj.FindExpert('UsesClauseMgr', UsesExpert) then begin
        if TUsesClauseMgrExpert(UsesExpert).TryGetIdentifiers(Identifiers) then begin
          Resolver := TExternalTypeResolver.Create(Identifiers, SourceEditor);
          ExternalLookup := Resolver.Lookup;
        end;
      end;
    end;

    try
      Step := 'AnalyzeForDeclaration';
      if not AnalyzeForDeclaration(Src, CursorCharIdx, DeclInfo, ExternalLookup) then begin
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
      if not TfmDeclareVariable.Execute(GetBitmap, DeclInfo, InsertPt, DeclText) then
        Exit;

      Step := 'InsertDeclaration';
      ModifiedSource := InsertDeclaration(Src, InsertPt, DeclText);

      Step := 'ReplaceEditorText';
      Ota.ReplaceEditorText(SourceEditor, ModifiedSource);
      IncCallCount;
    finally
      FreeAndNil(Resolver);
    end;
  except
    on E: Exception do
      ShowError('Declare Variable error at step: ' + Step + sLineBreak
        + E.ClassName + ': ' + E.Message);
  end;
end;

initialization
  RegisterEditorExpert(TDeclareVariableExpert);

end.
