// the code formatter configuration dialog
// Original Author:     Egbert van Nes (http://www.dow.wau.nl/aew/People/Egbert_van_Nes.html)
// Contributors:        Thomas Mueller (http://www.dummzeuch.de)
unit GX_CodeFormatterConfig;

{$I GX_CondDefine.inc}

interface

uses
  Windows,
  SysUtils,
  Classes,
  Graphics,
  Forms,
  Controls,
  StdCtrls,
  Buttons,
  ExtCtrls,
  ComCtrls,
  Dialogs,
  Grids,
  Menus,
  GX_CodeFormatterTypes,
  GX_CodeFormatterEngine,
  GX_CodeFormatterSettings;

type
  TStringGrid = class(Grids.TStringGrid)
  private
    FSpacingOptions: TStringList;
  protected
    function GetEditStyle(ACol: Integer; ARow: Integer): TEditStyle; override;
    function CreateEditor: TInplaceEdit; override;
    function CanEditModify: Boolean; override;
    function CanEditShow: Boolean; override;
    procedure OnGetSpacingOptions(ACol, ARow: Integer; Items: TStrings);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

type
  TfmCodeFormatterConfig = class(TForm)
    pc_Main: TPageControl;
    ts_Indent: TTabSheet;
    ts_Spacing: TTabSheet;
    ts_LineBreaks: TTabSheet;
    SpacePerIndentEdit: TEdit;
    SpacePerIndentUpDown: TUpDown;
    l_SpacesPerIndent: TLabel;
    ts_Capitalization: TTabSheet;
    chk_IndentBegin: TCheckBox;
    UpperCompDirectivesCheck: TCheckBox;
    BlankProcCheck: TCheckBox;
    BlankSubProcCheck: TCheckBox;
    chk_IndentTry: TCheckBox;
    FeedAfterSemiColonCheck: TCheckBox;
    FeedRoundBeginCombo: TComboBox;
    l_BeginStyle: TLabel;
    FeedAfterThenCheck: TCheckBox;
    FeedBeforeEndCheck: TCheckBox;
    UpperNumbersCheck: TCheckBox;
    chk_IndentTryElse: TCheckBox;
    l_Capitalize: TLabel;
    ts_Align: TTabSheet;
    OpenDialog: TOpenDialog;
    WrapLinesCheck: TCheckBox;
    l_WrapAtPosition: TLabel;
    WrapPositionEdit: TEdit;
    WrapPositionUpDown: TUpDown;
    FeedAfterVarCheck: TCheckBox;
    l_ReservedWords: TLabel;
    ReservedCaseCombo: TComboBox;
    l_StandardDirectives: TLabel;
    StandDirectivesCombo: TComboBox;
    IndentCommentsCheck: TCheckBox;
    IndentCompDirectivesCheck: TCheckBox;
    ts_Misc: TTabSheet;
    AlignCommentsCheck: TCheckBox;
    l_AlignComentsAtPosition: TLabel;
    AlignCommentPosEdit: TEdit;
    AlignCommentPosUpDown: TUpDown;
    AlignVarCheck: TCheckBox;
    l_AlignVarAtPosition: TLabel;
    AlignVarPosEdit: TEdit;
    AlignVarPosUpDown: TUpDown;
    FeedElseIfCheck: TCheckBox;
    NoFeedBeforeThenCheck: TCheckBox;
    NoIndentElseIfCheck: TCheckBox;
    chk_IndentCaseElse: TCheckBox;
    RemoveDoubleBlankCheck: TCheckBox;
    EditButton: TButton;
    ts_Preview: TTabSheet;
    m_PreviewBefore: TMemo;
    l_Before: TLabel;
    m_PreviewAfter: TMemo;
    l_After: TLabel;
    grid_Spacing: TStringGrid;
    FeedEachUnitCheck: TCheckBox;
    ExceptSingleCheck: TCheckBox;
    p_Botton: TPanel;
    b_Help: TButton;
    b_Ok: TButton;
    b_Cancel: TButton;
    chk_ShowDone: TCheckBox;
    pm_Extra: TPopupMenu;
    mi_ResetTo: TMenuItem;
    mi_ResetToDefault: TMenuItem;
    mi_Import: TMenuItem;
    mi_Export: TMenuItem;
    b_Tools: TButton;
    od_Import: TOpenDialog;
    sd_Export: TSaveDialog;
    grp_ExtraIndentBefore: TGroupBox;
    grp_AlwaysBreakLine: TGroupBox;
    grp_ForceBlankLineBetween: TGroupBox;
    l_TryStyle: TLabel;
    FeedRoundTryCombo: TComboBox;
    rb_CapitalizationInRegistry: TRadioButton;
    rb_CapitalizationInFile: TRadioButton;
    ed_CapitalizationFile: TEdit;
    b_CapitalizationSelect: TButton;
    rg_Capitalization: TRadioGroup;
    od_CapitalizationFile: TOpenDialog;
    grp_ConfigPrecedence: TGroupBox;
    grp_DirectivesPreventFormatting: TGroupBox;
    l_MiscStart: TLabel;
    l_MiscEnd: TLabel;
    StartCommentOutEdit: TEdit;
    EndCommentOutEdit: TEdit;
    lb_Precedence: TListBox;
    b_PrecedenceUp: TButton;
    b_PrecedenceDown: TButton;
    procedure b_HelpClick(Sender: TObject);
    procedure EditButtonClick(Sender: TObject);
    procedure ts_PreviewShow(Sender: TObject);
    procedure m_PreviewBeforeClick(Sender: TObject);
    procedure m_PreviewBeforeKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure m_PreviewBeforeKeyPress(Sender: TObject; var Key: Char);
    procedure m_PreviewBeforeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure FeedAfterThenCheckClick(Sender: TObject);
    procedure ts_PreviewResize(Sender: TObject);
    procedure b_ToolsClick(Sender: TObject);
    procedure mi_ResetToDefaultClick(Sender: TObject);
    procedure mi_ImportClick(Sender: TObject);
    procedure mi_ExportClick(Sender: TObject);
    procedure rb_CapitalizationInRegistryClick(Sender: TObject);
    procedure rb_CapitalizationInFileClick(Sender: TObject);
    procedure b_CapitalizationSelectClick(Sender: TObject);
    procedure lb_PrecedenceClick(Sender: TObject);
    procedure b_PrecedenceUpClick(Sender: TObject);
    procedure b_PrecedenceDownClick(Sender: TObject);
  private
    FCapitalization: TStringList;
    procedure EngineSettingsToForm(const AEngineSettings: TCodeFormatterEngineSettings);
    procedure SettingsToForm(const ASettings: TCodeFormatterSettings);
    procedure FormToEngineSettings(var ASettings: TCodeFormatterEngineSettings);
    procedure FormToSettings(ASettings: TCodeFormatterSettings);
    procedure FillPreview;
    procedure AddSpaceRow(RowNo: Integer; StrCol1, StrCol2: string;
      Space: TSpaceSet);
    function GetSpaceItem(i: Integer): TSpaceSet;
    procedure SetDefault(AWhich: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function Execute(ASettings: TCodeFormatterSettings): TModalResult;
  end;

implementation

{$R *.DFM}

uses
  Messages,
  GX_CodeFormatterConfigHandler,
  GX_CodeFormatterEditCapitalization,
  GX_CodeFormatterDefaultSettings;

resourcestring
  str_None = 'None';
  str_Before = 'Before only';
  str_after = 'After only';
  str_BeforeAfter = 'Before and after';
  str_DefaultSettings = '<default>';
  str_PrecedenceDirective = 'GXFormatter.config directive';
  str_PrecedenceIniFile = 'GXFormatter.ini file';
  str_PrecedenceMySettings = 'my settings as configured here';

constructor TfmCodeFormatterConfig.Create(AOwner: TComponent);
var
  st: TStringList;
  i: Integer;
  mi: TMenuItem;
begin
  inherited;
  FCapitalization := TStringList.Create;
  st := TStringList.Create;
  try
    TCodeFormatterConfigHandler.GetDefaultsList(st);
    for i := 0 to st.Count - 1 do begin
      mi := TMenuItem.Create(Self);
      mi.Caption := st[i];
      mi.OnClick := mi_ResetToDefaultClick;
      mi_ResetTo.Add(mi);
    end;
  finally
    st.Free;
  end;

  grid_Spacing.DefaultRowHeight := grid_Spacing.Canvas.TextHeight('Mg') + 4;

  lb_Precedence.Items.AddObject(str_PrecedenceDirective, pointer(cpDirective));
  lb_Precedence.Items.AddObject(str_PrecedenceIniFile, pointer(cpIniFile));
  lb_Precedence.Items.AddObject(str_PrecedenceMySettings, pointer(cpMyConfig));
end;

destructor TfmCodeFormatterConfig.Destroy;
begin
  FCapitalization.Free;
  inherited;
end;

function TfmCodeFormatterConfig.GetSpaceItem(i: Integer): TSpaceSet;
var
  s: string;
begin
  s := grid_Spacing.Cells[2, i];
  Result := spNone;
  if s = str_Before then
    Result := [spBefore]
  else if s = str_after then
    Result := [spAfter]
  else if s = str_BeforeAfter then
    Result := spBoth;
end;

procedure TfmCodeFormatterConfig.lb_PrecedenceClick(Sender: TObject);
var
  Idx: integer;
begin
  Idx := lb_Precedence.ItemIndex;
  b_PrecedenceUp.Enabled := (Idx <> 0);
  b_PrecedenceDown.Enabled := (Idx <> 2);
end;

procedure TfmCodeFormatterConfig.b_PrecedenceUpClick(Sender: TObject);
var
  Idx: integer;
begin
  Idx := lb_Precedence.ItemIndex;
  if Idx = 0 then
    Exit;
  lb_Precedence.Items.Exchange(Idx, Idx - 1);
  lb_PrecedenceClick(lb_Precedence);
end;

procedure TfmCodeFormatterConfig.b_PrecedenceDownClick(Sender: TObject);
var
  Idx: integer;
begin
  Idx := lb_Precedence.ItemIndex;
  if Idx = 2 then
    Exit;
  lb_Precedence.Items.Exchange(Idx, Idx + 1);
  lb_PrecedenceClick(lb_Precedence);
end;

procedure TfmCodeFormatterConfig.AddSpaceRow(RowNo: Integer; StrCol1, StrCol2: string;
  Space: TSpaceSet);

  procedure SetColText(_Col: Integer; const _s: string; _Offset: Integer = 4);
  var
    w: Integer;
  begin
    grid_Spacing.Cells[_Col, RowNo] := _s;
    w := grid_Spacing.Canvas.TextWidth(_s) + _Offset;
    if grid_Spacing.ColWidths[_Col] < w then
      grid_Spacing.ColWidths[_Col] := w;
  end;

begin
  SetColText(0, StrCol1);
  SetColText(1, StrCol2);

  if Space = spNone then
    SetColText(2, str_None, 40)
  else if Space = spBoth then
    SetColText(2, str_BeforeAfter, 40)
  else if spBefore in Space then
    SetColText(2, str_Before, 40)
  else
    SetColText(2, str_after, 40);
end;

function GetModuleDir: string;
begin
  Result := ExtractFilePath(GetModuleName(HInstance));
end;

procedure TfmCodeFormatterConfig.FillPreview;
var
  s: string;
begin
  s := IncludeTrailingPathDelimiter(GetModuleDir) + 'preview.pas';
  if FileExists(s) then begin
    m_PreviewBefore.Clear;
    m_PreviewBefore.Lines.LoadFromFile(s);
  end
end;

procedure TfmCodeFormatterConfig.FormToEngineSettings(var ASettings: TCodeFormatterEngineSettings);
begin
  ASettings := BorlandDefaults;

  ASettings.SpacePerIndent := SpacePerIndentUpDown.Position;
  ASettings.IndentBegin := chk_IndentBegin.Checked;
  ASettings.IndentComments := IndentCommentsCheck.Checked;
  ASettings.IndentCompDirectives := IndentCompDirectivesCheck.Checked;
  ASettings.IndentTry := chk_IndentTry.Checked;
  ASettings.IndentTryElse := chk_IndentTryElse.Checked;
  ASettings.IndentCaseElse := chk_IndentCaseElse.Checked;
  ASettings.UpperCompDirectives := UpperCompDirectivesCheck.Checked;
  ASettings.UpperNumbers := UpperNumbersCheck.Checked;
  ASettings.ReservedCase := TCase(ReservedCaseCombo.ItemIndex);
  ASettings.StandDirectivesCase := TCase(StandDirectivesCombo.ItemIndex);
  ASettings.BlankProc := BlankProcCheck.Checked;
  ASettings.BlankSubProc := BlankSubProcCheck.Checked;
  ASettings.RemoveDoubleBlank := RemoveDoubleBlankCheck.Checked;
  ASettings.WrapLines := WrapLinesCheck.Checked;
  ASettings.WrapPosition := WrapPositionUpDown.Position;
  ASettings.AlignComments := AlignCommentsCheck.Checked;
  ASettings.AlignCommentPos := AlignCommentPosUpDown.Position;
  ASettings.AlignVar := AlignVarCheck.Checked;
  ASettings.AlignVarPos := AlignVarPosUpDown.Position;
  ASettings.SpaceEqualOper := GetSpaceItem(1);
  ASettings.SpaceOperators := GetSpaceItem(2);
  ASettings.SpaceColon := GetSpaceItem(3);
  ASettings.SpaceSemiColon := GetSpaceItem(4);
  ASettings.SpaceComma := GetSpaceItem(5);
  ASettings.SpaceLeftBr := GetSpaceItem(6);
  ASettings.SpaceRightBr := GetSpaceItem(7);
  ASettings.SpaceLeftHook := GetSpaceItem(8);
  ASettings.SpaceRightHook := GetSpaceItem(9);
  ASettings.FeedAfterThen := FeedAfterThenCheck.Checked;
  ASettings.ExceptSingle := ExceptSingleCheck.Checked;
  ASettings.FeedEachUnit := FeedEachUnitCheck.Checked;
  ASettings.NoFeedBeforeThen := NoFeedBeforeThenCheck.Checked;
  ASettings.FeedAfterVar := FeedAfterVarCheck.Checked;
  ASettings.FeedElseIf := FeedElseIfCheck.Checked;
  ASettings.NoIndentElseIf := NoIndentElseIfCheck.Checked;
  ASettings.FeedBeforeEnd := FeedBeforeEndCheck.Checked;
  ASettings.FeedAfterSemiColon := FeedAfterSemiColonCheck.Checked;
  ASettings.FillNewWords := IntToCapfileMode(rg_Capitalization.ItemIndex);
  ASettings.StartCommentOut := Trim(StartCommentOutEdit.Text);
  ASettings.EndCommentOut := Trim(EndCommentOutEdit.Text);
  ASettings.FeedRoundBegin := TFeedBegin(FeedRoundBeginCombo.ItemIndex);
  ASettings.FeedRoundTry := TFeedBegin(FeedRoundTryCombo.ItemIndex);
end;

procedure TfmCodeFormatterConfig.FormToSettings(ASettings: TCodeFormatterSettings);
var
  Settings: TCodeFormatterEngineSettings;
  i: Integer;
  Idx: integer;
begin
  ASettings.CapNames.Assign(FCapitalization);
  ASettings.ShowDoneDialog := chk_ShowDone.Checked;
  ASettings.UseCapitalizationFile := rb_CapitalizationInFile.Checked;
  ASettings.CapitalizationFile := ed_CapitalizationFile.Text;

  for i := Low(TOneToThree) to High(TOneToThree) do begin
    Idx := i - Low(TOneToThree);
    ASettings.ConfigPrecedence[i] := TConfigPrecedenceEnum(lb_Precedence.Items.Objects[Idx])
  end;

  FormToEngineSettings(Settings);
  ASettings.Settings := Settings;
end;

procedure TfmCodeFormatterConfig.EngineSettingsToForm(const AEngineSettings: TCodeFormatterEngineSettings);
resourcestring
  str_Description = 'Description';
  str_Operators = 'Operators';
  str_Spacing = 'Spacing';
  str_Equals = 'Equals';
  str_MathOperators = 'Math. operators';
  str_MathOperatorsExample = '< > = + - / * etc.';
  str_Colon = 'Colon';
  str_SemiColon = 'Semicolon';
  str_Comma = 'Comma';
  str_LeftParenthesis = 'Left parenthesis';
  str_RightParenthesis = 'Right parenthesis';
  str_LeftBracket = 'Left bracket';
  str_RightBracket = 'Right bracket';
begin
  SpacePerIndentUpDown.Position := AEngineSettings.SpacePerIndent;
  chk_IndentBegin.Checked := AEngineSettings.IndentBegin;
  IndentCommentsCheck.Checked := AEngineSettings.IndentComments;
  IndentCompDirectivesCheck.Checked := AEngineSettings.IndentCompDirectives;
  chk_IndentTry.Checked := AEngineSettings.IndentTry;
  chk_IndentTryElse.Checked := AEngineSettings.IndentTryElse;
  chk_IndentCaseElse.Checked := AEngineSettings.IndentCaseElse;
  UpperCompDirectivesCheck.Checked := AEngineSettings.UpperCompDirectives;
  UpperNumbersCheck.Checked := AEngineSettings.UpperNumbers;
  ReservedCaseCombo.ItemIndex := Byte(AEngineSettings.ReservedCase);
  StandDirectivesCombo.ItemIndex := Byte(AEngineSettings.StandDirectivesCase);
  BlankProcCheck.Checked := AEngineSettings.BlankProc;
  BlankSubProcCheck.Checked := AEngineSettings.BlankSubProc;
  RemoveDoubleBlankCheck.Checked := AEngineSettings.RemoveDoubleBlank;
  with grid_Spacing do begin
    RowCount := 10;
    Cells[0, 0] := str_Description;
    Cells[1, 0] := str_Operators;
    Cells[2, 0] := str_Spacing;
    AddSpaceRow(1, str_Equals, ':=', AEngineSettings.SpaceEqualOper);
    AddSpaceRow(2, str_MathOperators, str_MathOperatorsExample,
      AEngineSettings.SpaceOperators);
    AddSpaceRow(3, str_Colon, ':', AEngineSettings.SpaceColon);
    AddSpaceRow(4, str_SemiColon, ';', AEngineSettings.SpaceSemiColon);
    AddSpaceRow(5, str_Comma, ',', AEngineSettings.SpaceComma);
    AddSpaceRow(6, str_LeftParenthesis, '(', AEngineSettings.SpaceLeftBr);
    AddSpaceRow(7, str_RightParenthesis, ')', AEngineSettings.SpaceRightBr);
    AddSpaceRow(8, str_LeftBracket, '[', AEngineSettings.SpaceLeftHook);
    AddSpaceRow(9, str_RightBracket, ']', AEngineSettings.SpaceRightHook);
  end;
  FeedAfterSemiColonCheck.Checked := AEngineSettings.FeedAfterSemiColon;
  FeedEachUnitCheck.Checked := AEngineSettings.FeedEachUnit;
  FeedAfterThenCheck.Checked := AEngineSettings.FeedAfterThen;
  ExceptSingleCheck.Checked := AEngineSettings.ExceptSingle;
  NoFeedBeforeThenCheck.Checked := AEngineSettings.NoFeedBeforeThen;
  FeedAfterVarCheck.Checked := AEngineSettings.FeedAfterVar;
  FeedElseIfCheck.Checked := AEngineSettings.FeedElseIf;
  NoIndentElseIfCheck.Checked := AEngineSettings.NoIndentElseIf;
  FeedBeforeEndCheck.Checked := AEngineSettings.FeedBeforeEnd;
  WrapLinesCheck.Checked := AEngineSettings.WrapLines;
  WrapPositionUpDown.Position := AEngineSettings.WrapPosition;
  AlignCommentsCheck.Checked := AEngineSettings.AlignComments;
  AlignCommentPosUpDown.Position := AEngineSettings.AlignCommentPos;
  AlignVarCheck.Checked := AEngineSettings.AlignVar;
  AlignVarPosUpDown.Position := AEngineSettings.AlignVarPos;
  rg_Capitalization.ItemIndex := CapfileModeToInt(AEngineSettings.FillNewWords);
  StartCommentOutEdit.Text := string(AEngineSettings.StartCommentOut);
  EndCommentOutEdit.Text := string(AEngineSettings.EndCommentOut);
  FeedRoundBeginCombo.ItemIndex := Integer(AEngineSettings.FeedRoundBegin);
  FeedRoundTryCombo.ItemIndex := Integer(AEngineSettings.FeedRoundTry);
  SpacePerIndentUpDown.Associate := SpacePerIndentEdit;
  WrapPositionUpDown.Associate := WrapPositionEdit;
  AlignCommentPosUpDown.Associate := AlignCommentPosEdit;
  AlignVarPosUpDown.Associate := AlignVarPosEdit;
  FeedAfterThenCheckClick(nil)
end;

procedure TfmCodeFormatterConfig.SettingsToForm(const ASettings: TCodeFormatterSettings);

  procedure AddPrecedenceSetting(_cp: TConfigPrecedenceEnum);
  begin
    case _cp of
      cpDirective: lb_Precedence.Items.AddObject(str_PrecedenceDirective, pointer(cpDirective));
      cpIniFile: lb_Precedence.Items.AddObject(str_PrecedenceIniFile, pointer(cpIniFile));
      cpMyConfig: lb_Precedence.Items.AddObject(str_PrecedenceMySettings, pointer(cpMyConfig));
    end;
  end;

var
  i: integer;
  cp: TConfigPrecedenceEnum;
  PrecedenceSet: set of TConfigPrecedenceEnum;
begin
  chk_ShowDone.Checked := ASettings.ShowDoneDialog;
  rb_CapitalizationInFile.Checked := ASettings.UseCapitalizationFile;
  ed_CapitalizationFile.Text := ASettings.CapitalizationFile;
  FCapitalization.Assign(ASettings.CapNames);

  lb_Precedence.Items.Clear;
  // the set is used to prevent manipulated settings from crashing the program
  PrecedenceSet := [cpDirective, cpIniFile, cpMyConfig];
  for i := Low(TOneToThree) to High(TOneToThree) do begin
    if ASettings.ConfigPrecedence[i] in PrecedenceSet then begin
      AddPrecedenceSetting(ASettings.ConfigPrecedence[i]);
      Exclude(PrecedenceSet, ASettings.ConfigPrecedence[i]);
    end;
  end;
  for cp := Low(TConfigPrecedenceEnum) to High(TConfigPrecedenceEnum) do begin
    if cp in PrecedenceSet then
      AddPrecedenceSetting(cp);
  end;

  EngineSettingsToForm(ASettings.Settings);
end;

procedure TfmCodeFormatterConfig.SetDefault(AWhich: string);
resourcestring
  str_CouldNotReadS = 'Could not read default configuration %s.';
var
  Defaults: TCodeFormatterSettings;
begin
  AWhich := StringReplace(AWhich, '&', '', [rfReplaceAll]);
  grid_Spacing.EditorMode := False;
  Defaults := TCodeFormatterSettings.Create;
  try
    if AWhich <> str_DefaultSettings then begin
      if not TCodeFormatterConfigHandler.GetDefaultConfig(AWhich, Defaults) then begin
        MessageDlg(Format(str_CouldNotReadS, [AWhich]), mtError, [mbOK], 0);
        Exit;
      end;
    end;
    SettingsToForm(Defaults);
  finally
    Defaults.Free;
  end;
  if pc_Main.ActivePage = ts_Preview then
    ts_PreviewShow(nil);
end;

procedure TfmCodeFormatterConfig.b_CapitalizationSelectClick(Sender: TObject);
var
  s: string;
begin
  od_CapitalizationFile.FileName := ed_CapitalizationFile.Text;
  if not od_CapitalizationFile.Execute then
    Exit;

  s := od_CapitalizationFile.FileName;
  if FileExists(s) then begin
    if FCapitalization.Count > 0 then begin
      if mrYes <> MessageDlg(
        'Your current capitalization list is not empty and the file already exists.'#13#10
        + 'If you continue, your list will be discarded and the selected file loaded instead.'#13#10
        + 'Continue?', mtWarning, [mbYes, mbNo], 0) then
        Exit;
    end;
    FCapitalization.LoadFromFile(s);
  end else begin
    if FCapitalization.Count > 0 then begin
      FCapitalization.SaveToFile(s);
    end;
  end;

  ed_CapitalizationFile.Text := s;
end;

procedure TfmCodeFormatterConfig.b_HelpClick(Sender: TObject);
var
  HlpFile: string;
begin
  { TODO : replace with GExperts help (and add contents of DelFor help to GExperts help) }
  HlpFile := GetModuleDir + 'delfor.hlp';
  WinHelp(0, PChar(HlpFile), HELP_KEY, Integer(pc_Main.ActivePage.Caption));
end;

procedure TfmCodeFormatterConfig.EditButtonClick(Sender: TObject);
var
  FileEditDlg: TfmCodeFormatterEditCapitalization;
  Cur: TCursor;
begin
  Cur := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  FileEditDlg := TfmCodeFormatterEditCapitalization.Create(Self);
  try
    FileEditDlg.ListToForm(FCapitalization);
    Screen.Cursor := Cur;
    if mrOk = FileEditDlg.ShowModal then begin
      if FileEditDlg.IsChanged then
        FileEditDlg.FormToList(FCapitalization);
    end;
  finally
    Screen.Cursor := Cur;
    FileEditDlg.Free;
  end;
end;

procedure TfmCodeFormatterConfig.ts_PreviewShow(Sender: TObject);
var
  Formatter: TCodeFormatterEngine;
  st: TStringList;
begin
  st := nil;
  Formatter := TCodeFormatterEngine.Create;
  try
    // this temporary string list is necessary to prevent an infinite loop (whose reason I don't really understand :-( )
    st := TStringList.Create;
    st.Assign(m_PreviewBefore.Lines);
    FormToSettings(Formatter.Settings);
    Formatter.Execute(st);
    m_PreviewAfter.Lines.BeginUpdate;
    m_PreviewAfter.Lines.Assign(st);
    m_PreviewAfter.Lines.EndUpdate;
    m_PreviewBeforeClick(nil);
  finally
    Formatter.Free;
    st.Free;
  end;
end;

procedure TfmCodeFormatterConfig.m_PreviewBeforeClick(Sender: TObject);
var
  CurLine2, CurLine: Integer;
begin
  CurLine := SendMessage(m_PreviewBefore.Handle, EM_GETFIRSTVISIBLELINE, 0, 0);
  CurLine2 := SendMessage(m_PreviewAfter.Handle, EM_GETFIRSTVISIBLELINE, 0, 0);
  SendMessage(m_PreviewAfter.Handle, EM_LINESCROLL, 0, CurLine - CurLine2);
  m_PreviewAfter.SelStart := m_PreviewBefore.SelStart;
end;

procedure TfmCodeFormatterConfig.m_PreviewBeforeKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  m_PreviewBeforeClick(nil);
end;

procedure TfmCodeFormatterConfig.m_PreviewBeforeKeyPress(Sender: TObject; var Key: Char);
begin
  m_PreviewBeforeClick(nil);
end;

procedure TfmCodeFormatterConfig.m_PreviewBeforeMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  m_PreviewBeforeClick(nil);
end;

procedure TfmCodeFormatterConfig.rb_CapitalizationInFileClick(Sender: TObject);
begin
  ed_CapitalizationFile.Enabled := True;
  b_CapitalizationSelect.Enabled := True;
end;

procedure TfmCodeFormatterConfig.rb_CapitalizationInRegistryClick(Sender: TObject);
begin
  ed_CapitalizationFile.Enabled := False;
  b_CapitalizationSelect.Enabled := False;
end;

procedure TfmCodeFormatterConfig.FormShow(Sender: TObject);
begin
  FillPreview;
  pc_Main.ActivePage := ts_Indent;
end;

procedure TfmCodeFormatterConfig.FeedAfterThenCheckClick(Sender: TObject);
begin
  ExceptSingleCheck.Enabled := FeedAfterThenCheck.Checked;
end;

procedure TfmCodeFormatterConfig.ts_PreviewResize(Sender: TObject);
var
  w: Integer;
begin
  w := (ts_Preview.ClientWidth - 16) div 2;
  m_PreviewBefore.Left := 8;
  l_Before.Left := 8;
  m_PreviewBefore.Width := w;
  m_PreviewBefore.Height := ts_Preview.ClientHeight - m_PreviewBefore.Top - 8;
  l_After.Left := w + 9;
  m_PreviewAfter.Left := w + 9;
  m_PreviewAfter.Width := w;
  m_PreviewAfter.Height := ts_Preview.ClientHeight - m_PreviewAfter.Top - 8;
end;

procedure TfmCodeFormatterConfig.b_ToolsClick(Sender: TObject);
var
  Point: TPoint;
begin
  Point.X := b_Tools.Width;
  Point.Y := 0;
  Point := b_Tools.ClientToScreen(Point);
  pm_Extra.Popup(Point.X, Point.Y);
end;

procedure TfmCodeFormatterConfig.mi_ResetToDefaultClick(Sender: TObject);
begin
  SetDefault(TMenuItem(Sender).Caption);
end;

procedure TfmCodeFormatterConfig.mi_ImportClick(Sender: TObject);
var
  Settings: TCodeFormatterSettings;
begin
  grid_Spacing.EditorMode := False;
  od_Import.FileName := 'DelForExOptions.ini';
  if not od_Import.Execute then
    Exit;

  Settings := TCodeFormatterSettings.Create;
  try
    TCodeFormatterConfigHandler.ImportFromFile(od_Import.FileName, Settings);
    SettingsToForm(Settings);
  finally
    Settings.Free;
  end;

  if pc_Main.ActivePage = ts_Preview then
    ts_PreviewShow(nil);
end;

procedure TfmCodeFormatterConfig.mi_ExportClick(Sender: TObject);
var
  Settings: TCodeFormatterSettings;
begin
  grid_Spacing.EditorMode := False;
  sd_Export.FileName := 'DelForExOptions.ini';
  if not sd_Export.Execute then
    Exit;

  Settings := TCodeFormatterSettings.Create;
  try
    FormToSettings(Settings);
    TCodeFormatterConfigHandler.ExportToFile(sd_Export.FileName, Settings);
  finally
    Settings.Free;
  end;
end;

class function TfmCodeFormatterConfig.Execute(ASettings: TCodeFormatterSettings): TModalResult;
var
  frm: TfmCodeFormatterConfig;
begin
  frm := TfmCodeFormatterConfig.Create(nil);
  try
    frm.HelpFile := 'delfor.hlp';
    frm.SettingsToForm(ASettings);
    Result := frm.ShowModal;
    if Result = mrOk then
      frm.FormToSettings(ASettings);
  finally
    frm.Free;
  end;
end;

{ TStringGrid }

constructor TStringGrid.Create(AOwner: TComponent);
begin
  inherited;
  FSpacingOptions := TStringList.Create;
  FSpacingOptions.Add(str_None);
  FSpacingOptions.Add(str_Before);
  FSpacingOptions.Add(str_after);
  FSpacingOptions.Add(str_BeforeAfter);
end;

function TStringGrid.GetEditStyle(ACol, ARow: Integer): TEditStyle;
begin
  if Col = 2 then
    Result := esPickList
  else
    Result := esSimple;
end;

function TStringGrid.CreateEditor: TInplaceEdit;
begin
  Result := TInplaceEditList.Create(Self);
  (Result as TInplaceEditList).OnGetPickListitems := OnGetSpacingOptions;
  (Result as TInplaceEditList).DropDownRows := 15;
end;

procedure TStringGrid.OnGetSpacingOptions(ACol, ARow: Integer; Items: TStrings);
begin
  Items.Assign(FSpacingOptions);
end;

destructor TStringGrid.Destroy;
begin
  FSpacingOptions.Free;
  inherited;
end;

function TStringGrid.CanEditModify: Boolean;
begin
  Result := False;
end;

function TStringGrid.CanEditShow: Boolean;
begin
  Result := (Col = 2);
end;

end.

