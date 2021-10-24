unit GX_GrepSearch;

{$I GX_CondDefine.inc}

interface

uses
  Classes, Controls, Forms, StdCtrls, ExtCtrls,
  GX_Experts, GX_GrepExpert, GX_GrepBackend, GX_BaseForm;

type
  TfmGrepSearch = class(TfmBaseForm)
    lblFind: TLabel;
    cbText: TComboBox;
    gbxOptions: TGroupBox;
    cbCaseSensitive: TCheckBox;
    cbForms: TCheckBox;
    cbFormsMultiline: TCheckBox;
    cbFormsSpecialChars: TCheckBox;
    gbxWhere: TGroupBox;
    rbAllProjFiles: TRadioButton;
    rbOpenFiles: TRadioButton;
    rbDirectories: TRadioButton;
    gbxDirectories: TGroupBox;
    lblMasks: TLabel;
    cbMasks: TComboBox;
    cbInclude: TCheckBox;
    pnlBottom: TPanel;
    btnOK: TButton;
    btnCancel: TButton;
    cbWholeWord: TCheckBox;
    rbCurrentOnly: TRadioButton;
    btnHelp: TButton;
    cbRegEx: TCheckBox;
    cbDirectory: TComboBox;
    btnBrowse: TButton;
    lblDirectory: TLabel;
    rbAllProjGroupFiles: TRadioButton;
    rbResults: TRadioButton;
    cbExcludedDirs: TComboBox;
    lblExcludeDirs: TLabel;
    cbSQLFiles: TCheckBox;
    gbxContentTypes: TGroupBox;
    cbGrepCode: TCheckBox;
    cbGrepStrings: TCheckBox;
    cbGrepComments: TCheckBox;
    gbxUnitSections: TGroupBox;
    cbSectionInterface: TCheckBox;
    cbSectionImplementation: TCheckBox;
    cbSectionInitialization: TCheckBox;
    cbSectionFinalization: TCheckBox;
    btnOptions: TButton;
    rgSaveOption: TRadioGroup;
    btnSearch: TButton;
    timHintTimer: TTimer;
    btnGrepAll: TButton;
    btnSectionAll: TButton;
    chk_UseMapFile: TCheckBox;
    txt_NoMapFile: TStaticText;
    l_MinMaxDepth: TLabel;
    ed_MinDepth: TEdit;
    ed_MaxDepth: TEdit;
    chk_SubDirRegex: TCheckBox;
    b_TestRegEx: TButton;
    procedure btnBrowseClick(Sender: TObject);
    procedure rbDirectoriesClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure cbDirectoryDropDown(Sender: TObject);
    procedure cbExcludedDirsDropDown(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ComboKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure btnOptionsClick(Sender: TObject);
    procedure cbGrepCodeClick(Sender: TObject);
    procedure cbGrepStringsClick(Sender: TObject);
    procedure cbGrepCommentsClick(Sender: TObject);
    procedure cbSectionInterfaceClick(Sender: TObject);
    procedure cbSectionImplementationClick(Sender: TObject);
    procedure cbSectionInitializationClick(Sender: TObject);
    procedure cbSectionFinalizationClick(Sender: TObject);
    procedure timHintTimerTimer(Sender: TObject);
    procedure btnGrepAllClick(Sender: TObject);
    procedure btnSectionAllClick(Sender: TObject);
    procedure cbIncludeClick(Sender: TObject);
    procedure ed_MinDepthChange(Sender: TObject);
    procedure chk_SubDirRegexClick(Sender: TObject);
    procedure b_TestRegExClick(Sender: TObject);
  private
    FEmbedded: Boolean;
    FCheckedWhere: Boolean;
    FEmbeddedHolder: TWinControl;
    FOriginalWidth: Integer;
    FOriginalOptionsGroupWidth: Integer;
    FOriginalWhereGroupWidth: Integer;
    FLoadingSettings: Boolean;
    FTheHintWindow: THintWindow;
    procedure EnableDirectoryControls(New: Boolean);
    procedure LoadFormSettings;
    procedure SaveFormSettings;
    procedure UpdateMRUs;
    procedure cbDirectoryOnDropFiles(_Sender: TObject; _Files: TStrings);
    procedure CheckEnabledWhereControls;
    procedure CheckContentTypeSelection(ClickedOption: TCheckBox);
    procedure CheckSectionSelection(ClickedOption: TCheckBox);
  public
    constructor Create(AOwner: TComponent); override;
    procedure EmbeddedInit(AHolderControl: TWinControl; ASearchEvent: TNotifyEvent);
    procedure EmbeddedUpdatePos;
    procedure EmbeddedShow;
    procedure EmbeddedSetHeights;
    procedure RetrieveSettings(var Value: TGrepSettings);
    procedure AdjustSettings(Value: TGrepSettings);
  end;

implementation

{$R *.dfm}

uses
  SysUtils, Windows, Messages, Graphics, StrUtils, Menus, Math,
  SynRegExpr,
  u_dzVclUtils, u_dzOsUtils, u_dzStringUtils,
  GX_GenericUtils, GX_GxUtils, GX_OtaUtils, GX_GrepResults, GX_GrepOptions,
  GX_TestRegEx;

{ TfmGrepSearch }

procedure TfmGrepSearch.btnGrepAllClick(Sender: TObject);
begin
  cbGrepCode.Checked := True;
  cbGrepStrings.Checked := True;
  cbGrepComments.Checked := True;
end;

procedure TfmGrepSearch.btnBrowseClick(Sender: TObject);
var
  Temp: string;
begin
  Temp := cbDirectory.Text;
  if GetDirectory(Temp) then
    cbDirectory.Text := Temp;
end;

procedure TfmGrepSearch.btnOptionsClick(Sender: TObject);
var
  UseCurrentIdent: Boolean;
  ExternalEditor: string;
  Params: string;
begin
  Assert(Assigned(gblGrepMenuEntryExpert));
  UseCurrentIdent := gblGrepExpert.GrepUseCurrentIdent;
  ExternalEditor := gblGrepExpert.ExternalEditor;
  Params := gblGrepExpert.ExternalEditorParams;
  if TfmGrepOptions.Execute(Self, UseCurrentIdent, ExternalEditor, Params) then begin
    gblGrepExpert.GrepUseCurrentIdent := UseCurrentIdent;
    gblGrepExpert.ExternalEditor := ExternalEditor;
    gblGrepExpert.ExternalEditorParams := Params;
  end;
end;

procedure TfmGrepSearch.btnSectionAllClick(Sender: TObject);
begin
  cbSectionInterface.Checked := True;
  cbSectionImplementation.Checked := True;
  cbSectionInitialization.Checked := True;
  cbSectionFinalization.Checked := True;
end;

procedure TfmGrepSearch.b_TestRegExClick(Sender: TObject);
var
  s: string;
  b: Boolean;
begin
  s := cbText.Text;
  b := cbCaseSensitive.Checked;
  if TfmTestRegEx.Execute(Self, gblGrepExpert.ContextFont, gblGrepExpert.ContextMatchColor, s, b) then begin
    cbText.Text := s;
    cbCaseSensitive.Checked := b;
  end;
end;

procedure TfmGrepSearch.EnableDirectoryControls(New: Boolean);
begin
  cbDirectory.Enabled := New;
  cbMasks.Enabled := New;
  cbExcludedDirs.Enabled := New;
  cbInclude.Enabled := New;
  ed_MinDepth.Enabled := New;
  ed_MaxDepth.Enabled := New;
  chk_SubDirRegex.Enabled := New;
  btnBrowse.Enabled := New;
  if not New then
  begin
    cbDirectory.Color := clBtnface;
    cbExcludedDirs.Color := clBtnface;
    cbMasks.Color := clBtnface;
    ed_MinDepth.Color := clBtnface;
    ed_MaxDepth.Color := clBtnface;
  end
  else
  begin
    cbDirectory.Color := clWindow;
    cbExcludedDirs.Color := clWindow;
    cbMasks.Color := clWindow;
    ed_MinDepth.Color := clWindow;
    ed_MaxDepth.Color := clWindow;
  end
end;

procedure TfmGrepSearch.rbDirectoriesClick(Sender: TObject);
begin
  EnableDirectoryControls(rbDirectories.Checked);
end;

procedure TfmGrepSearch.btnHelpClick(Sender: TObject);
begin
  GxContextHelp(Self, 1);
end;

procedure TfmGrepSearch.cbDirectoryDropDown(Sender: TObject);
begin
  SizeComboDropdownToItems(cbDirectory);
end;

procedure TfmGrepSearch.cbExcludedDirsDropDown(Sender: TObject);
begin
  SizeComboDropdownToItems(cbExcludedDirs);
end;

procedure TfmGrepSearch.CheckContentTypeSelection(ClickedOption: TCheckBox);
begin
  if FLoadingSettings then
    Exit;

  if cbGrepCode.Checked or cbGrepStrings.Checked or cbGrepComments.Checked then begin
    // at least one option is selected -> OK
  end else begin
    // we can't search with no option checked -> try to correct intelligently
    if Assigned(ClickedOption) then begin
      // unchecked interactively -> default to code
      cbGrepCode.Checked := True;
    end else begin
      // not interactively -> check them all
      cbGrepCode.Checked := True;
      cbGrepStrings.Checked := True;
      cbGrepComments.Checked := True;
    end;
  end;
end;

procedure TfmGrepSearch.cbGrepCodeClick(Sender: TObject);
begin
  CheckContentTypeSelection(cbGrepCode);
end;

procedure TfmGrepSearch.cbGrepCommentsClick(Sender: TObject);
begin
  CheckContentTypeSelection(cbGrepComments);
end;

procedure TfmGrepSearch.cbGrepStringsClick(Sender: TObject);
begin
  CheckContentTypeSelection(cbGrepStrings);
end;

procedure TfmGrepSearch.cbIncludeClick(Sender: TObject);
var
  b: Boolean;
begin
  b := cbInclude.Checked;
  l_MinMaxDepth.Enabled := b;
  ed_MinDepth.Enabled := b;
  ed_MaxDepth.Enabled := b;
end;

procedure TfmGrepSearch.CheckSectionSelection(ClickedOption: TCheckBox);
begin
  if FLoadingSettings then
    Exit;

  if cbSectionInterface.Checked or cbSectionImplementation.Checked
    or cbSectionInitialization.Checked or cbSectionFinalization.Checked then begin
    // at least one option is selected -> OK
  end else begin
    // we can't search no section
    if Assigned(ClickedOption) then begin
      // unchecked interactively -> try to guess what the user wants
      if ClickedOption = cbSectionInterface then begin
        // he unchecked Interface, so he probably only wants Implementation
        cbSectionImplementation.Checked := True;
      end else if ClickedOption = cbSectionImplementation then begin
        // he unchecked Implementation, so he probably only wants Interface
        cbSectionInterface.Checked := True;
      end else begin
        // he unchecked Initialization or Finalization, so he might only want Interface and Implementation
        cbSectionInterface.Checked := True;
        cbSectionImplementation.Checked := True;
      end;
    end else begin
      // not interactively -> check them all
      cbSectionInterface.Checked := True;
      cbSectionImplementation.Checked := True;
      cbSectionInitialization.Checked := True;
      cbSectionFinalization.Checked := True;
    end;
  end;
end;

procedure TfmGrepSearch.cbSectionFinalizationClick(Sender: TObject);
begin
  CheckSectionSelection(cbSectionFinalization);
end;

procedure TfmGrepSearch.cbSectionImplementationClick(Sender: TObject);
begin
  CheckSectionSelection(cbSectionImplementation);
end;

procedure TfmGrepSearch.cbSectionInitializationClick(Sender: TObject);
begin
  CheckSectionSelection(cbSectionInitialization);
end;

procedure TfmGrepSearch.cbSectionInterfaceClick(Sender: TObject);
begin
  CheckSectionSelection(cbSectionInterface);
end;

procedure TfmGrepSearch.btnOKClick(Sender: TObject);
resourcestring
  SSpecifiedDirectoryDoesNotExist = 'The search directory %s does not exist.';
  SSearchTextEmpty = 'The search text is empty.';
var
  i: Integer;
  Dirs: TStringList;
  s: string;
begin
  // we allow for ' ' (spaces)
  if cbText.Text = '' then
    raise Exception.Create(SSearchTextEmpty);

  if rbDirectories.Checked then
  begin
    s := Trim(cbDirectory.Text);
    if IsEmpty(s) then
      s := GetCurrentDir;
    Dirs := TStringList.Create;
    try
      AnsiStrTok(s, ';', Dirs);
      for i := 0 to Dirs.Count - 1 do begin
        Dirs[i] := ExpandFileName(AddSlash(Dirs[i]));
        if not DirectoryExists(Dirs[i]) then
          raise Exception.CreateFmt(SSpecifiedDirectoryDoesNotExist, [Dirs[i]]);
        if i < Dirs.Count - 1 then
          Dirs[i] := Dirs[i] + ';'
      end;
      cbDirectory.Text := StringReplace(Dirs.Text, #13#10, '', [rfReplaceAll]);
    finally
      FreeAndNil(Dirs);
    end;
  end;
  s := cbExcludedDirs.Text;
  while StartsStr(';', s) do
    s := Copy(s, 2);
  cbExcludedDirs.Text := StringReplace(s, ';;', ';', [rfReplaceAll]);

  SaveFormSettings;

  if cbRegEx.Checked then
  try
    ExecRegExpr(cbText.Text, '');
  except
    on E: ERegExpr do begin
      ShowError(E.Message);
      TryFocusControl(cbText);
      cbText.SelStart := E.CompilerErrorPos;
      cbText.SelLength := 0;
      Abort;
    end;
  end;

  ModalResult := mrOk;
end;

constructor TfmGrepSearch.Create(AOwner: TComponent);
begin
  inherited;
  FEmbedded := False;
  FEmbeddedHolder := nil;

  TWinControl_ActivateDropFiles(cbDirectory, cbDirectoryOnDropFiles);

  pnlBottom.BevelOuter := bvNone;

  InitDpiScaler;

  LoadFormSettings;
  FCheckedWhere := True;
end;

procedure TfmGrepSearch.ed_MinDepthChange(Sender: TObject);
begin
  inherited;
  if ed_MinDepth.Text <> '0' then
    ed_MinDepth.Color := clYellow
  else
    ed_MinDepth.Color := clWindow;
end;

procedure TfmGrepSearch.FormCreate(Sender: TObject);
begin
  FOriginalWidth := Width;
  FOriginalOptionsGroupWidth := gbxOptions.Width;
  FOriginalWhereGroupWidth := gbxWhere.Width;
end;

procedure TfmGrepSearch.cbDirectoryOnDropFiles(_Sender: TObject; _Files: TStrings);
var
  s: string;
begin
  if IsShiftDown then
    s := cbDirectory.Text + ';'
  else
    s := '';
  cbDirectory.Text := s + _Files.DelimitedText;
end;

procedure TfmGrepSearch.SaveFormSettings;
begin
  AddMRUString(cbText.Text, gblGrepExpert.SearchList, False, 90, -1);
  AddMRUString(cbDirectory.Text, gblGrepExpert.DirList, True);
  AddMRUString(cbMasks.Text, gblGrepExpert.MaskList, False);
  AddMRUString(cbExcludedDirs.Text, gblGrepExpert.ExcludedDirsList, False, True);

  gblGrepExpert.GrepCaseSensitive := cbCaseSensitive.Checked;
  gblGrepExpert.GrepCode := cbGrepCode.Checked;
  gblGrepExpert.GrepComments := cbGrepComments.Checked;
  gblGrepExpert.GrepStrings := cbGrepStrings.Checked;
  gblGrepExpert.GrepFinalization := cbSectionFinalization.Checked;
  gblGrepExpert.GrepImplementation := cbSectionImplementation.Checked;
  gblGrepExpert.GrepInitialization := cbSectionInitialization.Checked;
  gblGrepExpert.GrepInterface := cbSectionInterface.Checked;
  gblGrepExpert.GrepForms := cbForms.Checked;
  gblGrepExpert.GrepFormsMultiline := cbFormsMultiline.Checked;
  gblGrepExpert.GrepFormsSpecialChars := cbFormsSpecialChars.Checked;
  gblGrepExpert.GrepSQLFiles := cbSQLFiles.Checked;
  gblGrepExpert.GrepSub := cbInclude.Checked;
  gblGrepExpert.GrepWholeWord := cbWholeWord.Checked;
  gblGrepExpert.GrepRegEx := cbRegEx.Checked;

  gblGrepExpert.GrepSaveOption := TGrepSaveOption(rgSaveOption.ItemIndex);

  if rbCurrentOnly.Checked then
    gblGrepExpert.GrepSearch := 0
  else if rbAllProjFiles.Checked then
    gblGrepExpert.GrepSearch := 1
  else if rbOpenFiles.Checked then
    gblGrepExpert.GrepSearch := 2
  else if rbDirectories.Checked then
    gblGrepExpert.GrepSearch := 3
  else if rbAllProjGroupFiles.Checked then
    gblGrepExpert.GrepSearch := 4
  else if rbResults.Checked then
    gblGrepExpert.GrepSearch := 5;

  gblGrepExpert.GrepMinDepth := StrToIntDef(ed_MinDepth.Text, 0);
  gblGrepExpert.GrepMaxDepth := StrToIntDef(ed_MaxDepth.Text, -1);
  gblGrepExpert.ExcludedDirsIsRegEx := chk_SubDirRegex.Checked;

  gblGrepExpert.GrepUseMapFile := chk_UseMapFile.Checked;
end;

procedure TfmGrepSearch.timHintTimerTimer(Sender: TObject);
begin
  timHintTimer.Enabled := False;
  if Assigned(FTheHintWindow) then begin
    FTheHintWindow.ReleaseHandle;
    FreeAndNil(FTheHintWindow);
  end;
end;

procedure TfmGrepSearch.LoadFormSettings;

  function RetrieveEditorBlockSelection: string;
  var
    i: Integer;
  begin
    Result := GxOtaGetCurrentSelection;
    if Trim(Result) = '' then begin
      // we don't search for white space only
      Result := '';
    end else begin
      if Length(Result) > 80 then begin
        // Allow a maximum length of 80 characters
        // I'm not sure whether this is restriction still makes sense, since nowadays lines are
        // usually longer than 80 characters, but I'm not going to change this unless somebody
        // explicitly requests it. Personally I have never had a need for >80 chars for the
        // search pattern. -- 2018-11-04 twm
        Result := LeftStr(Result, 80)
      end;
      i := Min(Pos(#13, Result), Pos(#10, Result));
      if i > 0 then begin
        // The engine does not allow line breaks
        Result := LeftStr(Result, i - 1);
      end;
    end;
  end;

  procedure SetSearchPattern(Str: string);
  begin
    cbText.Text := Str;
    cbText.SelectAll;
  end;

  procedure SetDefaultSearchPattern;
  var
    Selection: string;
  begin
    Selection := fmGrepResults.ContextSearchText;
    if Trim(Selection) = '' then
      Selection := RetrieveEditorBlockSelection;
    if (Trim(Selection) = '') and gblGrepExpert.GrepUseCurrentIdent then
      try
        Selection := GxOtaGetCurrentIdent;  //if access violation created
      except
        on E: Exception do
          Selection := '';
      end;
    if (Selection = '') and (cbText.Items.Count > 0) then
      Selection := cbText.Items[0];
    SetSearchPattern(Selection);
  end;

resourcestring
  SGrepResultsNotActive = 'The Grep Results window is not active';

var
  sl: TStringList;
begin
  if not Assigned(fmGrepResults) then
    raise Exception.Create(SGrepResultsNotActive);

  FLoadingSettings := True;
  try
    cbText.Items.Assign(gblGrepExpert.SearchList);
    cbDirectory.Items.Assign(gblGrepExpert.DirList);
    cbMasks.Items.Assign(gblGrepExpert.MaskList);
    cbExcludedDirs.Items.Assign(gblGrepExpert.ExcludedDirsList);
    sl := TStringList.Create;
    try
      AddDelphiDirsToIgnore(sl);
      AddSCMDirsToIgnore(sl);
      sl.Delimiter := ';';
      cbExcludedDirs.Items.Add(sl.DelimitedText);
    finally
      FreeAndNil(sl);
    end;

    rbResults.Enabled := fmGrepResults.lbResults.Count > 0;

    cbCaseSensitive.Checked := gblGrepExpert.GrepCaseSensitive;
    cbGrepCode.Checked := gblGrepExpert.GrepCode;
    cbGrepComments.Checked := gblGrepExpert.GrepComments;
    cbGrepStrings.Checked := gblGrepExpert.GrepStrings;
    cbSectionFinalization.Checked := gblGrepExpert.GrepFinalization;
    cbSectionImplementation.Checked := gblGrepExpert.GrepImplementation;
    cbSectionInitialization.Checked := gblGrepExpert.GrepInitialization;
    cbSectionInterface.Checked := gblGrepExpert.GrepInterface;
    cbForms.Checked := gblGrepExpert.GrepForms;
    cbFormsMultiline.Checked := gblGrepExpert.GrepFormsMultiline;
    cbFormsSpecialChars.Checked := gblGrepExpert.GrepFormsSpecialChars;
    cbSQLFiles.Checked := gblGrepExpert.GrepSQLFiles;
    cbInclude.Checked := gblGrepExpert.GrepSub;
    cbWholeWord.Checked := gblGrepExpert.GrepWholeWord;
    cbRegEx.Checked := gblGrepExpert.GrepRegEx;

    rgSaveOption.ItemIndex := Integer(gblGrepExpert.SaveOption);

    case gblGrepExpert.GrepSearch of
      0: rbCurrentOnly.Checked := True;
      1: rbAllProjFiles.Checked := True;
      2: rbOpenFiles.Checked := True;
      3: rbDirectories.Checked := True;
      4: rbAllProjGroupFiles.Checked := True;
      5: begin
          if rbResults.Enabled then
            rbResults.Checked := True
          else
            rbAllProjFiles.Checked := True;
        end;
    else
      rbAllProjFiles.Checked := True;
    end;

    ed_MinDepth.Text := IntToStr(gblGrepExpert.GrepMinDepth);
    if gblGrepExpert.GrepMaxDepth >= 0 then
      ed_MaxDepth.Text := IntToStr(gblGrepExpert.GrepMaxDepth)
    else
      ed_MaxDepth.Text :=  '';

    chk_SubDirRegex.Checked := gblGrepExpert.ExcludedDirsIsRegEx;

    chk_UseMapFile.Checked := gblGrepExpert.GrepUseMapFile;

    if cbText.Items.Count > 0 then
      cbText.Text := cbText.Items[0];
    if cbDirectory.Items.Count > 0 then
      cbDirectory.Text := cbDirectory.Items[0];
    if cbMasks.Items.Count > 0 then
      cbMasks.Text := cbMasks.Items[0];
    if cbExcludedDirs.Items.Count > 0 then
      cbExcludedDirs.Text := cbExcludedDirs.Items[0];

    if not gblGrepExpert.GrepSaveHistoryListItems then begin
      rgSaveOption.Visible := False;
      Height := Height - rgSaveOption.Height;
    end;

    if not IsStandAlone then
      SetDefaultSearchPattern;
  finally
    FLoadingSettings := False;
  end;

  CheckContentTypeSelection(nil);
  CheckSectionSelection(nil);
  CheckEnabledWhereControls;

  EnableDirectoryControls(rbDirectories.Checked);
end;

procedure TfmGrepSearch.CheckEnabledWhereControls;
var
  MapFn: string;
begin
  if not IsStandAlone then
  begin
    if Trim(GxOtaGetCurrentProjectName) = '' then
    begin
      rbAllProjFiles.Enabled := False;
      chk_UseMapFile.Enabled := False;
      txt_NoMapFile.Visible := True;
      rbOpenFiles.Enabled := False;
    end
    else
    begin
      rbAllProjFiles.Enabled := True;
      if GxOtaGetCurrentMapFileName(MapFn) then begin
        txt_NoMapFile.Visible := False;
        chk_UseMapFile.Enabled := True;
      end else begin
        txt_NoMapFile.Visible := True;
        chk_UseMapFile.Enabled := False;
      end;
      rbOpenFiles.Enabled := True;
    end;

    if Trim(GxOtaGetProjectGroupFileName) = '' then
      rbAllProjGroupFiles.Enabled := False
    else
      rbAllProjGroupFiles.Enabled := True;

    rbCurrentOnly.Enabled := Trim(GxOtaGetFileNameOfCurrentModule) <> '';
  end
  else // IsStandAlone
  begin
    rbDirectories.Checked := True;
    rbOpenFiles.Enabled := False;
    rbAllProjGroupFiles.Enabled := False;
    rbAllProjFiles.Enabled := False;
    chk_UseMapFile.Enabled := False;
    rbCurrentOnly.Enabled := False;
  end;
end;

procedure TfmGrepSearch.RetrieveSettings(var Value: TGrepSettings);
begin
  Value.IncludeComments := cbGrepComments.Checked;
  Value.IncludeCode := cbGrepCode.Checked;
  Value.IncludeStrings := cbGrepStrings.Checked;
  Value.SectionInterface := cbSectionInterface.Checked;
  Value.SectionImplementation := cbSectionImplementation.Checked;
  Value.SectionInitialization := cbSectionInitialization.Checked;
  Value.SectionFinalization := cbSectionFinalization.Checked;
  Value.CaseSensitive := cbCaseSensitive.Checked;
  Value.WholeWord := cbWholeWord.Checked;
  Value.RegEx := cbRegEx.Checked;
  Value.Pattern := cbText.Text;
  Value.IncludeForms := cbForms.Checked;
  Value.HandleFormMultiline:= cbFormsMultiline.Checked;
  Value.HandleFormSpecialChars := cbFormsSpecialChars.Checked;
  Value.IncludeSQLs := cbSQLFiles.Checked;
  Value.SaveOption := TGrepSaveOption(rgSaveOption.ItemIndex);
  Value.Mask := '';
  Value.Directories := '';
  Value.ExcludedDirs := '';
  Value.IncludeSubdirs := True;
  Value.UseMapFile := gblGrepExpert.GrepUseMapFile;

  if rbAllProjFiles.Checked then
    Value.GrepAction := gaProjGrep
  else if rbCurrentOnly.Checked then
    Value.GrepAction := gaCurrentOnlyGrep
  else if rbOpenFiles.Checked then
    Value.GrepAction := gaOpenFilesGrep
  else if rbAllProjGroupFiles.Checked then
    Value.GrepAction := gaProjGroupGrep
  else if rbResults.Checked then
    Value.GrepAction := gaResults
  else
  begin
    Value.GrepAction := gaDirGrep;
    Value.Mask := cbMasks.Text;
    Value.Directories := cbDirectory.Text;
    Value.IncludeSubdirs := cbInclude.Checked;
    Value.MinDepth := StrToIntDef(ed_MinDepth.Text, 0);
    Value.MaxDepth := StrToIntDef(ed_MaxDepth.Text, -1);
    Value.ExcludedDirs := cbExcludedDirs.Text;
    Value.ExcludedDirsIsRegEx := chk_SubDirRegex.Checked;
  end;
end;

procedure TfmGrepSearch.AdjustSettings(Value: TGrepSettings);
begin
  cbGrepComments.Checked := Value.IncludeComments;
  cbGrepCode.Checked := Value.IncludeCode;
  cbGrepStrings.Checked := Value.IncludeStrings;
  cbSectionInterface.Checked := Value.SectionInterface;
  cbSectionImplementation.Checked := Value.SectionImplementation;
  cbSectionInitialization.Checked := Value.SectionInitialization;
  cbSectionFinalization.Checked := Value.SectionFinalization;
  cbCaseSensitive.Checked := Value.CaseSensitive;
  cbWholeWord.Checked := Value.WholeWord;
  cbRegEx.Checked := Value.RegEx;
  cbText.Text := Value.Pattern;
  cbText.SelectAll;
  cbForms.Checked := Value.IncludeForms;
  cbFormsMultiline.Checked := Value.HandleFormMultiline;
  cbFormsSpecialChars.Checked := Value.HandleFormSpecialChars;
  cbSQLFiles.Checked := Value.IncludeSQLs;

  rgSaveOption.ItemIndex := Integer(Value.SaveOption);

  cbMasks.Text := '';
  cbDirectory.Text := '';
  cbExcludedDirs.Text := '';
  cbInclude.Checked := True;
  case Value.GrepAction of
    gaProjGrep: rbAllProjFiles.Checked := True;
    gaCurrentOnlyGrep: rbCurrentOnly.Checked := True;
    gaOpenFilesGrep: rbOpenFiles.Checked := True;
    gaProjGroupGrep: rbAllProjGroupFiles.Checked := True;
    gaResults: rbResults.Checked := True;
    gaDirGrep:
    begin
      rbDirectories.Checked := True;
      cbMasks.Text := Value.Mask;
      cbDirectory.Text := Value.Directories;
      cbInclude.Checked := Value.IncludeSubdirs;
      cbExcludedDirs.Text := Value.ExcludedDirs;
        chk_SubDirRegex.Checked := Value.ExcludedDirsIsRegEx;
    end;
  end;
  EnableDirectoryControls(rbDirectories.Checked);
end;

procedure TfmGrepSearch.FormShow(Sender: TObject);
begin
  TControl_SetConstraints(Self, [ccMinWidth, ccMinHeight, ccMaxHeight]);
end;

procedure TfmGrepSearch.ComboKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  SelectedIndex: Integer;
  Combo: TCustomComboBox;
begin
  if (Key = VK_DELETE) and (ssCtrl in Shift) then
  begin
    Combo := (Sender as TCustomComboBox);
    if Combo.DroppedDown then
    begin
      SelectedIndex := Combo.ItemIndex;
      if SelectedIndex > -1 then begin
        Combo.Items.Delete(SelectedIndex);
        UpdateMRUs;
      end;
    end;
  end;
end;

procedure TfmGrepSearch.UpdateMRUs;
begin
  gblGrepExpert.SearchList.Assign(cbText.Items);
  gblGrepExpert.DirList.Assign(cbDirectory.Items);
  gblGrepExpert.MaskList.Assign(cbMasks.Items);
  gblGrepExpert.ExcludedDirsList.Assign(cbExcludedDirs.Items);
end;

procedure TfmGrepSearch.EmbeddedInit(AHolderControl: TWinControl; ASearchEvent: TNotifyEvent);
begin
  FEmbedded := True;
  FEmbeddedHolder := AHolderControl;
  //.Left + 2, lbResults.Top + 55, lbResults.Width - 4

  Parent := TWinControl(Owner);
  Height := ClientHeight;
  BorderIcons := [];
  BorderStyle := bsNone; //ToolWindow;
  Position := poDesigned;
  FormStyle := fsStayOnTop;

  btnSearch.Left := btnOptions.Left;
  EmbeddedSetHeights;
  EmbeddedUpdatePos;

  btnOK.Visible := False;
  btnCancel.Visible := False;
  btnOptions.Visible := False;

  btnSearch.Visible := True;
  btnSearch.OnClick := ASearchEvent;

  FCheckedWhere := False;
end;

procedure TfmGrepSearch.EmbeddedSetHeights;

//  function MoveTo(ATopDelta: Integer; AMainCtrl: TControl; AItemsDelta: Integer; AItems: array of TControl): Integer;
//  var
//    I, Delta: Integer;
//  begin
//    AMainCtrl.Top := AMainCtrl.Top - ATopDelta;
//    Result := 0;
//    for I := 0 to High(AItems) do
//    begin
//      if AItemsDelta > 0 then
//      begin
//        Delta := (I+1) * AItemsDelta;
//        Inc(Result, Delta div 2);
//      end
//      else
//        Delta := ATopDelta;
//      AItems[I].Top := AItems[I].Top - Delta;
//    end;
//    if High(AItems) = -1 then
//      Inc(Result, AItemsDelta);
//    if AItemsDelta > 0 then
//      AMainCtrl.Height := AMainCtrl.Height - Result;
//    Inc(Result, ATopDelta);
//  end;

//var
//  LHS: Integer;  // LastHeightsSum
begin
  // I'm not sure this has ever been necessary, but it's definitely broken now after many new
  // controls have been added. So I've commented it out but I leave it here in case it turns
  // out to be necessary after all. -- 2020-05-02 twm

//  MoveTo(5, cbText, 0, [lblFind]);
//
//         MoveTo(10, gbxOptions, 3, [cbCaseSensitive, cbWholeWord, cbForms, cbSQLFiles, cbRegEx]);
//  LHS := MoveTo(10, gbxWhere, 5, [rbCurrentOnly, rbAllProjGroupFiles, rbAllProjFiles, rbOpenFiles, rbDirectories, rbResults]);
//         gbxWhere.Height := gbxWhere.Height + 13;
//         gbxOptions.Height := gbxWhere.Height;
//
//         MoveTo(-5 + LHS, gbxContentTypes, 3, [cbGrepCode, cbGrepStrings, cbGrepComments]);
//  LHS := MoveTo(-5 + LHS, gbxUnitSections, 3, [cbSectionInterface, cbSectionImplementation, cbSectionInitialization, cbSectionFinalization]);
//         gbxContentTypes.Height := gbxUnitSections.Height;
//
//  LHS := MoveTo(4 + LHS, gbxDirectories, 5, [cbDirectory, cbExcludedDirs, cbMasks, cbInclude]);
//         lblDirectory.Top := cbDirectory.Top;
//         btnBrowse.Top := cbDirectory.Top;
//         lblExcludeDirs.Top := cbExcludedDirs.Top;
//         lblMasks.Top := cbMasks.Top;
//
//  LHS := MoveTo(5 + LHS, rgSaveOption, 15, []);
//
//  Height := Height - LHS - 3;
end;

procedure TfmGrepSearch.EmbeddedUpdatePos;
const
  cEmbeddedLeft = 1;
  cEmbeddedTop = 55;
  cMinWidth = 382;
var
  NewWidth: Integer;
  Margins: Integer;
  HorizontalScale: Double;
begin
  Left := FEmbeddedHolder.Left + cEmbeddedLeft;
  Top := FEmbeddedHolder.Top + cEmbeddedTop;

  NewWidth := FEmbeddedHolder.Width - 2 * cEmbeddedLeft;
  if NewWidth >= FOriginalWidth then
    NewWidth := FOriginalWidth
  else
    NewWidth := Max(NewWidth, cMinWidth);

  Margins := gbxWhere.Left - (gbxOptions.Left + gbxOptions.Width);
  HorizontalScale := (NewWidth - 3 * Margins) / (FOriginalWidth - 3 * Margins);

  gbxContentTypes.Anchors := [akTop, akLeft];
  gbxUnitSections.Anchors := [akTop, akLeft];

  gbxOptions.Left := Margins div 2;
  gbxOptions.Width := Trunc(FOriginalOptionsGroupWidth * HorizontalScale);
  cbFormsSpecialChars.Left := (gbxOptions.Width - cbFormsMultiline.Left) div 2;

  gbxWhere.Left := Margins + gbxOptions.Left + gbxOptions.Width;
  gbxWhere.Width := Trunc(FOriginalWhereGroupWidth * HorizontalScale);

  gbxContentTypes.Left := Margins + gbxWhere.Left + gbxWhere.Width;
  gbxContentTypes.Width := NewWidth - gbxContentTypes.Left - Margins div 2;
  gbxUnitSections.Left := Margins + gbxWhere.Left + gbxWhere.Width;
  gbxUnitSections.Width := NewWidth - gbxUnitSections.Left - Margins div 2;

  gbxDirectories.Left := Margins div 2;
  gbxDirectories.Width := NewWidth - Margins;

  rgSaveOption.Left := Margins div 2;
  rgSaveOption.Width := NewWidth - Margins;

  Width := NewWidth;
end;

procedure TfmGrepSearch.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  if FEmbedded then
    Action := caHide;
end;

procedure TfmGrepSearch.EmbeddedShow;
begin
  if not FEmbedded then
    Exit;

  if not FCheckedWhere then
  begin
    FCheckedWhere := True;
    CheckEnabledWhereControls;
  end;

  if not Visible then
    EmbeddedUpdatePos;

  Show;
  BringToFront;
end;

procedure TfmGrepSearch.chk_SubDirRegexClick(Sender: TObject);
begin
  if chk_SubDirRegex.Checked then
    lblExcludeDirs.Caption := 'Exclude Dirs'
  else
    lblExcludeDirs.Caption := 'Exclude Dirs (separate by semicolon)'
end;

end.

