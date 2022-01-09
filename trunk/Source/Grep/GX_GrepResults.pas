{Search history author: (ERT) Ferenc Kiffer, Hungary <kifferferenc@yahoo.com>}

unit GX_GrepResults;

{$I GX_CondDefine.inc}

interface

uses
  Windows, Classes, Graphics, Controls, Forms, ActnList, Dialogs, StdCtrls, ExtCtrls, ToolWin,
  Types, // for inlining
  ComCtrls, Menus, Actions, UITypes,
  DropSource,
  GX_GrepBackend, GX_GrepExpert, GX_ConfigurationInfo, GX_IdeDock, GX_GrepSearch, GX_SharedImages,
  // if you get a compile error in the next line, add 'GX_IdeDockStandAlone=GX_IdeDock' to unit aliases
  // This entry gets added by the Delphi IDE every time this form is edited as part of the
  // stand alone Grep project, I got tired of removing it every time.
  GX_IdeDockStandAlone;

type
  TPageIndexType = (pitClickedEntryKeyIndex, pitTopKeyIndex, pitClickedEntryItemIndex, pitTopItemIndex);
  TPageSavedIndexType = Low(TPageIndexType)..pitTopKeyIndex;
  TPageIndexes = array[TPageIndexType] of Integer;
  TExpandMode = (emExpand, emContract, emToggle);

  TfmGrepResults = class(TfmIdeDockForm)
    StatusBar: TStatusBar;
    lbResults: TListBox;
    MainMenu: TMainMenu;
    mitFile: TMenuItem;
    mitFileSearch: TMenuItem;
    mitFileExit: TMenuItem;
    mitFilePrint: TMenuItem;
    mitList: TMenuItem;
    mitListContract: TMenuItem;
    mitListExpand: TMenuItem;
    mitHelp: TMenuItem;
    mitHelpHelp: TMenuItem;
    mitHelpAbout: TMenuItem;
    mitFileAbort: TMenuItem;
    mitListGotoSelected: TMenuItem;
    mitFileRefresh: TMenuItem;
    mitViewStayOnTop: TMenuItem;
    mitHelpSep1: TMenuItem;
    TheActionList: TActionList;
    actFileSearch: TAction;
    actFileRefresh: TAction;
    actFileAbort: TAction;
    actFilePrint: TAction;
    actViewStayOnTop: TAction;
    actFileExit: TAction;
    actListGotoSelected: TAction;
    actListContract: TAction;
    actListExpand: TAction;
    actHelpHelp: TAction;
    actHelpContents: TAction;
    actHelpAbout: TAction;
    mitHelpContents: TMenuItem;
    ToolBar: TToolBar;
    tbnSearch: TToolButton;
    tbnRefresh: TToolButton;
    tbnAbort: TToolButton;
    tbnSep2: TToolButton;
    tbnGoto: TToolButton;
    tbnSep3: TToolButton;
    tbnPrint: TToolButton;
    tbnSep4: TToolButton;
    tbnContract: TToolButton;
    tbnExpand: TToolButton;
    tbnSep1: TToolButton;
    tbnSep5: TToolButton;
    tbnStayOnTop: TToolButton;
    tbnSep6: TToolButton;
    tbnHelp: TToolButton;
    mitListSep1: TMenuItem;
    mitFileSep1: TMenuItem;
    pnlBottom: TPanel;
    reContext: TRichEdit;
    SplitterContext: TSplitter;
    mitViewSep1: TMenuItem;
    actViewShowContext: TAction;
    miViewShowMatchContext: TMenuItem;
    actFileSave: TAction;
    actListCopy: TAction;
    mitFileSave: TMenuItem;
    mitFileCopy: TMenuItem;
    mitView: TMenuItem;
    mitViewSep3: TMenuItem;
    actViewToolBar: TAction;
    mitViewToolBar: TMenuItem;
    mitViewOptions: TMenuItem;
    actViewOptions: TAction;
    actReplaceAll: TAction;
    actReplaceSelected: TAction;
    tbnReplaceAll: TToolButton;
    tbnReplaceSelected: TToolButton;
    tbnSep7: TToolButton;
    mitReplace: TMenuItem;
    mitReplaceReplaceAll: TMenuItem;
    mitReplaceSelected: TMenuItem;
    mitListSep2: TMenuItem;
    pnlMain: TPanel;
    actListGotoSelectedAndClose: TAction;
    GotoSelectedandClose1: TMenuItem;
    lbHistoryList: TListBox;
    SplitterHistoryList: TSplitter;
    pmHistoryMenu: TPopupMenu;
    actHistoryView: TAction;
    actHistoryDelete: TAction;
    actHistoryRefresh: TAction;
    miHistoryView: TMenuItem;
    miHistoryRefresh: TMenuItem;
    mitHistorySep2: TMenuItem;
    miHistoryDelete: TMenuItem;
    mitHistorySep1: TMenuItem;
    miHistoryItemName: TMenuItem;
    actViewShowHistoryList: TAction;
    actViewShowFullFilename: TAction;
    miViewShowHistoryList: TMenuItem;
    mitViewSep2: TMenuItem;
    miViewShowFullFilename: TMenuItem;
    actHistoryDeleteSelected: TAction;
    pmContextMenu: TPopupMenu;
    actContextSelSearch: TAction;
    miContextSearchSelectedText: TMenuItem;
    actHistorySearch: TAction;
    miHistorySearch: TMenuItem;
    mitFileSep2: TMenuItem;
    mitFileSep4: TMenuItem;
    actFilePrintToFile: TAction;
    actFileSavePrint: TAction;
    actFileOpen: TAction;
    miFilePrintToFile: TMenuItem;
    miFileSavePrint: TMenuItem;
    mitFileOpen: TMenuItem;
    mitFileSep3: TMenuItem;
    OpenDialog: TOpenDialog;
    actHistoryRefreshSelected: TAction;
    mitFileSep5: TMenuItem;
    miFileRefreshSelected: TMenuItem;
    actHistoryModifySearchSettings: TAction;
    miHistoryModifySearchSettings: TMenuItem;
    tbnSep8: TToolButton;
    tbnShowFullFilename: TToolButton;
    mitFileDeleteSelected: TMenuItem;
    mitFileSep7: TMenuItem;
    miHistorySettings: TMenuItem;
    miSettingsCurrentFile: TMenuItem;
    miSettingsAllFilesInProjectGroup: TMenuItem;
    miSettingsAllFilesInProject: TMenuItem;
    miSettingsOpenProjectFiles: TMenuItem;
    miSettingsDirectories: TMenuItem;
    miSettingsPreviousSearchResultFiles: TMenuItem;
    miSettingsCaseSensitive: TMenuItem;
    miSettingsWholeWord: TMenuItem;
    miSettingsSearchFormFiles: TMenuItem;
    miSettingsSearchSQLFiles: TMenuItem;
    miSettingsRegularExpression: TMenuItem;
    miSettingsDirectoriesData: TMenuItem;
    miSettingsExcludeDirs: TMenuItem;
    miSettingsFileMasks: TMenuItem;
    miSettingsSearchSubDirectories: TMenuItem;
    tcHistoryListPage: TTabControl;
    actHistorySort: TAction;
    miSettingsSaveOption: TMenuItem;
    miSettingsSep1: TMenuItem;
    miSettingsSep2: TMenuItem;
    miSettingsSep3: TMenuItem;
    actHistoryModifySaveOptions: TAction;
    mitFileSep6: TMenuItem;
    mitFileModifySaveOptions: TMenuItem;
    miHistoryLastSearchTime: TMenuItem;
    actViewShowIndent: TAction;
    miViewShowIndent: TMenuItem;
    tbnShowLineIndent: TToolButton;
    mitHistorySep3: TMenuItem;
    miHistorySort: TMenuItem;
    actHistorySearchInHistory: TAction;
    mitFileSearhInHistory: TMenuItem;
    miSettingsSep4: TMenuItem;
    miSettingsGrepCode: TMenuItem;
    miSettingsGrepStrings: TMenuItem;
    miSettingsGrepComments: TMenuItem;
    miSettingsSectionInterface: TMenuItem;
    miSettingsSectionImplementation: TMenuItem;
    miSettingsSectionInitialization: TMenuItem;
    miSettingsSectionFinalization: TMenuItem;
    miSettingsSepDir: TMenuItem;
    tbnSearchInHistory: TToolButton;
    tbnSep9: TToolButton;
    actListSelectNext: TAction;
    actListSelectPrevious: TAction;
    mitListSelectNext: TMenuItem;
    mitListSelectPrevious: TMenuItem;
    tbnHamburgerMenu: TToolButton;
    actHamburgerMenu: TAction;
    pmHamburgerMenu: TPopupMenu;
    miFile: TMenuItem;
    List1: TMenuItem;
    View1: TMenuItem;
    Replace1: TMenuItem;
    Help1: TMenuItem;
    Search1: TMenuItem;
    Refresh1: TMenuItem;
    Abort1: TMenuItem;
    N1: TMenuItem;
    Print1: TMenuItem;
    Open1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    Save1: TMenuItem;
    PrinttoFile1: TMenuItem;
    SavePrint1: TMenuItem;
    N4: TMenuItem;
    Refresh2: TMenuItem;
    N5: TMenuItem;
    Delete1: TMenuItem;
    N6: TMenuItem;
    ModifySaveOptions1: TMenuItem;
    Search2: TMenuItem;
    GotoSelected1: TMenuItem;
    GotoSelectedandClose2: TMenuItem;
    SelectNext1: TMenuItem;
    SelectPrevious1: TMenuItem;
    N7: TMenuItem;
    Copy1: TMenuItem;
    N8: TMenuItem;
    Contract1: TMenuItem;
    Expand1: TMenuItem;
    Options1: TMenuItem;
    N9: TMenuItem;
    ShowMatchContext1: TMenuItem;
    ShowHistoryList1: TMenuItem;
    N10: TMenuItem;
    ShowFullFilename1: TMenuItem;
    ShowIndent1: TMenuItem;
    StayonTop1: TMenuItem;
    N11: TMenuItem;
    ReplaceAllItems1: TMenuItem;
    ReplaceSelectedItem1: TMenuItem;
    Help2: TMenuItem;
    Contents1: TMenuItem;
    miSettingsSep5: TMenuItem;
    miSettingsFormHandleMultiline: TMenuItem;
    miSettingsFormHandleSpecialChars: TMenuItem;
    miSettingsExcludeDirsIsRegEx: TMenuItem;
    procedure FormResize(Sender: TObject);
    procedure lbResultsMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure lbResultsKeyPress(Sender: TObject; var Key: Char);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure lbResultsDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure lbResultsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure lbResultsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure actFileSearchExecute(Sender: TObject);
    procedure actFileRefreshExecute(Sender: TObject);
    procedure actFileAbortExecute(Sender: TObject);
    procedure actFilePrintExecute(Sender: TObject);
    procedure actListCopyExecute(Sender: TObject);
    procedure actFileSaveExecute(Sender: TObject);
    procedure actViewStayOnTopExecute(Sender: TObject);
    procedure actFileExitExecute(Sender: TObject);
    procedure actHelpHelpExecute(Sender: TObject);
    procedure actHelpAboutExecute(Sender: TObject);
    procedure actListGotoSelectedExecute(Sender: TObject);
    procedure actListContractExecute(Sender: TObject);
    procedure actListExpandExecute(Sender: TObject);
    procedure TheActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure actHelpContentsExecute(Sender: TObject);
    procedure lbResultsClick(Sender: TObject);
    procedure actShowMatchContextExecute(Sender: TObject);
    procedure actViewToolBarExecute(Sender: TObject);
    procedure actViewOptionsExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure actReplaceAllExecute(Sender: TObject);
    procedure actReplaceSelectedExecute(Sender: TObject);
    procedure lbResultsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure actListGotoSelectedAndCloseExecute(Sender: TObject);
    procedure lbHistoryListData(Control: TWinControl; Index: Integer; var Data: string);
    procedure lbHistoryListMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure actHistoryViewExecute(Sender: TObject);
    procedure actHistoryRefreshExecute(Sender: TObject);
    procedure lbHistoryListContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure lbHistoryListDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure actHistoryDeleteExecute(Sender: TObject);
    procedure actViewShowHistoryListExecute(Sender: TObject);
    procedure actViewShowFullFilenameExecute(Sender: TObject);
    procedure actContextSelSearchExecute(Sender: TObject);
    procedure actHistoryDeleteSelectedExecute(Sender: TObject);
    procedure actFileOpenExecute(Sender: TObject);
    procedure actHistoryRefreshSelectedExecute(Sender: TObject);
    procedure lbHistoryListDblClick(Sender: TObject);
    procedure actHistoryUpdate(Sender: TObject);
    procedure tcHistoryListPageChange(Sender: TObject);
    procedure actHistorySortExecute(Sender: TObject);
    procedure lbHistoryListMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure actHistoryModifySaveOptionsExecute(Sender: TObject);
    procedure actViewShowIndentExecute(Sender: TObject);
    procedure miHistoryItemNameClick(Sender: TObject);
    procedure lbHistoryListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure actHistorySearchInHistoryExecute(Sender: TObject);
    procedure SplitterHistoryListMoved(Sender: TObject);
    procedure reContextContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure actListSelectNextExecute(Sender: TObject);
    procedure actListSelectPreviousExecute(Sender: TObject);
    procedure actHamburgerMenuExecute(Sender: TObject);
    procedure SplitterContextCanResize(Sender: TObject; var NewSize: Integer; var Accept: Boolean);
    procedure SplitterContextMoved(Sender: TObject);
    procedure SplitterHistoryListCanResize(Sender: TObject; var NewSize: Integer; var Accept: Boolean);
  private
    FLastRepaintTick: DWORD;
    FSearchInProgress: Boolean;
    FReplaceInProgress: Boolean;
    FDragSource: TDropFileSource;
    FDragPoint: TPoint;
    FGrepSettings: TGrepSettings;
    FSearcher: TGrepSearchRunner;
    FShowContext: Boolean;
    FDoSearchReplace: Boolean;
    FShowFullFilename: Boolean;
    FShowLineIndent: Boolean;
    FShowHistoryList: Boolean;
    FLoadedContextHeightRelative: Integer;
    FLoadedHistoryListWidthRelative: Integer;
    FLoadHistoryListPage: Integer;
    FContextSearchText: string;
    FPageIndexes: array[TGrepHistoryListMode] of TPageIndexes;
    FSavedLastSearchTimeCaption: string;
    FSavedSaveOptionCaption: string;
    FSavedDirectoriesDataCaption: string;
    FSavedExcludeDirsCaption: string;
    FSavedFileMasksCaption: string;
    FHistoryMousePos: TPoint;
    FSelectedCount: Integer;
    FSaveSplitCount: Integer;
    FEmbeddedGrepSearch: TfmGrepSearch;
    FNewSortMode: TGrepHistorySort;
    FNewSortDesc: Boolean;
    FSearchInClearSearchList: Boolean;
    FSaveItemEmptyCaption: string;
    FSaveSortCaption: string;
    FSavedFormCaption: string;
    FlbHistoryListIndexForHistoryMenuActions: Integer;
    procedure SetStayOnTop(Value: Boolean);
    procedure RefreshContextLines;
    procedure SetShowContext(Value: Boolean);
    procedure HighlightMemo(FileMatches: TFileResult; StartLine, MatchLineNo: Integer);
    procedure ReportProgress(const PathName: string);
    procedure StartDirectorySearch(Sender: TObject; const DirectoryName: string);
    procedure StartFileSearch(Sender: TObject; const FileName: string);
    procedure SaveSettings;
    procedure LoadSettings;
    function ResultListItemIsFileResult(ListBoxIndex: Integer): boolean;
    procedure ToggleFileResultExpanded(ListBoxIndex: Integer; const AExpandMode: TExpandMode = emToggle);
    procedure ExpandList(AUsedState, ASetState: Boolean; AExpandFewLines: Integer);
    procedure ContractList(ASetState: Boolean);
    procedure ResizeListBox;
    procedure GotoHighlightedListEntry;
    procedure ClearResultsListbox;
    function ShowModalForm(Dlg: TCustomForm): TModalResult;
    function QueryUserForGrepOptions(AState: TGrepSearchState): Boolean;
    function QueryUserForReplaceOptions(const ReplaceInString: string): Boolean;
    procedure Abort;
    procedure SetStatusString(const StatusStr: string);
    procedure SetMatchString(const MatchStr: string);
    function DoingSearchOrReplace: Boolean;
    procedure ExpandOrContractList(Expand, UsedState, SetState: Boolean; AExpandFewLines: Integer);
    function GetStayOnTop: Boolean;
    procedure ResizeStatusBar;
    procedure RefreshInformation(AMatchesFound: Integer; ADoExpand, AUSedExpandState, ASetExpandState: Boolean);
    procedure ViewHistoryListItems(AIndex: Integer; AUsedExpandState: Boolean);
    function SelectNextListItem: boolean;
    function SelectPrevListItem: boolean;
    procedure SetShowHistoryList(const Value: Boolean);
    procedure SetShowFullFilename(const Value: Boolean);
    procedure SetShowLineIndent(const Value: Boolean);
    procedure RefreshHistoryView(DoUpdateIndex: Boolean);
    procedure SetHistoryListMode(ANewMode: TGrepHistoryListMode; DoRefresh, DoSaveIndex, DoUpdateIndex: Boolean;
      AddIf: Boolean = False; AConditionMode: TGrepHistoryListMode = hlmSettings);
    function  GetSavedHistoryIndex: Integer;
    procedure SetSavedHistoryIndexes(AIndex: Integer);
    function  HistoryListSelect(ASelectType: TGrepSelectType; const ACaption: string): TGrepSelectResult;
    procedure ClearResultsData;
    procedure DoEmbeddedSearch(Sender: TObject);
    procedure UpdateHistoryPagesOptions;
    procedure GoToMatchLine(MatchLine: TLineResult);
    procedure ForceRedraw;
    procedure GetEnabledFlags(out _IsOnlySaveSettings, _HaveItems, _Processing: Boolean);
    ///<summary>
    /// Gets and sets the value of FLoadedContextHeightRelative taking the current height and
    /// and the factor 10000 into account. </summary>
    function GetLoadedContextHeight: integer;
    procedure SetLoadedContextHeight(_Value: integer);
    procedure UpdateContextPanelHeight;
    property lbHistoryListIndexForHistoryMenuActions: Integer
      read FlbHistoryListIndexForHistoryMenuActions write FlbHistoryListIndexForHistoryMenuActions;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure AssignSettingsToForm;
    function ConfigurationKey: string;
{$IFDEF IDE_IS_HIDPI_AWARE}
    procedure ApplyDpi(_NewDpi: Integer; _NewBounds: PRect); override;
    procedure ArrangeControls;  override;
{$ENDIF}
public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure InitGrepSettings(AGrepSettings: TGrepSettings);
    function  Execute(AState: TGrepSearchState): Boolean;
    procedure UpdateFromSettings;
    procedure InternalSaveSettings(_Settings: IExpertSettings);
    property StayOnTop: Boolean read GetStayOnTop write SetStayOnTop;
    property ShowContext: Boolean read FShowContext write SetShowContext;
    property ShowFullFilename: Boolean read FShowFullFilename write SetShowFullFilename;
    property ShowLineIndent: Boolean read FShowLineIndent write SetShowLineIndent;
    property ShowHistoryList: Boolean read FShowHistoryList write SetShowHistoryList;
    property DoSearchReplace: Boolean read FDoSearchReplace write FDoSearchReplace;
    property ContextSearchText: string read FContextSearchText;
    property GrepSettings: TGrepSettings read FGrepSettings;
  end;

var
  fmGrepResults: TfmGrepResults = nil;

implementation

{$R *.dfm}

uses
  SysUtils, Messages, Math, StrUtils, IniFiles, TypInfo, Contnrs, Clipbrd, DateUtils,
  {$IFOPT D+} GX_DbugIntf, {$ENDIF D+}
  u_dzVclUtils,
  GX_GExperts, GX_GenericUtils, GX_StringList, GX_OtaUtils, GX_GxUtils, GX_IdeUtils, GX_MessageBox,
  GX_GrepPrinting, GX_Replace, GX_GrepReplace, GX_GrepSelect, GX_GrepProgress;

resourcestring
  SGrepReplaceStats = 'Replaced %d occurrence(s) in %.2f seconds';

const  //do not localize
  cKeyPageIndexType: array[TPageSavedIndexType] of string = ('Last', 'Top');
  cKeyPageIndexMode: array[TGrepHistoryListMode] of string = (
    'ViewedResultsItem',
    'ViewedSettingsItem',
    'ViewedItem',
    'ViewedSearchItems');

type
  TShowUnicodeReplaceMessage = class(TGxMsgBoxAdaptor)
  protected
    function GetMessage: string; override;
    function ShouldShow: Boolean; override;
  end;

  TGxResultRefreshSelectedQuestion = class(TGxQuestionBoxAdaptor)
  protected
    function GetMessage: string; override;
  end;

  TGxResultDeleteSelectedQuestion = class(TGxQuestionBoxAdaptor)
  protected
    function GetMessage: string; override;
  end;

procedure TfmGrepResults.GoToMatchLine(MatchLine: TLineResult);
resourcestring
  SCouldNotOpenFile = 'Could not open file %s';
var
  MatchFileName: string;
  Params: string;
begin
  MatchFileName := TFileResult(MatchLine.Collection).FileName;

  if IsStandAlone then begin
    if gblGrepExpert.ExternalEditor = '' then begin
      GXShellExecute(MatchFileName, '', True);
    end else begin
      Params := gblGrepExpert.ExternalEditorParams;
      if Params = '' then
        Params := '{FILE}';
      Params := StringReplace(Params, '{FILE}', AnsiQuotedStr(MatchFileName, '"'), [rfReplaceAll, rfIgnoreCase]);
      Params := StringReplace(Params, '{LINE}', IntToStr(MatchLine.LineNo), [rfReplaceAll, rfIgnoreCase]);
      Params := StringReplace(Params, '{COLUMN}', IntToStr(MatchLine.Matches[0].SPos), [rfReplaceAll, rfIgnoreCase]);
      GXShellExecute(gblGrepExpert.ExternalEditor, Params, True);
    end;
  end else
    GxOtaGoToFileLineColumn(MatchFileName, MatchLine.LineNo, MatchLine.Matches[0].SPos,
      MatchLine.Matches[0].EPos, gblGrepExpert.GrepMiddle);
end;

{ TShowUnicodeReplaceMessage }

function TShowUnicodeReplaceMessage.GetMessage: string;
begin
  Result := 'Using GExperts grep replace can corrupt your source code if the file ' +
    'being replaced is in a UNICODE format on disk or if the file is loaded into the IDE ' +
    'editor and contains characters other than low ASCII.  Canceling the replace ' +
    'is recommended if any of your files fall into one of these categories.';
end;

function TShowUnicodeReplaceMessage.ShouldShow: Boolean;
begin
  Result := RunningDelphi8OrGreater;
end;

{ TGxResultRefreshSelectedQuestion }

function TGxResultRefreshSelectedQuestion.GetMessage: string;
begin
  if FData = 'A' then
    Result := 'Are you sure you want to refresh all search history items?'
  else
    Result := 'Are you sure you want to refresh selected search history items?';
end;

{ TGxResultDeleteSelectedQuestion }

function TGxResultDeleteSelectedQuestion.GetMessage: string;
begin
  if FData = 'A' then
    Result := 'Are you sure you want to delete all search history items?'
  else
    Result := 'Are you sure you want to delete selected search history items?';
end;

{ TfmGrepResults }

procedure TfmGrepResults.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #27 then
  begin
    if FSearchInProgress then
      Self.Abort
    else
    begin
      if IsStandAlone then
        ModalResult := mrCancel
      else
        Hide;
    end;
  end;
end;

procedure TfmGrepResults.ReportProgress(const PathName: string);
resourcestring
  SProcessing = 'Processing %s';
var
  Dummy: Boolean;
  CurrentGetTickCount: DWORD;
begin
  SetStatusString(Format(SProcessing, [PathName]));
  TheActionListUpdate(nil, Dummy);
  CurrentGetTickCount := GetTickCount;
  if CurrentGetTickCount <> FLastRepaintTick then
  begin
    Application.ProcessMessages;
    FLastRepaintTick := CurrentGetTickCount;
  end;
end;

procedure TfmGrepResults.StartDirectorySearch(Sender: TObject; const DirectoryName: string);
begin
  ReportProgress(DirectoryName);
end;

procedure TfmGrepResults.StartFileSearch(Sender: TObject; const FileName: string);
begin
  ReportProgress(FileName);
end;

function TfmGrepResults.Execute(AState: TGrepSearchState): Boolean;
resourcestring
  SGrepActive = 'A Grep search is currently active.  Please abort it or wait until it is finished.';
  SGrepSearchStats = 'Searched %d files in %.2f seconds for "%s"';
var
  TimeStart: TDateTime;
  FilesSearched: Cardinal;
  MatchesFound: Cardinal;
  ResultFiles: TStringList;
  I : Integer;
  AItemIndex, ATopIndex : Integer;
begin
  Result := False;
  if FSearchInProgress then
    raise Exception.Create(SGrepActive);

  gblGrepExpert.IncCallCount;
  if (AState in [gssNormal, gssSearchAgain, gssModifySearchSettings]) or not FGrepSettings.CanRefresh then
    if not QueryUserForGrepOptions(AState) then
      Exit;

  Result := True;

  if AState = gssModifySearchSettings then
  begin
    gblGrepExpert.HistoryList.UpdateGrepSettings(FGrepSettings);
    lbHistoryList.Refresh;
    Exit;
  end;

  FLastRepaintTick := GetTickCount;
  reContext.Clear;

  ResultFiles := TStringList.Create;
  try
    ContractList(False);
    for i := 0 to lbResults.Items.Count - 1 do
      ResultFiles.Add(lbResults.Items[i]);
    SetStatusString('');
    SetMatchString('');
    ClearResultsListbox;
    BringToFront;
    IdeDockManager.ShowForm(Self);
    EnsureFormVisible(Self);

    TimeStart := Now;
    TCursor_TempHourglass;

    FSearcher := TGrepSearchRunner.Create(FGrepSettings, lbResults.Items, ResultFiles);
    try
      FSearcher.OnSearchFile := StartFileSearch;
      FSearcher.OnSearchDirectory := StartDirectorySearch;
      FSearchInProgress := True;
      FSearcher.Execute;
      FilesSearched := FSearcher.FileSearchCount;
      MatchesFound := FSearcher.MatchCount;
    finally
      FreeAndNil(FSearcher);
      FSearchInProgress := False;
    end;
  finally
    FreeAndNil(ResultFiles);
  end;

  SetStatusString(Format(SGrepSearchStats, [FilesSearched, (Now - TimeStart) * 24*60*60, FGrepSettings.Pattern]));

  lbResults.Sorted := True;  // There is no Sort method
  lbResults.Sorted := False; //FI:W508 - sort once but do not keep sorted

  if gblGrepExpert.HistoryList.ListMode = hlmSettings then
    SetSavedHistoryIndexes(lbHistoryList.ItemIndex + 1);

  SetHistoryListMode(hlmResults, False, False, False, True);

  AItemIndex := gblGrepExpert.HistoryList.AddItem(FGrepSettings, lbResults.Items, TimeStart);
  gblGrepExpert.HistoryListSaveSettings(AItemIndex);

  ATopIndex := lbHistoryList.TopIndex;
  lbHistoryList.Count := gblGrepExpert.HistoryList.Count;
  lbHistoryList.ItemIndex := AItemIndex;
  if (ATopIndex < lbHistoryList.Count) and (AItemIndex < lbHistoryList.Count-1) then
    lbHistoryList.TopIndex := ATopIndex;
  SetSavedHistoryIndexes(AItemIndex);

  lbHistoryList.Refresh;

  RefreshInformation(MatchesFound, gblGrepExpert.GrepExpandAll, False, True);

  if Assigned(FEmbeddedGrepSearch) then
    FEmbeddedGrepSearch.Hide;
end;

procedure TfmGrepResults.RefreshHistoryView(DoUpdateIndex: Boolean);
begin
  lbHistoryList.Items.BeginUpdate;
  try
    lbHistoryList.Count := gblGrepExpert.HistoryList.Count;
//    lbHistoryList.Refresh;
    if DoUpdateIndex then
      GetSavedHistoryIndex;
  finally
    lbHistoryList.Items.EndUpdate;
  end;
end;

function TfmGrepResults.SelectNextListItem: boolean;
var
  liIx: integer;
begin
  result := false;
  liIx := lbResults.ItemIndex + 1;
  if ResultListItemIsFileResult(liIx) then
  begin
    ToggleFileResultExpanded(liIx, emExpand);
    Inc(liIx);
  end;
  if liIx >= lbResults.Items.Count then Exit;
  lbResults.ItemIndex := liIx;
  result := true;
end;

function TfmGrepResults.SelectPrevListItem: boolean;
var
  liIx, liOrgListCnt: integer;
begin
  liIx := lbResults.ItemIndex - 1;
  if ResultListItemIsFileResult(liIx) then
  begin
    liOrgListCnt := lbResults.Items.Count;
    ToggleFileResultExpanded(liIx, emExpand);
    Inc(liIx, lbResults.Items.Count - liOrgListCnt);
  end;
  lbResults.ItemIndex := Max(liIx, -1);
  result := lbResults.ItemIndex >= 0;
end;

procedure TfmGrepResults.SetHistoryListMode(ANewMode: TGrepHistoryListMode;
  DoRefresh, DoSaveIndex, DoUpdateIndex, AddIf: Boolean; AConditionMode: TGrepHistoryListMode);
begin
  if not AddIf or (gblGrepExpert.HistoryList.ListMode = AConditionMode) then
  begin
    if DoSaveIndex then
      SetSavedHistoryIndexes(lbHistoryList.ItemIndex);
    gblGrepExpert.HistoryList.ListMode := ANewMode;
    tcHistoryListPage.TabIndex := Integer(ANewMode);
    //Caption := FSavedFormCaption + Format(' [%s page]', [tcHistoryListPage.Tabs[Integer(ANewMode)]]);
    if DoRefresh then
      RefreshHistoryView(DoUpdateIndex);
  end;
end;

function TfmGrepResults.GetSavedHistoryIndex: Integer;
var
  APageIndexes: TPageIndexes;
  ATopIndex: Integer;
begin
  APageIndexes := FPageIndexes[gblGrepExpert.HistoryList.ListMode];
  Result := gblGrepExpert.HistoryList.ItemIndexByKeyIndex(APageIndexes[pitClickedEntryKeyIndex]);
  if Result = -1 then
    Result := APageIndexes[pitClickedEntryItemIndex];
  ATopIndex := gblGrepExpert.HistoryList.ItemIndexByKeyIndex(APageIndexes[pitTopKeyIndex]);
  if ATopIndex = -1 then
    ATopIndex := APageIndexes[pitTopItemIndex];

  if Result <> -1 then
    lbHistoryList.ItemIndex := Result
  else
  begin
    lbHistoryList.ItemIndex := lbHistoryList.Count-1;
    Result := lbHistoryList.ItemIndex;
  end;
  if (ATopIndex <> -1) and (ATopIndex <= Result) then
    lbHistoryList.TopIndex := ATopIndex;

  ViewHistoryListItems(lbHistoryList.ItemIndex, True);
end;

procedure TfmGrepResults.SetSavedHistoryIndexes(AIndex: Integer);
var
  AItem: TGrepHistoryListItem;
  APageIndexes: TPageIndexes;
begin
  APageIndexes[pitClickedEntryItemIndex] := AIndex;
  AItem := gblGrepExpert.HistoryList.Items[AIndex];
  if Assigned(AItem) then
    APageIndexes[pitClickedEntryKeyIndex] := AItem.KeyIndex
  else
    APageIndexes[pitClickedEntryKeyIndex] := -1;

  APageIndexes[pitClickedEntryItemIndex] := lbHistoryList.TopIndex;
  AItem := gblGrepExpert.HistoryList.Items[lbHistoryList.TopIndex];
  if Assigned(AItem) then
    APageIndexes[pitTopKeyIndex] := AItem.KeyIndex
  else
    APageIndexes[pitTopKeyIndex] := -1;

  FPageIndexes[gblGrepExpert.HistoryList.ListMode] := APageIndexes;
end;

procedure TfmGrepResults.RefreshInformation(AMatchesFound: Integer; ADoExpand, AUSedExpandState, ASetExpandState: Boolean);
resourcestring
  SMatches = '%d matches';
  SMatches1 = '%d match';
  SFiles = 'in %d files';
  SFiles1 = 'in %d file';
var
  MatchString: string;
  FilesHit: Integer;
begin
  FilesHit := lbResults.Items.Count;

  if (lbResults.Items.Count = 1) or ADoExpand or gblGrepExpert.GrepExpandFew or
    (gblGrepExpert.GrepExpandIf and
      (AMatchesFound <= gblGrepExpert.GrepExpandIfMatches) and (FilesHit <= gblGrepExpert.GrepExpandIfFiles)
    )
  then
  begin
    lbResults.ItemIndex := 0;
    ExpandList(AUSedExpandState, ASetExpandState, IfThen(gblGrepExpert.GrepExpandFew, gblGrepExpert.GrepExpandFewLines));
  end;

  lbResults.Refresh;

  if AMatchesFound = 1 then
    MatchString := Format(SMatches1, [AMatchesFound])
  else
    MatchString := Format(SMatches, [AMatchesFound]);

  if FilesHit = 1 then
    MatchString := MatchString + ' ' + Format(SFiles1, [FilesHit])
  else
    MatchString := MatchString + ' ' + Format(SFiles, [FilesHit]);

  SetMatchString(MatchString);
end;

procedure TfmGrepResults.tcHistoryListPageChange(Sender: TObject);
var
  ANewMode: TGrepHistoryListMode;
begin
  ANewMode := TGrepHistoryListMode(tcHistoryListPage.TabIndex);
  if gblGrepExpert.HistoryList.ListMode <> ANewMode then
    SetHistoryListMode(ANewMode, True, True, True);
end;

procedure TfmGrepResults.ClearResultsListbox;
begin
  lbResults.Clear;
end;

procedure TfmGrepResults.lbResultsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
  DragTreshold: Integer;
  Temp: TObject;
begin
  DragTreshold := Mouse.DragThreshold;

  // Make sure mouse has moved at least DragTreshold pixels before starting drag ...
  if (FDragPoint.X = -1) or ((Shift <> [ssLeft]) and (Shift <> [ssRight])) or
    ((Abs(FDragPoint.X - X) < DragTreshold) and (Abs(FDragPoint.Y - Y) < DragTreshold)) then
  begin
    Exit;
  end;

  i := lbResults.ItemAtPos(Point(X, Y), True);

  if i >= 0 then
  begin
    FDragSource.Files.Clear;

    Temp := lbResults.Items.Objects[i];
    if Temp is TFileResult then
      FDragSource.Files.Add(TFileResult(Temp).FileName)
    else
    begin
      Temp := lbResults.Items.Objects[i];
      if Temp is TLineResult then
      begin
        Temp := TLineResult(Temp).Collection;
        FDragSource.Files.Add((Temp as TFileResult).FileName);
      end
      else
        Assert(False, 'Internal Error');
    end;

    if FDragSource.Files.Count > 0 then
      FDragSource.Execute;
  end;
end;

procedure TfmGrepResults.lbResultsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FDragPoint := Point(X, Y);
end;

procedure TfmGrepResults.FormResize(Sender: TObject);
begin
  ResizeStatusBar;
  UpdateContextPanelHeight;
  if Assigned(FEmbeddedGrepSearch) then
    FEmbeddedGrepSearch.EmbeddedUpdatePos;
  ForceRedraw;
  lbResults.Refresh;
end;

procedure TfmGrepResults.InitGrepSettings(AGrepSettings: TGrepSettings);
begin
  FGrepSettings := AGrepSettings;
end;

function ForceBetween(_Min, _Max, _Value: Integer): Integer;
begin
  Result := Max(_Min, Min(_Max, _Value));
end;

procedure TfmGrepResults.InternalSaveSettings(_Settings: IExpertSettings);
var
  WindowSettings: IExpertSettings;
  PerTenThousand: Integer;
  IM: TGrepHistoryListMode;
  IT: TPageIndexType;
begin
  if not Assigned(gblGrepExpert) then
    Exit; //==>

  _Settings.SaveForm('Window', Self);
  WindowSettings := _Settings.Subkey('Window');
  WindowSettings.WriteBool('OnTop', StayOnTop);
  if IsStandAlone then
    WindowSettings.WriteBool('ShowToolBar', ToolBar.Visible);
  WindowSettings.WriteBool('ShowContext', ShowContext);
  WindowSettings.WriteBool('ShowHistoryList', ShowHistoryList);

  // this used to be a pixel value and later a percent value, now it is a per-10-thousand
  // value to account for higher resolution monitors and changing resolutions due to multiple
  // monitors and Remote Desktop
  if ShowContext then begin
    PerTenThousand := MulDiv(pnlBottom.Height, 10000, pnlMain.ClientHeight);
  end else begin
    // if the context is not visible, the bottom panel has the height of the status bar
    // but FLoadedContextHeightRelative contains its former height
    PerTenThousand := FLoadedContextHeightRelative;
  end;
  PerTenThousand := ForceBetween(1000, 5000, PerTenThousand);
  WindowSettings.WriteInteger('ContextHeightRelative', PerTenThousand);
  WindowSettings.DeleteKey('ContextHeightPercent');
  WindowSettings.DeleteKey('ContextHeight');

  if ShowHistoryList then begin
    PerTenThousand := MulDiv(tcHistoryListPage.Width, 10000, pnlMain.ClientWidth);
  end else begin
    PerTenThousand := FLoadedHistoryListWidthRelative;
  end;
  PerTenThousand := ForceBetween(1000, 5000, PerTenThousand);
  WindowSettings.WriteInteger('HistoryListWidthRelative', PerTenThousand);
  WindowSettings.DeleteKey('HistoryListWidth');

  WindowSettings.WriteBool('ShowFullFilename', ShowFullFilename);
  WindowSettings.WriteBool('ShowLineIndent', ShowLineIndent);

  for IM := Low(TGrepHistoryListMode) to High(TGrepHistoryListMode) do
    for IT := Low(TPageSavedIndexType) to High(TPageSavedIndexType) do
      WindowSettings.WriteInteger(cKeyPageIndexType[IT] + cKeyPageIndexMode[IM], FPageIndexes[IM, IT]);

  WindowSettings.WriteInteger('HistoryListPage', tcHistoryListPage.TabIndex);
end;

procedure TfmGrepResults.SaveSettings;
var
  Settings: IExpertSettings;
begin
  Settings := ConfigInfo.GetExpertSettings(ConfigurationKey);
  InternalSaveSettings(Settings);
end;

procedure TfmGrepResults.Loaded;
var
  PropInfo: PPropInfo;
  i: Integer;
  cmp: TComponent;
begin
  inherited Loaded;
  PropInfo := GetPropInfo(Self, 'StyleElements');
  if Assigned(PropInfo) then
    SetOrdProp(Self, PropInfo, 0);
  for I := 0 to ComponentCount - 1 do begin
    cmp := Components[I];
    PropInfo := GetPropInfo(cmp, 'StyleElements');
    if Assigned(PropInfo) then
      SetOrdProp(cmp, PropInfo, 0);
  end;
end;

procedure TfmGrepResults.LoadSettings;
var
  WindowSettings: IExpertSettings;
  ExpSettings: IExpertSettings;
  AHistoryIniVersion: Integer;
  IM: TGrepHistoryListMode;
  IT: TPageIndexType;
begin
  // Do not localize any of the below strings.
  ExpSettings := ConfigInfo.GetExpertSettings(ConfigurationKey);
  AHistoryIniVersion := ExpSettings.ReadInteger('HistoryIniVersion', 0);

  ExpSettings.LoadForm('Window', Self);
  WindowSettings := ExpSettings.Subkey('Window');
  EnsureFormVisible(Self);
  StayOnTop := WindowSettings.ReadBool('OnTop', True);
  if IsStandAlone then
    ToolBar.Visible := WindowSettings.ReadBool('ShowToolBar', ToolBar.Visible)
  else
    ToolBar.Visible := True;

  ShowContext := WindowSettings.ReadBool('ShowContext', True);

  for IM := Low(TGrepHistoryListMode) to High(TGrepHistoryListMode) do
    for IT := Low(TPageSavedIndexType) to High(TPageSavedIndexType) do
      FPageIndexes[IM, IT] := WindowSettings.ReadInteger(cKeyPageIndexType[IT] + cKeyPageIndexMode[IM], FPageIndexes[IM, IT]);

  if AHistoryIniVersion = 0 then begin
    ShowHistoryList := WindowSettings.ReadBool('ShowFoundList', True);
    FLoadedHistoryListWidthRelative := WindowSettings.ReadInteger('FoundListWidth', tcHistoryListPage.Width);
    FLoadedHistoryListWidthRelative := MulDiv(FLoadedHistoryListWidthRelative, 10000, pnlMain.ClientWidth);
  end else begin
    ShowHistoryList := WindowSettings.ReadBool('ShowHistoryList', True);
    if WindowSettings.ValueExists('HistoryListWidthRelative') then
      FLoadedHistoryListWidthRelative := WindowSettings.ReadInteger('HistoryListWidthRelative', 2000)
    else begin
      FLoadedHistoryListWidthRelative := WindowSettings.ReadInteger('HistoryListWidth', tcHistoryListPage.Width);
      FLoadedHistoryListWidthRelative := MulDiv(FLoadedHistoryListWidthRelative, 10000, pnlMain.ClientWidth);
    end;
  end;
  FLoadedHistoryListWidthRelative := ForceBetween(1000, 5000, FLoadedHistoryListWidthRelative);

  // this used to be a pixel value and later a percent value, now it is a per-10-thousand
  // value to account for higher resolution monitors and changing resolutions due to multiple
  // monitors and Remote Desktop
  if WindowSettings.ValueExists('ContextHeightRelative') then begin
    FLoadedContextHeightRelative := WindowSettings.ReadInteger('ContextHeightRelative', 2000);
  end else if WindowSettings.ValueExists('ContextHeightPercent') then begin
    FLoadedContextHeightRelative := WindowSettings.ReadInteger('ContextHeightPercent', 20) * 100;
  end else if WindowSettings.ValueExists('ContextHeight') then begin
    FLoadedContextHeightRelative := WindowSettings.ReadInteger('ContextHeight', pnlBottom.Height);
    FLoadedContextHeightRelative := MulDiv(FLoadedContextHeightRelative, 10000, pnlMain.ClientHeight);
  end else begin
    FLoadedContextHeightRelative := 2000;
  end;
  FLoadedContextHeightRelative := ForceBetween(1000, 5000, FLoadedContextHeightRelative);

  ShowFullFilename := WindowSettings.ReadBool('ShowFullFilename', False);
  ShowLineIndent := WindowSettings.ReadBool('ShowLineIndent', False);

  FLoadHistoryListPage := WindowSettings.ReadInteger('HistoryListPage', tcHistoryListPage.TabIndex);
end;

procedure TfmGrepResults.UpdateFromSettings;
begin
  FEmbeddedGrepSearch := TfmGrepSearch.Create(pnlMain);
  FEmbeddedGrepSearch.EmbeddedInit(lbResults, DoEmbeddedSearch);

  gblGrepExpert.HistoryList.Enabled := ShowHistoryList;
  if ShowContext then
    pnlBottom.Height := GetLoadedContextHeight
  else
    pnlBottom.ClientHeight := StatusBar.Height;

  if FShowHistoryList then
  begin
    UpdateHistoryPagesOptions;
    if gblGrepExpert.GrepSaveHistoryListItems then
    begin
      tcHistoryListPage.Width := MulDiv(FLoadedHistoryListWidthRelative, pnlMain.ClientWidth, 10000);
      if gblGrepExpert.GrepHistoryListDefaultPage < tcHistoryListPage.Tabs.Count then
        FLoadHistoryListPage := gblGrepExpert.GrepHistoryListDefaultPage;
      SetHistoryListMode(TGrepHistoryListMode(FLoadHistoryListPage), True, False, True);
      SetSavedHistoryIndexes(lbHistoryList.ItemIndex);
    end;
  end;
end;

procedure TfmGrepResults.UpdateHistoryPagesOptions;
begin
  tcHistoryListPage.MultiLine := gblGrepExpert.GrepHistoryPagesTabMultiline;
  if not gblGrepExpert.GrepHistoryPagesTabMultiline then
    tcHistoryListPage.TabWidth := gblGrepExpert.GrepHistoryPagesTabWidth;
end;

procedure TfmGrepResults.lbResultsClick(Sender: TObject);
var
  AItem: TGrepHistoryListItem;
begin
  AItem := gblGrepExpert.HistoryList.Items[lbHistoryList.ItemIndex];
  if Assigned(AItem) and AItem.IsOnlySaveSettings then
    Exit;

  RefreshContextLines;
end;

procedure TfmGrepResults.actShowMatchContextExecute(Sender: TObject);
begin
  ShowContext := not ShowContext;
end;

procedure TfmGrepResults.SetShowContext(Value: Boolean);
begin
  if (FShowContext = True) and (Value = False) then
    SetLoadedContextHeight(pnlBottom.Height);
  FShowContext := Value;
  reContext.Visible := Value;
  SplitterContext.Visible := Value;

  UpdateContextPanelHeight;

  RefreshContextLines;
end;

procedure TfmGrepResults.UpdateContextPanelHeight;
begin
  if ShowContext then
    pnlBottom.Height := GetLoadedContextHeight
  else begin
    pnlBottom.ClientHeight := StatusBar.Height;
  end;
end;

procedure TfmGrepResults.RefreshContextLines;
resourcestring
  SMatchContextNotAvail = 'Unable to load match context lines';
var
  CurrentLine: TLineResult;
  CurrentFile: TFileResult;
  MatchLineNo, BeginLineNo, EndLineNo, REMatchLineNo: Integer;
  FileLines: TGXUnicodeStringList;
  FileName: string;
  i: Integer;
begin
  if not ShowContext then
    Exit;

  reContext.Lines.BeginUpdate;
  try
    reContext.Clear;
    if (lbResults.ItemIndex < 0) then
      Exit;
    if (ShowContext) and (gblGrepExpert.NumContextLines > 0) then
    begin
      if (lbResults.Items.Objects[lbResults.ItemIndex] is TLineResult) then
      begin
        CurrentLine := TLineResult(lbResults.Items.Objects[lbResults.ItemIndex]);
        CurrentFile := TFileResult(CurrentLine.Collection);
        FileName := CurrentFile.FileName;
        SetStatusString(CurrentFile.RelativeFileName);

        MatchLineNo := CurrentLine.LineNo - 1;

        FileLines := TGXUnicodeStringList.Create;
        try
          try
            GxOtaLoadFileToUnicodeStrings(FileName, FileLines);
          except
            on E: EGXFileNotFound do
            begin
              reContext.Lines.Text := E.Message;
              Exit;
            end;
          end;
          if FileLines.Count < 1 then
          begin
            reContext.Lines.Text := SMatchContextNotAvail;
            Exit;
          end;

          BeginLineNo := MatchLineNo - gblGrepExpert.NumContextLines;
          BeginLineNo := Max(BeginLineNo, 0);
          EndLineNo := MatchLineNo + gblGrepExpert.NumContextLines;
          EndLineNo := Min(EndLineNo, FileLines.Count - 1);

          REMatchLineNo := 0;
          reContext.SelStart := reContext.GetTextLen;
          for i := BeginLineNo to EndLineNo do
          begin
            reContext.SelText := FileLines[i] + IfThen(i <> EndLineNo, sLineBreak);
            if i = MatchLineNo then
              REMatchLineNo := reContext.Lines.Count - 1;
          end;
        finally
          FreeAndNil(FileLines);
        end;
        HighlightMemo(TFileResult(CurrentLine.Collection), BeginLineNo, REMatchLineNo);
        CenterLineInEdit(reContext, REMatchLineNo)
      end;
    end;
  finally
    reContext.Lines.EndUpdate;
  end;
end;

// Make any matches found in the context lines bold
// Also highlight the current match line using clHighlightText
procedure TfmGrepResults.HighlightMemo(FileMatches: TFileResult; StartLine, MatchLineNo: Integer);
var
  Matches: TMatchArray;
  i, j: Integer;
begin
  reContext.SelStart := 0;
  reContext.SelLength := Length(reContext.Lines.Text);
  reContext.SelAttributes.Name := reContext.DefAttributes.Name;
  reContext.SelAttributes.Size := reContext.DefAttributes.Size;
  reContext.SelAttributes.Style := [];

  // Highlight the matched line
  reContext.SelStart := reContext.Perform(EM_LINEINDEX, MatchLineNo, 0);
  reContext.SelLength := Length(reContext.Lines[MatchLineNo]);
  reContext.SelAttributes.Color := gblGrepExpert.ContextMatchLineColor;

  for i := StartLine + 1 to StartLine + reContext.Lines.Count + 1 do
  begin
    FileMatches.GetMatchesOnLine(i, Matches);
    for j := 0 to Length(Matches) - 1 do
    begin
      if Matches[j].ShowBold then
      begin
        reContext.SelStart := reContext.Perform(EM_LINEINDEX, i - StartLine - 1, 0) + Matches[j].SPos - 1;
        reContext.SelLength := Matches[j].EPos - Matches[j].SPos + 1;
        reContext.SelAttributes.Color := gblGrepExpert.ContextMatchColor;
        reContext.SelAttributes.Style := [fsBold];
      end;
    end;
  end;
  reContext.SelStart := 0;
end;

procedure TfmGrepResults.lbResultsMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ClickedEntry: Integer;
begin
  if Button = mbLeft then
  begin
    ClickedEntry := lbResults.ItemAtPos(Point(X, Y), True);
    if (ClickedEntry <> -1) and
       (lbResults.Items.Objects[ClickedEntry] is TFileResult) then
    begin
      ToggleFileResultExpanded(ClickedEntry);
    end;
  end;
end;

procedure TfmGrepResults.ToggleFileResultExpanded(ListBoxIndex: Integer; const AExpandMode: TExpandMode);
var
  AFileResult: TFileResult;
  i: Integer;
begin
  if FSearchInProgress or
     (ListBoxIndex < 0) or (ListBoxIndex >= lbResults.Items.Count) then
  begin
    Exit;
  end;

  if lbResults.Items.Objects[ListBoxIndex] is TFileResult then
  begin
    AFileResult := TFileResult(lbResults.Items.Objects[ListBoxIndex]);

    lbResults.Items.BeginUpdate;
    try
      if AFileResult.Expanded and (AExpandMode in [emToggle, emContract]) then
      begin
        while (ListBoxIndex + 1 <= lbResults.Items.Count - 1) and
              (not (lbResults.Items.Objects[ListBoxIndex + 1] is TFileResult)) do
        begin
          lbResults.Items.Delete(ListBoxIndex + 1);
        end;
        AFileResult.Expanded := False;
        AFileResult.ExpandState := False;
      end
      else if not AFileResult.Expanded and (AExpandMode in [emToggle, emExpand]) then
      begin
        for i := AFileResult.Count - 1 downto 0 do
          lbResults.Items.InsertObject(ListBoxIndex + 1, AFileResult.Items[i].Line, AFileResult.Items[i]);
        AFileResult.Expanded := True;
        AFileResult.ExpandState := True;
      end
    finally
      lbResults.Items.EndUpdate;
    end;
  end;
end;

procedure TfmGrepResults.lbResultsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  i: Integer;
begin
  lbResults.Items.BeginUpdate;
  try
    i := lbResults.ItemIndex;
    if (i < 0) then
      Exit;

    // Search for a TFileResult above the selected line
    while (i > 0) and not (lbResults.Items.Objects[i] is TFileResult) do
      Dec(i);

    if not (lbResults.Items.Objects[i] is TFileResult) then
      Exit;

    if  (Key in [VK_LEFT, VK_RIGHT])
      and (TFileResult(lbResults.Items.Objects[i]).Expanded = (Key = VK_LEFT)) then
    begin
      lbResults.ItemIndex := i;
      ToggleFileResultExpanded(i);
      Key := 0;
    end;

  finally
    lbResults.Items.EndUpdate;
  end;
end;

procedure TfmGrepResults.lbResultsKeyPress(Sender: TObject; var Key: Char);
begin
  case Key of
    '+', '-':
      ToggleFileResultExpanded(lbResults.ItemIndex);
    #13:
      GotoHighlightedListEntry;
  end;
end;

procedure TfmGrepResults.ResizeListBox;
begin
  lbResults.Canvas.Font.Assign(lbResults.Font);
  lbResults.ItemHeight := lbResults.Canvas.TextHeight(SAllAlphaNumericChars) + 3;
  lbResults.Refresh;
end;

procedure TfmGrepResults.lbResultsDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  ResultsCanvas: TCanvas;
  LineText: string;
resourcestring
  SItemMatch = '%5d matches';

  procedure PaintFileHeader;
  var
    TopColor: TColor;
    BottomColor: TColor;
    i: Integer;
    FileNameWidth: Integer;
    FileString: string;
    FileResult: TFileResult;
  begin
    TopColor := clBtnHighlight;
    BottomColor := clBtnShadow;

    FileResult := TFileResult(lbResults.Items.Objects[Index]);
    // Paint an expandable search file header (gray)
    ResultsCanvas.Brush.Color := clBtnFace;
    ResultsCanvas.Font.Color := clBtnText;
    ResultsCanvas.FillRect(Rect);

    Rect.Right := Rect.Right + 2;
    if odSelected in State then
      Frame3D(ResultsCanvas, Rect, BottomColor, TopColor, 1)
    else
      Frame3D(ResultsCanvas, Rect, TopColor, BottomColor, 1);

    i := ResultsCanvas.TextWidth('+');
    if ShowFullFilename then
      FileString := FileResult.FileName
    else
      FileString := FileResult.RelativeFileName;
    ResultsCanvas.TextOut(Rect.Left + i + 8, Rect.Top, FileString);

    if FileResult.Expanded then
      ResultsCanvas.TextOut(Rect.Left + 3, Rect.Top, '-')
    else
      ResultsCanvas.TextOut(Rect.Left + 3, Rect.Top, '+');

    LineText := Format(SItemMatch, [FileResult.TotalMatches]);

    FileNameWidth := ResultsCanvas.TextWidth(LineText) + 10;
    if (ResultsCanvas.TextWidth(FileString) + i + 10) <= Rect.Right - FileNameWidth then
      ResultsCanvas.TextOut(lbResults.ClientWidth - FileNameWidth, Rect.Top, LineText);
  end;

  procedure PaintLineMatch;
  var
    i: Integer;
    ALineResult: TLineResult;
    LineNoWidth: Integer;
    PaintText: string;
    Trimmed: Integer;
    TextTop: Integer;
    sb: TColor;
    sf: TColor;
    nb: TColor;
    nf: TColor;
    Match: TMatchResult;
    NextMatchStart: Integer;
    PrevMatchEnd: Integer;
    LineMatches: TLineMatches;
  begin
    // Paint a search match line number and highlighted match
    ALineResult := lbResults.Items.Objects[Index] as TLineResult;

    if odSelected in State then
    begin
      if gblGrepExpert.ListUseDefaultColors then
      begin
        nb := clHighLight;
        nf := clHighLightText;
        sb := clWindow;
        sf := clWindowText;
      end
      else
      begin
        nb := gblGrepExpert.ListMatchBrushColor;
        nf := gblGrepExpert.ListMatchTextColor;
        sb := clWindow;
        sf := gblGrepExpert.ListFont.Color;
      end;
    end
    else
    begin
      if gblGrepExpert.ListUseDefaultColors then
      begin
        sb := clHighLight;
        sf := clHighLightText;
        nb := clWindow;
        nf := clWindowText;
      end
      else
      begin
        sb := gblGrepExpert.ListMatchBrushColor;
        sf := gblGrepExpert.ListMatchTextColor;
        nb := clWindow;
        nf := gblGrepExpert.ListFont.Color;
      end;
    end;

    // Paint line number of match line
    TextTop := Rect.Top + 1;
    ResultsCanvas.Brush.Color := nb;
    ResultsCanvas.Font.Color := nf;
    ResultsCanvas.FillRect(Rect);
    ResultsCanvas.TextOut(Rect.Left + 10, TextTop, IntToStr(ALineResult.LineNo));

    LineNoWidth := 60;
    LineText := lbResults.Items[Index];

    if not FShowLineIndent then
    begin
      LineMatches := ALineResult.Matches;
      if Assigned(LineMatches) and (LineMatches.Count > 0) then
      begin
        // Avoid trimming inside the first match :
        Trimmed := 0;
        while (Length(LineText) > Trimmed)
          and CharInSet(LineText[Trimmed + 1], [#9, #32])
          and (Trimmed < LineMatches[0].SPos - 1) do
            Inc(Trimmed);

        if Trimmed > 0 then
          Delete(LineText, 1, Trimmed);
      end
      else
        Trimmed := LeftTrimChars(LineText);
    end
    else
      Trimmed := 0;

    PrevMatchEnd := 0;

    // Paint first match line up to first match character
    PaintText := Copy(LineText, 0, ALineResult.Matches[0].SPos - 1 - Trimmed);
    ResultsCanvas.TextOut(Rect.Left + LineNoWidth, TextTop, PaintText);

    // For each match, paint out the match and then the text until the next match
    for i := 0 to ALineResult.Matches.Count - 1 do
    begin
      // Paint the highlighted match
      Match := ALineResult.Matches[i];
      PaintText := Copy(LineText, Max(Match.SPos, PrevMatchEnd + 1) - Trimmed, Match.Length);
      ResultsCanvas.Font.Color := sf;
      ResultsCanvas.Brush.Color := sb;
      ResultsCanvas.TextOut(ResultsCanvas.PenPos.X + 1, TextTop, PaintText);
      PrevMatchEnd := Match.EPos;

      // Paint any non-matching text after the match
      if i = ALineResult.Matches.Count - 1 then
        NextMatchStart := Length(LineText) + Trimmed
      else
        NextMatchStart := ALineResult.Matches[i + 1].SPos - Trimmed - 1;
      PaintText := Copy(LineText, Match.EPos - Trimmed + 1, NextMatchStart - Match.EPos + Trimmed);
      ResultsCanvas.Font.Color := nf;
      ResultsCanvas.Brush.Color := nb;
      ResultsCanvas.TextOut(ResultsCanvas.PenPos.X + 1, TextTop, PaintText);
    end;
  end;

  procedure PaintOnlySaveSettings;
  begin
    ResultsCanvas.Brush.Color := clWindow;
    ResultsCanvas.Font.Color := clWindowText;
    ResultsCanvas.FillRect(Rect);

    ResultsCanvas.TextOut(Rect.Left + 3, Rect.Top + 1, lbResults.Items[Index]);
  end;

begin
  ResultsCanvas := lbResults.Canvas;
  if not Assigned(lbResults.Items.Objects[Index]) then
    PaintOnlySaveSettings
  else if lbResults.Items.Objects[Index] is TFileResult then
    PaintFileHeader
  else
    PaintLineMatch;
end;

procedure TfmGrepResults.SetStayOnTop(Value: Boolean);
begin
  // Stay on top hides some modal dialogs
  if (StayOnTop <> Value) and IsStandAlone then
  begin
    if Value then
      Self.FormStyle := fsStayOnTop
    else
      Self.FormStyle := fsNormal;
  end;
end;

procedure TfmGrepResults.actFileSearchExecute(Sender: TObject);
begin
  Execute(gssNormal);
end;

procedure TfmGrepResults.actFileRefreshExecute(Sender: TObject);
begin
  Execute(gssRefresh);
  RefreshContextLines;
end;

procedure TfmGrepResults.actFileAbortExecute(Sender: TObject);
begin
  {$IFOPT D+} SendDebug('Grep abort requested by user'); {$ENDIF}
  Self.Abort;
end;

procedure TfmGrepResults.actFilePrintExecute(Sender: TObject);
var
  AItem: TGrepHistoryListItem;
begin
  if (lbHistoryList.ItemIndex = -1) or (lbHistoryList.Count = 0) then
    Exit;

  AItem := gblGrepExpert.HistoryList.Items[lbHistoryList.ItemIndex];
  if not AItem.IsOnlySaveSettings then
  begin
    gblGrepExpert.HistoryList.ClearAllChecked;
    AItem.Checked := True;
    SaveGrepResultsToFile(Self, gblGrepExpert.HistoryList, gblGrepExpert.HistoryIniVersion, False,
      sfPrintToFile, grPrint);
  end;
end;

procedure TfmGrepResults.actListCopyExecute(Sender: TObject);
var
  AItem: TGrepHistoryListItem;
begin
  if reContext.Focused and (reContext.SelLength > 0) then
  begin
    reContext.CopyToClipboard;
    Exit;
  end;

  if (lbHistoryList.ItemIndex = -1) or (lbHistoryList.Count = 0) then
    Exit;

  AItem := gblGrepExpert.HistoryList.Items[lbHistoryList.ItemIndex];
  if not AItem.IsOnlySaveSettings then
  begin
    gblGrepExpert.HistoryList.ClearAllChecked;
    AItem.Checked := True;
    SaveGrepResultsToFile(Self, gblGrepExpert.HistoryList, gblGrepExpert.HistoryIniVersion, False,
      sfPrintToFile, grCopy);
  end;
end;

procedure TfmGrepResults.actFileOpenExecute(Sender: TObject);
resourcestring
  rsOpenCaption = 'Select the items to open';   //Open saved Search History
var
  ASelResult: TGrepSelectResult;
  AIni: TGrepIniFile;
  frm: TfmGrepSelect;
  AOpenHistoryList: TGrepHistoryList;
  AItemIndex: Integer;
  AClearList, AOverwriteItem, AOnlyIfNewer: Boolean;
begin
  if not OpenDialog.Execute then
    Exit;

  AOpenHistoryList := TGrepHistoryList.Create;
  try
    AOpenHistoryList.ListMode := hlmAll;

    AIni := TGrepIniFile.Create(OpenDialog.FileName);
    try
      if AIni.SectionExists(TGrepHistoryListItem.SubKeyNameHistory) then begin
         //without numbers
        AOpenHistoryList.LoadItemFromIni(FGrepSettings, AIni, gblGrepExpert.HistoryIniVersion,
          gblGrepExpert.OpenSaveOption, gblGrepExpert.HistoryList);
      end else begin
        AOpenHistoryList.LoadFromSettings(FGrepSettings, AIni, gblGrepExpert.HistoryIniVersion,
          ifmSingle, '', gblGrepExpert.OpenSaveOption, gblGrepExpert.HistoryList);
      end;
    finally
      AIni.Free;
    end;

    if AOpenHistoryList.Count = 0 then
    begin
      MessageDlg('Could not read history list from file!', mtWarning, [mbOK], 0);
      Exit;
    end;

    frm := TfmGrepSelect.Create(Self);
    try
      frm.Init(AOpenHistoryList, gstOpen, rsOpenCaption);
      ASelResult := frm.Execute;
      AClearList := frm.cbOpenClear.Checked;
      AOverwriteItem := frm.cbOpenOverwrite.Checked;
      AOnlyIfNewer := frm.cbOpenOnlyIfNewer.Checked;
    finally
      frm.Free;
    end;

    if ASelResult = gsrNoSelection then
      Exit;

    SetHistoryListMode(hlmResults, False, True, False, True);

    //"copy to"
    AItemIndex := gblGrepExpert.HistoryList.MoveItems(AOpenHistoryList, AClearList, AOverwriteItem, AOnlyIfNewer);

    RefreshHistoryView(False);

    lbHistoryList.ItemIndex := AItemIndex;

    lbHistoryList.Refresh;

    ViewHistoryListItems(AItemIndex, False);
  finally
    AOpenHistoryList.Free;
  end;
end;

procedure TfmGrepResults.actViewStayOnTopExecute(Sender: TObject);
begin
  StayOnTop := not StayOnTop;
end;

procedure TfmGrepResults.actFileExitExecute(Sender: TObject);
begin
  if IsStandAlone then
    ModalResult := mrCancel
  else
    Hide;
end;

procedure TfmGrepResults.actHelpHelpExecute(Sender: TObject);
begin
  GxContextHelp(Self, 3);
end;

procedure TfmGrepResults.actHelpAboutExecute(Sender: TObject);
begin
  ShowGXAboutForm;
end;

procedure TfmGrepResults.actListGotoSelectedExecute(Sender: TObject);
begin
  GotoHighlightedListEntry;
end;

procedure TfmGrepResults.actListSelectNextExecute(Sender: TObject);
begin
  if SelectNextListItem then
    GotoHighlightedListEntry;
end;

procedure TfmGrepResults.actListSelectPreviousExecute(Sender: TObject);
begin
  if SelectPrevListItem then
    GotoHighlightedListEntry;
end;

procedure TfmGrepResults.actHamburgerMenuExecute(Sender: TObject);
var
  Pnt: TPoint;
begin
  Pnt := tbnHamburgerMenu.ClientToScreen(Point(0, tbnHamburgerMenu.Height));
  pmHamburgerMenu.Popup(Pnt.X, Pnt.Y);
end;

procedure TfmGrepResults.actListContractExecute(Sender: TObject);
begin
  ContractList(True);
end;

procedure TfmGrepResults.actListExpandExecute(Sender: TObject);
begin
  ExpandList(False, True, 0);
end;

procedure TfmGrepResults.ExpandOrContractList(Expand, UsedState, SetState: Boolean; AExpandFewLines: Integer);

  function ExpandFileResult(ListBoxIndex: Integer): Integer;
  var
    FileResult: TFileResult;
    t: Integer;
  begin
    FileResult := lbResults.Items.Objects[ListBoxIndex] as TFileResult;

    for t := FileResult.Count - 1 downto 0 do
      lbResults.Items.InsertObject(ListBoxIndex + 1, FileResult.Items[t].Line, FileResult.Items[t]);

    FileResult.Expanded := True;
    if SetState then
      FileResult.ExpandState := True;
    Result := ListBoxIndex + FileResult.Count - 1;
  end;

var
  i: Integer;
  LFileResult: TFileResult;
begin
  if (lbResults.Items.Count > 0) and not Assigned(lbResults.Items.Objects[0]) then
    Exit;

  lbResults.Items.BeginUpdate;
  try
    RefreshContextLines;

    i := 0;
    while i <= lbResults.Items.Count - 1 do
    begin
      if Expand then
      begin
        if lbResults.Items.Objects[i] is TFileResult then
        begin
          LFileResult := TFileResult(lbResults.Items.Objects[i]);

          if not LFileResult.Expanded and (not UsedState or LFileResult.ExpandState) and
            ( (AExpandFewLines = 0) or (LFileResult.Count <= AExpandFewLines) )
          then
            i := ExpandFileResult(i);
        end;

        Inc(i);
      end
      else // Contract
      begin
       if lbResults.Items.Objects[i] is TLineResult then
          lbResults.Items.Delete(i)
        else
        begin
          LFileResult := TFileResult(lbResults.Items.Objects[i]);
          LFileResult.Expanded := False;
          if SetState then
            LFileResult.ExpandState := False;

          Inc(i);
        end;
      end;
    end;
  finally
    lbResults.Items.EndUpdate;
  end;
end;

procedure TfmGrepResults.ExpandList(AUsedState, ASetState: Boolean; AExpandFewLines: Integer);
begin
  ExpandOrContractList(True, AUsedState, ASetState, AExpandFewLines);
end;

procedure TfmGrepResults.ContractList(ASetState: Boolean);
begin
  ExpandOrContractList(False, False, ASetState, 0);
end;

function TfmGrepResults.GetStayOnTop: Boolean;
begin
  Result := (FormStyle = fsStayOnTop);
end;

procedure TfmGrepResults.GotoHighlightedListEntry;
var
  CurrentLine: TLineResult;
  ResultIndex: Integer;
begin
  ResultIndex := lbResults.ItemIndex;
  if ResultIndex < 0 then
    Exit;

  if lbResults.Items.Objects[ResultIndex] is TFileResult then
  begin
    ToggleFileResultExpanded(ResultIndex);
    Exit;
  end;

  CurrentLine := lbResults.Items.Objects[ResultIndex] as TLineResult;
  if CurrentLine = nil then
    Exit;

  GoToMatchLine(CurrentLine);

  // Hide the results window if the window is not configured to stay on top in D8+ and we are floating
  if gblGrepExpert.AutoHide and RunningDelphi8OrGreater then begin
    if (not StayOnTop) and (not Assigned(Self.Parent)) then
    begin
      if IsStandAlone then
        ModalResult := mrCancel
      else
        Hide;
    end;
  end;
end;

constructor TfmGrepResults.Create(AOwner: TComponent);
var
  IM: TGrepHistoryListMode;
  IT: TPageIndexType;
begin
  inherited;

  TControl_SetMinConstraints(Self);

  FShowHistoryList := True;

  for IM := Low(TGrepHistoryListMode) to High(TGrepHistoryListMode) do
    for IT := Low(TPageIndexType) to High(TPageIndexType) do
      FPageIndexes[IM, IT] := -1;

  SetToolbarGradient(ToolBar);

  if IsStandAlone then begin
    actHamburgerMenu.Visible := False;
    Menu := MainMenu;
  end;

  FSearchInProgress := False;
  lbResults.DoubleBuffered := True;
  CenterForm(Self);

  InitDpiScaler;

  LoadSettings;
  ResizeListBox;
  SetMatchString('');

  FContextSearchText := '';

  FDragSource := TDropFileSource.Create(nil);

  if IsStandAlone then
  begin
    FormStyle := fsNormal;
    BorderStyle := bsSizeable;
  end;

  FEmbeddedGrepSearch := nil;

  FSavedFormCaption := Caption;
  FSavedLastSearchTimeCaption := miHistoryLastSearchTime.Caption;
  FSavedSaveOptionCaption := miSettingsSaveOption.Caption;
  FSavedDirectoriesDataCaption := miSettingsDirectoriesData.Caption;
  FSavedExcludeDirsCaption := miSettingsExcludeDirs.Caption;
  FSavedFileMasksCaption := miSettingsFileMasks.Caption;
  FSaveItemEmptyCaption := miHistoryItemName.Caption;
  FSaveSortCaption := miHistorySort.Caption;

  actViewStayOnTop.Visible := IsStandAlone;
  tbnSep6.Visible := actViewStayOnTop.Visible;

{$IFNDEF ICONS_IN_POPUP_MENUS_ARE_BROKEN}
  if not IsStandAlone then
    pmHamburgerMenu.Images := GetSharedImageList;
{$ENDIF}
end;

destructor TfmGrepResults.Destroy;
begin
  Self.Abort;

  if lbResults.HandleAllocated then begin
    // XE used to crash here with a "Component already destroyed" error due to the listbox handle
    // being 0 and then recreated in a destructor
    ClearResultsListbox;
  end;

  SaveSettings;

  FreeAndNil(FDragSource);

  inherited Destroy;

  fmGrepResults := nil;
end;

procedure TfmGrepResults.GetEnabledFlags(out _IsOnlySaveSettings, _HaveItems, _Processing: Boolean);
var
  Idx: Integer;
  AItem: TGrepHistoryListItem;
begin
  Idx := lbHistoryList.ItemIndex;
  if Idx <> -1 then begin
    AItem := gblGrepExpert.HistoryList.Items[Idx];
    _IsOnlySaveSettings := AItem.IsOnlySaveSettings;
  end else
    _IsOnlySaveSettings := False;

  _HaveItems := not _IsOnlySaveSettings and (lbResults.Items.Count > 0);
  _Processing := DoingSearchOrReplace;
end;

function TfmGrepResults.GetLoadedContextHeight: integer;
begin
  Result := MulDiv(pnlMain.ClientHeight, FLoadedContextHeightRelative, 10000)
end;

procedure TfmGrepResults.SetLoadedContextHeight(_Value: Integer);
begin
  FLoadedContextHeightRelative := MulDiv(_Value, 10000, pnlMain.ClientHeight);
end;

procedure TfmGrepResults.TheActionListUpdate(Action: TBasicAction; var Handled: Boolean);
var
  HaveItems, Processing, AIsOnlySaveSettings: Boolean;
begin
  GetEnabledFlags(AIsOnlySaveSettings, HaveItems, Processing);

  actFileSearch.Enabled := not Processing;
  actViewOptions.Enabled := not Processing;
  actViewStayOnTop.Enabled := not Processing;
  actFilePrint.Enabled := not Processing and HaveItems;
  actListCopy.Enabled := not Processing and HaveItems;
  actListGotoSelected.Enabled := not Processing and HaveItems;
  actListGotoSelectedAndClose.Enabled := not Processing and HaveItems;
  actListContract.Enabled := not Processing and HaveItems;
  actListExpand.Enabled := not Processing and HaveItems;
  actFileAbort.Enabled := Processing;
  actReplaceAll.Enabled := not Processing and HaveItems;
  actReplaceSelected.Enabled := not Processing and HaveItems;
  actViewShowFullFilename.Enabled := not Processing and not AIsOnlySaveSettings;
  actViewShowIndent.Enabled := not Processing and not AIsOnlySaveSettings;

  actViewStayOnTop.Checked := StayOnTop;
  actViewShowContext.Checked := ShowContext;
  actViewToolBar.Checked := ToolBar.Visible;
  actViewShowHistoryList.Checked := ShowHistoryList;
  actViewShowFullFilename.Checked := ShowFullFilename;
  actViewShowIndent.Checked := ShowLineIndent;
  Handled := True;
end;

function TfmGrepResults.ShowModalForm(Dlg: TCustomForm): TModalResult;
var
  SavedOnTop: Boolean;
begin
  Result := mrCancel;
  SavedOnTop := StayOnTop;
  try
    // The search dialog can get hidden behind the results if we don't do this
    StayOnTop := False;

    if Dlg.ShowModal <> mrOk then
      Exit;

    Result := mrOk;
  finally
    StayOnTop := SavedOnTop;
  end;
end;

procedure TfmGrepResults.actHelpContentsExecute(Sender: TObject);
begin
  GxContextHelpContents(Self);
end;

procedure TfmGrepResults.Abort;
begin
  if FSearcher <> nil then
    FSearcher.AbortSignalled := True;
end;

procedure TfmGrepResults.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);

  if IsStandAlone then
  begin
    Params.ExStyle := Params.ExStyle or WS_EX_APPWINDOW;
    Params.WndParent := GetDesktopwindow;
  end;
end;

procedure TfmGrepResults.actViewToolBarExecute(Sender: TObject);
begin
  ToolBar.Visible := not ToolBar.Visible;
end;

procedure TfmGrepResults.actViewOptionsExecute(Sender: TObject);
begin
  gblGrepExpert.Configure;
  AssignSettingsToForm;
  ResizeListBox;
  RefreshContextLines;
  UpdateHistoryPagesOptions;
  lbHistoryList.Refresh;
end;

procedure TfmGrepResults.FormShow(Sender: TObject);
begin
  AssignSettingsToForm;
  ResizeListBox;
  ResizeStatusBar;
  ForceRedraw;
end;

procedure TfmGrepResults.ForceRedraw;
begin
  pnlMain.Visible := False;
  pnlMain.Visible := True; //FI:W508 Assigned twice successively
//  Repaint;
end;

{$IFDEF IDE_IS_HIDPI_AWARE}
procedure TfmGrepResults.ApplyDpi(_NewDpi: Integer; _NewBounds: PRect);
var
  il: TImageList;
begin
  inherited;
  ToolBar.DisabledImages := GExpertsInst.GetScaledSharedDisabledImages(_NewDpi);
  il := GExpertsInst.GetScaledSharedImages(_NewDpi);
  ToolBar.Images := il;
  TheActionList.Images := il;
  MainMenu.Images := il;
end;

procedure TfmGrepResults.ArrangeControls;
begin
  inherited;
  ResizeListBox;
  ResizeStatusBar;
  UpdateContextPanelHeight;
end;
{$ENDIF}

procedure TfmGrepResults.AssignSettingsToForm;
begin
  Assert(Assigned(gblGrepExpert));
  reContext.Font.Assign(gblGrepExpert.ContextFont);
  lbResults.Font.Assign(gblGrepExpert.ListFont);
end;

procedure TfmGrepResults.actReplaceAllExecute(Sender: TObject);
var
  TimeStart: TDateTime;
  MatchesFound: Integer;
begin
  Assert(not DoingSearchOrReplace);

  if not QueryUserForReplaceOptions('All matched files') then
    Exit;

  FReplaceInProgress := True;
  try
    TCursor_TempHourglass;
    TimeStart := Now;
    SetStatusString('');
    MatchesFound := ReplaceAll(lbResults.Items, FGrepSettings);
    SetStatusString(Format(SGrepReplaceStats, [MatchesFound, (Now - TimeStart) * 24*60*60]));
  finally
    FReplaceInProgress := False;
  end;
  RefreshContextLines;
end;

procedure TfmGrepResults.actReplaceSelectedExecute(Sender: TObject);
resourcestring
  SReplaceLine = sLineBreak + 'On line: ';
var
  TimeStart: TDateTime;
  MatchesFound: Integer;
  CurrentLine: TLineResult;
  ResultIndex: Integer;
  FileResult: TFileResult;
  MatchFile: string;
  ResultObject: TObject;
begin
  Assert(not DoingSearchOrReplace);

  ResultIndex := lbResults.ItemIndex;
  if ResultIndex < 0 then
    Exit;

  ResultObject := lbResults.Items.Objects[ResultIndex];
  FReplaceInProgress := True;
  try
    SetStatusString('');
    if ResultObject is TFileResult then
    begin
      FileResult := TFileResult(ResultObject);
      if not QueryUserForReplaceOptions(FileResult.FileName) then
        Exit;
      TCursor_TempHourglass;
      TimeStart := Now;
      MatchesFound := ReplaceAllInFiles(FileResult, FGrepSettings);
    end
    else if ResultObject is TLineResult then
    begin
      CurrentLine := ResultObject as TLineResult;
      MatchFile := TFileResult(CurrentLine.Collection).FileName;
      if not QueryUserForReplaceOptions(MatchFile + SReplaceLine + IntToStr(CurrentLine.LineNo)) then
        Exit;
      TCursor_TempHourglass;
      TimeStart := Now;
      MatchesFound := ReplaceLine(CurrentLine, FGrepSettings);
    end
    else
      raise Exception.Create('Internal Error: Unknown result type');
    SetStatusString(Format(SGrepReplaceStats, [MatchesFound, (Now - TimeStart) * 24*60*60]));
  finally
    FReplaceInProgress := False;
  end;
  RefreshContextLines;
end;

function TfmGrepResults.DoingSearchOrReplace: Boolean;
begin
  Result := FSearchInProgress or FReplaceInProgress;
end;

procedure TfmGrepResults.SetStatusString(const StatusStr: string);
begin
  StatusBar.Panels[0].Text := StatusStr;
end;

procedure TfmGrepResults.SetMatchString(const MatchStr: string);
begin
  StatusBar.Panels[1].Text := MatchStr;
  if IsEmpty(MatchStr) then
    StatusBar.Panels[1].Width := 0
  else
    StatusBar.Panels[1].Width := StatusBar.Canvas.TextWidth(MatchStr) + 50;
  ResizeStatusBar;
end;

function TfmGrepResults.QueryUserForGrepOptions(AState: TGrepSearchState): Boolean;
resourcestring
  rsSearchAgainCaption = 'Grep Search Again';
  rsModifySearchSettingsCaption = 'Grep Modify Search Parameters';
var
  frm: TfmGrepSearch;
begin
  Result := False;

  frm := TfmGrepSearch.Create(nil);
  try
    case AState of
      gssSearchAgain: frm.Caption := rsSearchAgainCaption;
      gssModifySearchSettings: frm.Caption := rsModifySearchSettingsCaption;
    end;
    if AState in [gssSearchAgain, gssModifySearchSettings] then
      frm.AdjustSettings(FGrepSettings);
    if ShowModalForm(frm) <> mrOk then
      Exit;
    FGrepSettings.CanRefresh := True;
    SetMatchString('');
    frm.RetrieveSettings(FGrepSettings);
    if AState <> gssModifySearchSettings then
      gblGrepExpert.SaveSettings;
    Result := True;
  finally
    FreeAndNil(frm);
  end;
end;

function TfmGrepResults.QueryUserForReplaceOptions(const ReplaceInString: string): Boolean;
var
  frm: TfmGrepReplace;
begin
  ShowGxMessageBox(TShowUnicodeReplaceMessage);

  Result := False;

  frm := TfmGrepReplace.Create(nil);
  try
    frm.ReplaceInString := ReplaceInString;
    frm.SearchString := FGrepSettings.Pattern;
    if ShowModalForm(frm) <> mrOk then
      Exit;
    SetMatchString('');
    frm.RetrieveSettings(FGrepSettings);
    Result := True;
  finally
    FreeAndNil(frm);
  end;
end;

function TfmGrepResults.ConfigurationKey: string;
begin
  Result := TGrepExpert.ConfigurationKey;
end;

procedure TfmGrepResults.ResizeStatusBar;
begin
  StatusBar.Panels[0].Width := StatusBar.ClientWidth - StatusBar.Panels[1].Width;
  if StatusBar.Panels[0].Text = '' then begin
    // for debugging, set some unobstrusive text
    StatusBar.Panels[0].Text := '.';
  end;
end;

function TfmGrepResults.ResultListItemIsFileResult(ListBoxIndex: Integer): boolean;
begin
  result := (ListBoxIndex >= 0) and (ListBoxIndex < lbResults.Items.Count) and (lbResults.Items.Objects[ListBoxIndex] is TFileResult);
end;

procedure TfmGrepResults.SetShowHistoryList(const Value: Boolean);
begin
  if FShowHistoryList <> Value then
  begin
    if FShowHistoryList then
      FLoadedHistoryListWidthRelative := MulDiv(tcHistoryListPage.Width, 10000, pnlMain.ClientWidth);
    FShowHistoryList := Value;
    if Assigned(gblGrepExpert) then
      gblGrepExpert.HistoryList.Enabled := Value;
    tcHistoryListPage.Visible := Value;
    if Value then
      SplitterHistoryList.Left := tcHistoryListPage.Left + tcHistoryListPage.Width;
    SplitterHistoryList.Visible := Value;
    if lbHistoryList.Items.Count > 0 then
    begin
      lbHistoryList.ItemIndex := 0;
      ViewHistoryListItems(lbHistoryList.ItemIndex, True);
    end;
  end;
end;

procedure TfmGrepResults.SetShowFullFilename(const Value: Boolean);
begin
  if FShowFullFilename <> Value then
  begin
    FShowFullFilename := Value;
    lbResults.Refresh;
  end;
end;

procedure TfmGrepResults.SetShowLineIndent(const Value: Boolean);
begin
  if FShowLineIndent <> Value then
  begin
    FShowLineIndent := Value;
    lbResults.Refresh;
  end;
end;

procedure TfmGrepResults.actListGotoSelectedAndCloseExecute(Sender: TObject);
begin
  GotoHighlightedListEntry;
  Close;
end;

procedure TfmGrepResults.actViewShowHistoryListExecute(Sender: TObject);
begin
  ShowHistoryList := not ShowHistoryList
end;

procedure TfmGrepResults.actViewShowFullFilenameExecute(Sender: TObject);
begin
  ShowFullFilename := not ShowFullFilename;
end;

procedure TfmGrepResults.actViewShowIndentExecute(Sender: TObject);
begin
  ShowLineIndent := not ShowLineIndent;
end;

procedure TfmGrepResults.ViewHistoryListItems(AIndex: Integer; AUsedExpandState: Boolean);
var
  AHistoryItem: TGrepHistoryListItem;
begin
  if FSearchInProgress then
    Exit; // prevent race condition where painting and search both write to `lbResults.Items`
  ClearResultsData;
  SetSavedHistoryIndexes(AIndex);
  if AIndex <> -1 then
  begin
    AHistoryItem := gblGrepExpert.HistoryList.Items[AIndex];
    if Assigned(AHistoryItem) then
    begin
      AHistoryItem.View(lbResults.Items);
      RefreshInformation(AHistoryItem.TotalMatchCount, True, AUsedExpandState, False);
      FGrepSettings := AHistoryItem.GrepSettings;
    end;
  end;
end;

procedure TfmGrepResults.lbHistoryListData(Control: TWinControl; Index: Integer; var Data: string);
begin
  Data := gblGrepExpert.HistoryList[Index];
end;

procedure TfmGrepResults.lbHistoryListDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  c2ndRowTop: Integer;
  cMatchesLeft: Integer;
  AItem: TGrepHistoryListItem;
  cnv: TCanvas;
begin
  AItem := gblGrepExpert.HistoryList.Items[Index];
  if not Assigned(AItem) or not (AItem is TGrepHistoryListItem) then
    Exit;

  cnv := lbHistoryList.Canvas;
  if not gblGrepExpert.ListUseDefaultColors then
  begin
    if odSelected in State then
    begin
      cnv.Font.Color := gblGrepExpert.ListMatchTextColor;
      cnv.Brush.Color := gblGrepExpert.ListMatchBrushColor;
    end
    else
    begin
      cnv.Font.Color := gblGrepExpert.ListFont.Color;
      cnv.Brush.Color := clWindow;
    end;
  end;

  cnv.FillRect(Rect);

  cnv.TextOut(Rect.Left + 1, Rect.Top + 1, lbHistoryList.Items[Index]);

  c2ndRowTop := cnv.TextHeight(SAllAlphaNumericChars);
  cMatchesLeft := FScaler.Calc(10);

  if AItem.GrepSettings.SaveOption = gsoNoSave then
    cnv.TextOut(Rect.Left + 1, Rect.Top + c2ndRowTop, '!!')
  else if AItem.GrepSettings.SaveOption = gsoOnlySaveSettings then
    cnv.TextOut(Rect.Left + 1, Rect.Top + c2ndRowTop, '*');

  if AItem.IsOnlySaveSettings then
    cnv.TextOut(Rect.Left + cMatchesLeft, Rect.Top + c2ndRowTop,
      Format('(%d in %d)', [AItem.TotalMatchCount, AItem.FileCount]))
  else
    cnv.TextOut(Rect.Left + cMatchesLeft, Rect.Top + c2ndRowTop,
      Format('%d in %d', [AItem.TotalMatchCount, AItem.ResultList.Count]));
end;

procedure TfmGrepResults.lbHistoryListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  AIndex, APage: Integer;
  AItem: TGrepHistoryListItem;
  ADoSearch: Boolean;
begin
  AIndex := lbHistoryList.ItemIndex;
  APage := lbHistoryList.Height div lbHistoryList.ItemHeight;
  ADoSearch := False;
  if Key = VK_UP then
    Dec(AIndex)
  else if Key = VK_DOWN then
    Inc(AIndex)
  else if Key = VK_HOME then
    AIndex := 0
  else if Key = VK_END then
    AIndex := lbHistoryList.Count-1
  else if Key = VK_PRIOR then
    Dec(AIndex, APage)
  else if Key = VK_NEXT then
    Inc(AIndex, APage)
  else if Key = VK_RETURN then
    ADoSearch := True
  else if Key <> VK_SPACE then
    Exit;

  Key := 0;

  if AIndex < 0 then
    AIndex := 0
  else if AIndex >= lbHistoryList.Count then
    AIndex := lbHistoryList.Count-1;

  lbHistoryList.ItemIndex := AIndex;
  lbHistoryListIndexForHistoryMenuActions := AIndex; // in case actHistroy... actions get fired without pmHistoryMenu being visible
  ViewHistoryListItems(AIndex, True);

  AItem := gblGrepExpert.HistoryList.Items[AIndex];
  if AItem.IsOnlySaveSettings and
    ( TGrepOnlySaveSettingsAction(gblGrepExpert.GrepOnlySaveParamsAction) = gossaShowEmbeddedSearch )
  then
  begin
    if ADoSearch then
      DoEmbeddedSearch(nil)
    else
    begin
      FEmbeddedGrepSearch.AdjustSettings(AItem.GrepSettings);
      FEmbeddedGrepSearch.EmbeddedShow;
    end;
  end;
end;

procedure TfmGrepResults.lbHistoryListMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  FHistoryMousePos.X := X;
  FHistoryMousePos.Y := Y;

  if (gblGrepExpert.HistoryList.ListMode = hlmSearch) and (lbHistoryList.Count = 0) then
    actHistorySearchInHistory.Execute;
end;

procedure TfmGrepResults.lbHistoryListMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  ClickedEntry: Integer;
  AItem: TGrepHistoryListItem;
begin
  if Button = mbLeft then
  begin
    ClickedEntry := lbHistoryList.ItemAtPos(Point(X, Y), True);
    if ClickedEntry <> -1 then
    begin
      AItem := gblGrepExpert.HistoryList.Items[ClickedEntry];
      if AItem.IsOnlySaveSettings then
        case TGrepOnlySaveSettingsAction(gblGrepExpert.GrepOnlySaveParamsAction) of
          gossaShowSearchWindow:
          begin
            ViewHistoryListItems(ClickedEntry, False);
            lbHistoryListIndexForHistoryMenuActions := ClickedEntry;
            actHistorySearch.Execute;
          end ;
          gossaShowEmbeddedSearch:
          begin
            ViewHistoryListItems(ClickedEntry, False);
            FEmbeddedGrepSearch.AdjustSettings(AItem.GrepSettings);
            FEmbeddedGrepSearch.EmbeddedShow;
          end;
          gossaAutoRefresh:
          begin
            lbHistoryListIndexForHistoryMenuActions := ClickedEntry;
            actHistoryRefresh.Execute;
          end;
          //Auto refresh when double click
          gossaAutoRefreshDouble: Exit;
        else //Empty list
          ViewHistoryListItems(ClickedEntry, True);
        end
      else
        ViewHistoryListItems(ClickedEntry, True);
    end;
  end;
end;

procedure TfmGrepResults.lbHistoryListDblClick(Sender: TObject);
var
  ClickedEntry: Integer;
  AItem: TGrepHistoryListItem;
begin
  ClickedEntry := lbHistoryList.ItemAtPos(FHistoryMousePos, True);
  if ClickedEntry <> -1 then
  begin
    AItem := gblGrepExpert.HistoryList.Items[ClickedEntry];
    if AItem.IsOnlySaveSettings then
      case TGrepOnlySaveSettingsAction(gblGrepExpert.GrepOnlySaveParamsAction) of
        //Auto refresh when double click
        gossaAutoRefreshDouble:
        begin
          lbHistoryListIndexForHistoryMenuActions := ClickedEntry;
          actHistoryRefresh.Execute;
        end;
      end;
  end;
end;

procedure TfmGrepResults.lbHistoryListContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
resourcestring
  rsItemCaptionFormat = 'Search text = %s';
  rsItemCaptionHint = 'Click to copy search text';
  rsSortUnsorted = 'Unsorted';
  rsSortKeyIndex = 'by keyindex';
  rsSortSearchText = 'by search text';
  rsSortSearchTime = 'by last search time';
  rsSortDesc = 'descending';
const
  cSortTexts: array[TGrepHistorySort] of string = (rsSortUnsorted, rsSortKeyIndex, rsSortSearchText, rsSortSearchTime, '');
var
  AIndex: Integer;
  AItem: TGrepHistoryListItem;
  IsDirectorySettings: Boolean;
begin
  AIndex := lbHistoryList.ItemAtPos(MousePos, True);
  if AIndex = -1 then
    AIndex := lbHistoryList.ItemIndex;

  lbHistoryListIndexForHistoryMenuActions := AIndex;

  if AIndex = -1 then
  begin
    miHistoryItemName.Caption := FSaveItemEmptyCaption;
    miHistoryItemName.Enabled := False;
    miHistoryItemName.Hint := '';
    miHistorySettings.Visible := False;
    miHistoryLastSearchTime.Visible := False;
    Exit;
  end;

  AItem := gblGrepExpert.HistoryList.Items[AIndex];
  miHistoryItemName.Caption := Format(rsItemCaptionFormat, [lbHistoryList.Items[AIndex]]);
  miHistoryItemName.Hint := rsItemCaptionHint;

  miHistoryLastSearchTime.Caption := Format('%s = %s',
    [FSavedLastSearchTimeCaption, DateTimeToStr(AItem.LastSearchTime)]);
  actHistorySort.Caption := FSaveSortCaption + Format(' (%s%s)',
    [cSortTexts[gblGrepExpert.HistoryList.SortMode], IfThen(gblGrepExpert.HistoryList.SortDesc, ' ' + rsSortDesc)]);

  miHistorySettings.Visible := True;

  miSettingsSaveOption.Caption := Format('%s = %s',
    [FSavedSaveOptionCaption, SaveOptionText(AItem.GrepSettingsSaveOption)]);

  miSettingsCurrentFile.Checked := AItem.GrepSettings.GrepAction = gaCurrentOnlyGrep;
  miSettingsAllFilesInProjectGroup.Checked := AItem.GrepSettings.GrepAction = gaProjGroupGrep;
  miSettingsAllFilesInProject.Checked := AItem.GrepSettings.GrepAction = gaProjGrep;
  miSettingsOpenProjectFiles.Checked := AItem.GrepSettings.GrepAction = gaOpenFilesGrep;
  miSettingsPreviousSearchResultFiles.Checked := AItem.GrepSettings.GrepAction = gaResults;

  IsDirectorySettings := AItem.GrepSettings.GrepAction = gaDirGrep;
  miSettingsDirectories.Checked := IsDirectorySettings;
  miSettingsSepDir.Visible := IsDirectorySettings;
  miSettingsDirectoriesData.Visible := IsDirectorySettings;
  miSettingsDirectoriesData.Checked := AItem.GrepSettings.Directories <> '';
  miSettingsDirectoriesData.Caption := Format('%s = %s', [FSavedDirectoriesDataCaption, AItem.GrepSettings.Directories]);
  miSettingsExcludeDirs.Visible := IsDirectorySettings;
  miSettingsExcludeDirs.Checked := AItem.GrepSettings.ExcludedDirs <> '';
  miSettingsExcludeDirs.Caption := Format('%s = %s', [FSavedExcludeDirsCaption, AItem.GrepSettings.ExcludedDirs]);
  miSettingsExcludeDirsIsRegEx.Visible := IsDirectorySettings;
  miSettingsExcludeDirsIsRegEx.Checked := AItem.GrepSettings.ExcludedDirsIsRegEx;
  miSettingsFileMasks.Visible := IsDirectorySettings;
  miSettingsFileMasks.Checked := AItem.GrepSettings.Mask <> '';
  miSettingsFileMasks.Caption := Format('%s = %s', [FSavedFileMasksCaption, AItem.GrepSettings.Mask]);
  miSettingsSearchSubDirectories.Visible := IsDirectorySettings;
  miSettingsSearchSubDirectories.Checked := AItem.GrepSettings.IncludeSubdirs;

  miSettingsCaseSensitive.Checked := AItem.GrepSettings.CaseSensitive;
  miSettingsWholeWord.Checked := AItem.GrepSettings.WholeWord;
  miSettingsSearchFormFiles.Checked := AItem.GrepSettings.IncludeForms;
  miSettingsFormHandleMultiline.Checked := AItem.GrepSettings.HandleFormMultiline;
  miSettingsFormHandleSpecialChars.Checked := AItem.GrepSettings.HandleFormSpecialChars;
  miSettingsSearchSQLFiles.Checked := AItem.GrepSettings.IncludeSQLs;
  miSettingsRegularExpression.Checked := AItem.GrepSettings.RegEx;

  miSettingsGrepCode.Checked := AItem.GrepSettings.IncludeCode;
  miSettingsGrepStrings.Checked := AItem.GrepSettings.IncludeStrings;
  miSettingsGrepComments.Checked := AItem.GrepSettings.IncludeComments;

  miSettingsSectionInterface.Checked := AItem.GrepSettings.SectionInterface;
  miSettingsSectionImplementation.Checked := AItem.GrepSettings.SectionImplementation;
  miSettingsSectionInitialization.Checked := AItem.GrepSettings.SectionInitialization;
  miSettingsSectionFinalization.Checked := AItem.GrepSettings.SectionFinalization;
end;

procedure TfmGrepResults.actHistoryUpdate(Sender: TObject);
var
  AAction: TAction;
  IsOnlySaveSettings: Boolean;
  HaveItems: Boolean;
  Processing: Boolean;
begin
  GetEnabledFlags(IsOnlySaveSettings, HaveItems, Processing);
  AAction := Sender as TAction;
  AAction.Enabled := not Processing and (lbHistoryList.Count > 0) and
    ((AAction.tag = 1) or (gblGrepExpert.HistoryList.ListMode <> hlmSettings));
end;

procedure TfmGrepResults.miHistoryItemNameClick(Sender: TObject);
var
  AItem: TGrepHistoryListItem;
begin
  AItem := gblGrepExpert.HistoryList.Items[lbHistoryListIndexForHistoryMenuActions];
  if Assigned(AItem) then
    Clipboard.AsText := AItem.SearchText;
end;

procedure TfmGrepResults.actHistoryViewExecute(Sender: TObject);
begin
  lbHistoryList.ItemIndex := lbHistoryListIndexForHistoryMenuActions;
  ViewHistoryListItems(lbHistoryList.ItemIndex, True);
end;

procedure TfmGrepResults.DoEmbeddedSearch(Sender: TObject);
var
  AItem: TGrepHistoryListItem;
begin
  AItem := gblGrepExpert.HistoryList.Items[lbHistoryList.ItemIndex];
  if not Assigned(AItem) then
    Exit;

  FGrepSettings.CanRefresh := True;
  SetMatchString('');
  FEmbeddedGrepSearch.RetrieveSettings(FGrepSettings);
  gblGrepExpert.SaveSettings;
  FEmbeddedGrepSearch.Hide;
  Execute(gssSearchEmbedded);
end;

procedure TfmGrepResults.actHistoryRefreshExecute(Sender: TObject);
var
  AItem: TGrepHistoryListItem;
  ASaveSettings: TGrepSettings;
begin
  AItem := gblGrepExpert.HistoryList.Items[lbHistoryListIndexForHistoryMenuActions];
  if not Assigned(AItem) then
    Exit;

  ASaveSettings := FGrepSettings;
  FGrepSettings := AItem.GrepSettings;
  if Sender = actHistoryRefresh then
    actFileRefresh.Execute
  else if Sender = actHistorySearch then
  begin
    if Execute(gssSearchAgain) then
      RefreshContextLines;
  end
  else if Sender = actHistoryModifySearchSettings then
  begin
    try
      Execute(gssModifySearchSettings);
    finally
      FGrepSettings := ASaveSettings;
    end;
  end;
end;

procedure TfmGrepResults.actFileSaveExecute(Sender: TObject);
resourcestring
  rsSaveAllCaption = 'Select the Items to Save';
  rsSaveProgress = 'Grep Save Progress';
  rsPrintAllCaption = 'Select the Items to Print to File';
  rsPrintProgress = 'Grep Print to File Progress';
  rsSavePrintAllCaption = 'Select the Items to Save & Print';
  rsSavePrintProgress = 'Grep Save & Print Progress';
const
  cSelectTypeByMode: array[TSaveToFileMode] of TGrepSelectType = (gstPrint, gstSave, gstSavePrint);
  cModeCaption: array[TSaveToFileMode] of string = (rsPrintAllCaption, rsSaveAllCaption, rsSavePrintAllCaption);
  cModeProgress: array[TSaveToFileMode] of string = (rsPrintProgress, rsSaveProgress, rsSavePrintProgress);
var
  AMode: TSaveToFileMode;
  ASelResult: TGrepSelectResult;
  AFileCount, ASplitCount: Integer;
begin
  if lbHistoryList.Count = 0 then
    Exit;

 if Sender = actFilePrintToFile then
    AMode := sfPrintToFile
  else if Sender = actFileSavePrint then
    AMode := sfBoth
  else
    AMode := sfSaveToLoadable;

  ASelResult := HistoryListSelect(cSelectTypeByMode[AMode], cModeCaption[AMode]);

  if ASelResult = gsrNoSelection then
    Exit;

  if FSaveSplitCount > 0 then
  begin
    AFileCount := (FSelectedCount div FSaveSplitCount) + 1;
    ASplitCount := FSaveSplitCount;
  end
  else
  begin
    AFileCount := 1;
    ASplitCount := FSelectedCount;
  end;
  if AMode = sfBoth then
    AFileCount := 2 * AFileCount;

  fmGrepProgress := TfmGrepProgress.Create(Self);
  try
    fmGrepProgress.Init(cModeProgress[AMode], AFileCount, ASplitCount);
    SaveGrepResultsToFile(Self, gblGrepExpert.HistoryList, gblGrepExpert.HistoryIniVersion,
      ASelResult = gsrSelectAll, AMode, grFile, 'GxGrep.' + TGrepHistoryList.KeyName, FSaveSplitCount);
  finally
    FreeAndNil(fmGrepProgress);
  end;
end;

procedure TfmGrepResults.ClearResultsData;
begin
  if Assigned(FEmbeddedGrepSearch) then
    FEmbeddedGrepSearch.Hide;
  reContext.Clear;
  ContractList(False);
  SetStatusString('');
  SetMatchString('');
  ClearResultsListbox;
end;

procedure TfmGrepResults.actHistoryDeleteExecute(Sender: TObject);
var
  AIndex, ATopIndex: Integer;
  NewItemIndex: Integer;
begin
  AIndex := lbHistoryListIndexForHistoryMenuActions;
  if AIndex = -1 then
   Exit; //==>

  if AIndex = lbHistoryList.ItemIndex then begin
    ClearResultsData;
  end;

  NewItemIndex := lbHistoryList.ItemIndex;
  if NewItemIndex > AIndex then
    Dec(NewItemIndex);  

  gblGrepExpert.HistoryListDeleteFromSettings(delOneItem, AIndex);

  gblGrepExpert.HistoryList.Delete(AIndex);

  ATopIndex := lbHistoryList.TopIndex;
  lbHistoryList.Count := gblGrepExpert.HistoryList.Count;
  if AIndex >= lbHistoryList.Count then
    Dec(AIndex);
  if (ATopIndex < lbHistoryList.Count) and (AIndex < lbHistoryList.Count - 1) then
    lbHistoryList.TopIndex := ATopIndex;

  if lbHistoryList.ItemIndex <> NewItemIndex then
    lbHistoryList.ItemIndex := NewItemIndex;
  ViewHistoryListItems(NewItemIndex, True);
end;

procedure TfmGrepResults.actHistoryDeleteSelectedExecute(Sender: TObject);
resourcestring
  rsDeleteSelectedCaption = 'Select the items to delete';
const
  cSelResult2DelMode: array[gsrSelectItems..gsrSelectAll] of TGrepDeleteMode = (delSelected, delAll);
var
  ASelResult: TGrepSelectResult;
begin
  if lbHistoryList.Count = 0 then
    Exit;

  ASelResult := HistoryListSelect(gstDelete, rsDeleteSelectedCaption);

  if (ASelResult = gsrSelectAll) and (gblGrepExpert.HistoryList.ListMode <> hlmAll) then
    ASelResult := gsrSelectItems;

  if (ASelResult = gsrNoSelection) or
    (ShowGxMessageBox(TGxResultDeleteSelectedQuestion, IfThen(ASelResult = gsrSelectAll, 'A')) <> mrYes)
  then
    Exit;

  gblGrepExpert.HistoryListDeleteFromSettings(cSelResult2DelMode[ASelResult]);

  if ASelResult = gsrSelectAll then
  begin
    ClearResultsData;
    lbHistoryList.Count := 0;
    gblGrepExpert.HistoryList.Clear;
  end
  else
  begin
    gblGrepExpert.HistoryList.DeleteSelected;
    lbHistoryList.Count := gblGrepExpert.HistoryList.Count;
    lbHistoryList.ItemIndex := lbHistoryList.Count - 1;
    ViewHistoryListItems(lbHistoryList.ItemIndex, True);
  end;
end;

procedure TfmGrepResults.actHistoryModifySaveOptionsExecute(Sender: TObject);
resourcestring
  rsSaveOptionAllCaption = 'Select the items to modify save option';
var
  ASelResult: TGrepSelectResult;
begin
  if lbHistoryList.Count = 0 then
    Exit;

  ASelResult := HistoryListSelect(gstSaveOptions, rsSaveOptionAllCaption);
  if ASelResult <> gsrNoSelection then
    lbHistoryList.Refresh;
end;

//Commented, because its not yet work
procedure TfmGrepResults.actHistoryRefreshSelectedExecute(Sender: TObject);
//resourcestring
//  rsRefreshSelectedCaption = 'Choose refreshed items';
//var
//  ASelResult: TGrepSelectResult;
begin
//  if lbHistoryList.Count = 0 then
    Exit;

//  ASelResult := HistoryListSelect(gstRefresh, rsRefreshSelectedCaption);
//
//  if (ASelResult = gsrNoSelection) or
//    ( ShowGxMessageBox(TGxResultRefreshSelectedQuestion//, IfThen(ASelResult = gsrSelectAll, 'A')) <> mrYes )
//  then
//    Exit;
//
//  lbHistoryList.ItemIndex := 0;
//  if ASelResult = gsrSelectAll then
//    Execute(gssRefreshAll)
//  else
//    Execute(gssRefreshSelected);
//
//  ViewHistoryListItems(lbHistoryList.ItemIndex, True);
//  RefreshContextLines;
end;

procedure TfmGrepResults.actHistorySortExecute(Sender: TObject);
resourcestring
  rsSortCaption = 'Sort history items';
var
  ASelResult: TGrepSelectResult;
begin
  if lbHistoryList.Count = 0 then
    Exit;

  ASelResult := HistoryListSelect(gstSort, rsSortCaption);
  if ASelResult <> gsrNoSelection then
  begin
    SetSavedHistoryIndexes(lbHistoryList.ItemIndex);
    gblGrepExpert.HistoryList.SortWithOptions(FNewSortMode, FNewSortDesc);
    GetSavedHistoryIndex;
    lbHistoryList.Refresh;
  end ;
end;

procedure TfmGrepResults.actHistorySearchInHistoryExecute(Sender: TObject);
resourcestring
  rsSearchInCaption = 'Search in the History List';
var
  ASelResult: TGrepSelectResult;
begin
  ASelResult := HistoryListSelect(gstSearchInHistory, rsSearchInCaption);
  if ASelResult <> gsrNoSelection then
  begin
    SetSavedHistoryIndexes(lbHistoryList.ItemIndex);
    gblGrepExpert.HistoryList.UpdateSearchList(FSearchInClearSearchList);
    SetHistoryListMode(hlmSearch, True, False, True);
    gblGrepExpert.HistoryListSaveSearchListSettings;
  end ;
end;

procedure TfmGrepResults.reContextContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
begin
  actContextSelSearch.Enabled := reContext.SelText <> '';
end;

procedure TfmGrepResults.actContextSelSearchExecute(Sender: TObject);
begin
  FContextSearchText := reContext.SelText;
  Execute(gssNormal);
  FContextSearchText := '';
end;

function TfmGrepResults.HistoryListSelect(ASelectType: TGrepSelectType; const ACaption: string): TGrepSelectResult;
var
  frmSelect: TfmGrepSelect;
begin
  frmSelect := TfmGrepSelect.Create(Self);
  try
    FSaveSplitCount := -1;
    FNewSortMode := ghsUnsorted;
    FNewSortDesc := False;
    FSearchInClearSearchList := True;

    frmSelect.Init(gblGrepExpert.HistoryList, ASelectType, ACaption);
    Result := frmSelect.Execute;
    FSelectedCount := frmSelect.SelectedCount;

    case ASelectType of
      gstSave:
        if frmSelect.cbSaveSplitToMoreFiles.Checked then
          FSaveSplitCount := StrToIntDef(frmSelect.eSaveSplitCount.Text, -1);
      gstSort:
      begin
        FNewSortMode := frmSelect.SortMode;
        FNewSortDesc := frmSelect.SortDesc;
      end;
      gstSearchInHistory:
        FSearchInClearSearchList := frmSelect.cbSearchInClearSearchList.Checked;
//      gstDelete:
//        FDeleteSelectedMoveToParamsPage := frmSelect.cbMoveToParamsPage.Checked;
    end;
  finally
    frmSelect.Free;
  end;
end;

procedure TfmGrepResults.SplitterContextCanResize(Sender: TObject; var NewSize: Integer;
  var Accept: Boolean);
var
  PerTenThousand: integer;
  h: integer;
begin
  h := pnlMain.ClientHeight;
  PerTenThousand := MulDiv(NewSize, 10000, h);
  PerTenThousand := ForceBetween(1000, 5000, PerTenThousand);
  NewSize := MulDiv(PerTenThousand, h, 10000);
end;

procedure TfmGrepResults.SplitterContextMoved(Sender: TObject);
begin
  SetLoadedContextHeight(pnlBottom.Height);
end;

procedure TfmGrepResults.SplitterHistoryListCanResize(Sender: TObject; var NewSize: Integer;
  var Accept: Boolean);
var
  w: Integer;
  PerTenThousand: Integer;
begin
  w := pnlMain.ClientWidth;
  PerTenThousand := MulDiv(NewSize, 10000, w);
  PerTenThousand := ForceBetween(1000, 5000, PerTenThousand);
  NewSize := MulDiv(PerTenThousand, w, 10000);
end;

procedure TfmGrepResults.SplitterHistoryListMoved(Sender: TObject);
begin
  if Assigned(FEmbeddedGrepSearch) then
    FEmbeddedGrepSearch.EmbeddedUpdatePos;
end;

end.

