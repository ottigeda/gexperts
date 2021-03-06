{Search history author: (ERT) Ferenc Kiffer, Hungary <kifferferenc@yahoo.com>}

unit GX_GrepResults;

{$I GX_CondDefine.inc}

interface

uses
  Windows, Classes, Graphics, Controls, Forms,
  StdCtrls, ExtCtrls, GX_GrepBackend, GX_GrepExpert, GX_ConfigurationInfo, ComCtrls,
  Menus, DropSource, GX_IdeDock, ActnList, Dialogs,
  ToolWin, Actions;

type
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
    Actions: TActionList;
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
    reContext: TRichEdit;
    SplitterContext: TSplitter;
    mitViewSep1: TMenuItem;
    actViewShowContext: TAction;
    miViewShowMatchContext: TMenuItem;
    actFileSave: TAction;
    actFileCopy: TAction;
    mitFileSave: TMenuItem;
    mitFileCopy: TMenuItem;
    mitFileSep6: TMenuItem;
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
    actHistorySave: TAction;
    miHistoryView: TMenuItem;
    miHistoryRefresh: TMenuItem;
    mitHistorySep2: TMenuItem;
    miHistoryDelete: TMenuItem;
    mitHistorySep3: TMenuItem;
    miHistorySave: TMenuItem;
    mitHistorySep1: TMenuItem;
    miHistoryItemName: TMenuItem;
    actViewShowHistoryList: TAction;
    actViewShowFullFilename: TAction;
    miViewShowHistoryList: TMenuItem;
    mitViewSep2: TMenuItem;
    miViewShowFullFilename: TMenuItem;
    actHistoryDeleteAll: TAction;
    actHistorySaveAll: TAction;
    pmContextMenu: TPopupMenu;
    actContextSelSearch: TAction;
    miContextSearchSelectedText: TMenuItem;
    actHistorySearch: TAction;
    miHistorySearch: TMenuItem;
    actHistoryPrintToFile: TAction;
    actHistorySavePrint: TAction;
    actHistoryPrintAllToFile: TAction;
    actHistorySavePrintAll: TAction;
    miHistoryPrintToFile: TMenuItem;
    miHistorySavePrint: TMenuItem;
    mitFileSep2: TMenuItem;
    mitFileSep4: TMenuItem;
    miFileSaveAll: TMenuItem;
    miFilePrintAllToFile: TMenuItem;
    miFileSavePrintAllToFile: TMenuItem;
    actFilePrintToFile: TAction;
    actFileSavePrint: TAction;
    actFileOpen: TAction;
    miFilePrintToFile: TMenuItem;
    miFileSavePrint: TMenuItem;
    mitFileOpen: TMenuItem;
    mitFileSep3: TMenuItem;
    OpenDialog: TOpenDialog;
    actHistoryRefreshAll: TAction;
    mitFileSep5: TMenuItem;
    miFileRefreshAll: TMenuItem;
    actHistoryModifySearchOptions: TAction;
    miHistoryModifySearchOptions: TMenuItem;
    mitFileDeleteAll: TMenuItem;
    mitFileSep7: TMenuItem;
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
    procedure actFileCopyExecute(Sender: TObject);
    procedure actFileSaveExecute(Sender: TObject);
    procedure actViewStayOnTopExecute(Sender: TObject);
    procedure actFileExitExecute(Sender: TObject);
    procedure actHelpHelpExecute(Sender: TObject);
    procedure actHelpAboutExecute(Sender: TObject);
    procedure actListGotoSelectedExecute(Sender: TObject);
    procedure actListContractExecute(Sender: TObject);
    procedure actListExpandExecute(Sender: TObject);
    procedure ActionsUpdate(Action: TBasicAction; var Handled: Boolean);
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
    procedure actHistorySaveExecute(Sender: TObject);
    procedure actHistoryDeleteExecute(Sender: TObject);
    procedure actViewShowHistoryListExecute(Sender: TObject);
    procedure actViewShowFullFilenameExecute(Sender: TObject);
    procedure actContextSelSearchExecute(Sender: TObject);
    procedure actHistoryDeleteAllExecute(Sender: TObject);
    procedure actHistorySaveAllExecute(Sender: TObject);
    procedure actFileOpenExecute(Sender: TObject);
    procedure actHistoryRefreshAllExecute(Sender: TObject);
    procedure lbHistoryListDblClick(Sender: TObject);
    procedure actHistoryUpdate(Sender: TObject);
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
    FShowHistoryList: Boolean;
    FLoadContextHeight: Integer;
    FLoadHistoryListWidth: Integer;
    FContextSearchText: string;
    FHistoryListClickedEntry: Integer;
    procedure SetStayOnTop(Value: Boolean);
    procedure RefreshContextLines;
    procedure SetShowContext(Value: Boolean);
    procedure HighlightMemo(FileMatches: TFileResult; StartLine, MatchLineNo: Integer);
    procedure StartFileSearch(Sender: TObject; const FileName: string);
    procedure SaveSettings;
    procedure LoadSettings;
    procedure ToggleFileResultExpanded(ListBoxIndex: Integer);
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
    procedure SetShowHistoryList(const Value: Boolean);
    procedure SetShowFullFilename(const Value: Boolean);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure AssignSettingsToForm;
    function ConfigurationKey: string;
    function ConfigWindowKey: String;
  public
    GrepExpert: TGrepExpert;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute(AState: TGrepSearchState);
    procedure UpdateFromSettings;
    procedure InternalSaveSettings(Settings: TGExpertsSettings);
    property StayOnTop: Boolean read GetStayOnTop write SetStayOnTop;
    property ShowContext: Boolean read FShowContext write SetShowContext;
    property ShowFullFilename: Boolean read FShowFullFilename write SetShowFullFilename;
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
  {$IFOPT D+} GX_DbugIntf, {$ENDIF D+}
  SysUtils, Messages, ToolsAPI,
  GX_GenericUtils, GX_OtaUtils, GX_GxUtils, GX_GrepPrinting, GX_GrepSearch,
  GX_GExperts, GX_SharedImages, GX_Replace, GX_GrepReplace,
  GX_MessageBox, GX_IdeUtils, Math, StrUtils, IniFiles;

resourcestring
  SGrepReplaceStats = 'Replaced %d occurrence(s) in %.2f seconds';

type
  TShowUnicodeReplaceMessage = class(TGxMsgBoxAdaptor)
  protected
    function GetMessage: string; override;
    function ShouldShow: Boolean; override;
  end;

  TGxResultRefreshAllQuestion = class(TGxQuestionBoxAdaptor)
  protected
    function GetMessage: string; override;
  end;

  TGxResultDeleteAllQuestion = class(TGxQuestionBoxAdaptor)
  protected
    function GetMessage: string; override;
  end;

procedure GoToMatchLine(MatchLine: TLineResult; SourceEditorInMiddle: Boolean);
var
  MatchFileName: string;
resourcestring
  SCouldNotOpenFile = 'Could not open file %s';
begin
  MatchFileName := TFileResult(MatchLine.Collection).FileName;

  if IsStandAlone then
    GXShellExecute(MatchFileName, '', True)
  else
    GxOtaGoToFileLineColumn(MatchFileName, MatchLine.LineNo, MatchLine.Matches[0].SPos, MatchLine.Matches[0].EPos, SourceEditorInMiddle);
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

{ TGxResultRefreshAllQuestion }

function TGxResultRefreshAllQuestion.GetMessage: string;
begin
  Result := 'Are you sure you want to refresh all search history item lists?';
end;

{ TGxResultDeleteAllQuestion }

function TGxResultDeleteAllQuestion.GetMessage: string;
begin
  Result := 'Are you sure you want to delete all search history items?';
end;

{ TfmGrepResults }

procedure TfmGrepResults.StartFileSearch(Sender: TObject; const FileName: string);
resourcestring
  SProcessing = 'Processing %s';
var
  Dummy: Boolean;
  CurrentGetTickCount: DWORD;
begin
  SetStatusString(Format(SProcessing, [FileName]));
  ActionsUpdate(nil, Dummy);
  CurrentGetTickCount := GetTickCount;
  if CurrentGetTickCount <> FLastRepaintTick then
  begin
    Application.ProcessMessages;
    FLastRepaintTick := CurrentGetTickCount;
  end;
end;

procedure TfmGrepResults.Execute(AState: TGrepSearchState);
resourcestring
  SGrepActive = 'A Grep search is currently active; either abort it or wait until it is finished.';
  SGrepSearchStats = 'Searched %d files in %.2f seconds for "%s"';
var
  TimeStart: TDateTime;
  FilesSearched: Cardinal;
  MatchesFound: Cardinal;
  Cursor: IInterface;
  ResultFiles: TStringList;
  i : Integer;
  AHistoryIndex : Integer;
  ATopIndex: Integer;
begin
  if FSearchInProgress then
    raise Exception.Create(SGrepActive);

  if (AState in [gssNormal, gssSearchAgain, gssSearchModifyOptions]) or not FGrepSettings.CanRefresh then
    if not QueryUserForGrepOptions(AState) then
      Exit;

  if AState = gssSearchModifyOptions then
  begin
    GrepExpert.HistoryList.UpdateGrepSettings(FGrepSettings);
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
    Cursor := TempHourGlassCursor;

    FSearcher := TGrepSearchRunner.Create(FGrepSettings, lbResults.Items, ResultFiles);
    try
      FSearcher.OnSearchFile := StartFileSearch;
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
  lbResults.Sorted := False;

  ATopIndex := lbHistoryList.TopIndex;
  AHistoryIndex := GrepExpert.HistoryList.AddItem(FGrepSettings, lbResults.Items);

  lbHistoryList.Count := GrepExpert.HistoryList.Count;
  lbHistoryList.ItemIndex := AHistoryIndex;
  if (ATopIndex < lbHistoryList.Count) and (AHistoryIndex < lbHistoryList.Count-1) then
    lbHistoryList.TopIndex := ATopIndex;

  RefreshInformation(MatchesFound, GrepExpert.GrepExpandAll, False, True);

  GrepExpert.HistoryListSaveSettings(AHistoryIndex);
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

  if (lbResults.Items.Count = 1) or ADoExpand or GrepExpert.GrepExpandFew or
    (GrepExpert.GrepExpandIf and
      (AMatchesFound <= GrepExpert.GrepExpandIfMatches) and (FilesHit <= GrepExpert.GrepExpandIfFiles)
    )
  then
  begin
    lbResults.ItemIndex := 0;
    ExpandList(AUSedExpandState, ASetExpandState, IfThen(GrepExpert.GrepExpandFew, GrepExpert.GrepExpandFewLines));
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
  lbResults.Refresh;
end;

procedure TfmGrepResults.InternalSaveSettings(Settings: TGExpertsSettings);
var
  WindowSettings: TExpertSettings;
  Percent: integer;
begin
  WindowSettings := Settings.CreateExpertSettings(ConfigWindowKey);
  try
    Settings.SaveForm(Self, ConfigWindowKey);
    WindowSettings.WriteBool('OnTop', StayOnTop);
    WindowSettings.WriteBool('ShowToolBar', ToolBar.Visible);
    WindowSettings.WriteBool('ShowContext', ShowContext);
    WindowSettings.WriteBool('ShowHistoryList', ShowHistoryList);
    if GrepExpert.ContextSaveSize then
    begin
      Percent := (reContext.Height * 100) div ClientHeight;
      WindowSettings.WriteInteger('ContextHeightPercent', Percent);
      Settings.DeleteKey(ConfigWindowKey, 'ContextHeight');
    end;
    if GrepExpert.HistoryListSaveSize then
      WindowSettings.WriteInteger('HistoryListWidth', lbHistoryList.Width);

    WindowSettings.WriteBool('ShowFullFilename', ShowFullFilename);

    WindowSettings.WriteInteger('LastViewedItem', FHistoryListClickedEntry);
  finally
    WindowSettings.Free;
  end;
end;

procedure TfmGrepResults.SaveSettings;
var
  Settings: TGExpertsSettings;
begin
  // Do not localize any of the below strings.
  Settings := TGExpertsSettings.Create;
  try
    InternalSaveSettings(Settings);
  finally
    FreeAndNil(Settings);
  end;
end;

procedure TfmGrepResults.LoadSettings;
var
  WindowSettings: TExpertSettings;
  Settings: TGExpertsSettings;
  AHistoryIniVersion: Integer;
begin
  // Do not localize any of the below strings.
  WindowSettings := nil;
  Settings := TGExpertsSettings.Create;
  try
    AHistoryIniVersion := Settings.ReadInteger(ConfigurationKey, 'HistoryIniVersion', 0);

    WindowSettings := Settings.CreateExpertSettings(ConfigWindowKey);
    Settings.LoadForm(Self, ConfigWindowKey);
    EnsureFormVisible(Self);
    StayOnTop := WindowSettings.ReadBool('OnTop', True);
    ToolBar.Visible := WindowSettings.ReadBool('ShowToolBar', ToolBar.Visible);
    ShowContext := WindowSettings.ReadBool('ShowContext', True);
    FHistoryListClickedEntry := WindowSettings.ReadInteger('LastViewedItem', FHistoryListClickedEntry);

    if AHistoryIniVersion = 0 then
    begin
      ShowHistoryList := WindowSettings.ReadBool('ShowFoundList', True);
      FLoadHistoryListWidth := WindowSettings.ReadInteger('FoundListWidth', lbHistoryList.Width)
    end
    else
    begin
      ShowHistoryList := WindowSettings.ReadBool('ShowHistoryList', True);
      FLoadHistoryListWidth := WindowSettings.ReadInteger('HistoryListWidth', lbHistoryList.Width);
    end;

    if WindowSettings.ValueExists('ContextHeightPercent') then
    begin
      FLoadContextHeight := WindowSettings.ReadInteger('ContextHeightPercent', 20);
    end
    else
    begin
      if Settings.ValueExists(ConfigWindowKey, 'ContextHeight') then
        FLoadContextHeight := WindowSettings.ReadInteger('ContextHeight', reContext.Height)
      else
        FLoadContextHeight := pnlMain.Height - ToolBar.Height - SplitterContext.Height -
          WindowSettings.ReadInteger('ResultsHeight', lbResults.Height);
      // negative means this is an absoulte pixel value rather than a percent value
      FLoadContextHeight := -FLoadContextHeight;
    end;

    ShowFullFilename := WindowSettings.ReadBool('ShowFullFilename', False);
  finally
    FreeAndNil(WindowSettings);
    FreeAndNil(Settings);
  end;
end;

procedure TfmGrepResults.UpdateFromSettings;
begin
  GrepExpert.HistoryList.Enabled := ShowHistoryList;
  if GrepExpert.ContextSaveSize then
  begin
    if FLoadContextHeight < 0 then
      reContext.Height := -FLoadContextHeight
    else
      reContext.Height := (ClientHeight * FLoadContextHeight) div 100;
  end;
  if GrepExpert.HistoryListSaveSize then
    lbHistoryList.Width := FLoadHistoryListWidth;
  if GrepExpert.GrepSaveHistoryListItems and FShowHistoryList and (GrepExpert.HistoryList.Count > 0) then
  begin
    lbHistoryList.Count := GrepExpert.HistoryList.Count;
    if FHistoryListClickedEntry <> -1 then
      lbHistoryList.ItemIndex := FHistoryListClickedEntry
    else
      lbHistoryList.ItemIndex := lbHistoryList.Count-1;
    ViewHistoryListItems(lbHistoryList.ItemIndex, True);
  end;
end;

procedure TfmGrepResults.lbResultsClick(Sender: TObject);
begin
  RefreshContextLines;
end;

procedure TfmGrepResults.actShowMatchContextExecute(Sender: TObject);
begin
  ShowContext := not ShowContext;
end;

procedure TfmGrepResults.SetShowContext(Value: Boolean);
begin
  FShowContext := Value;
  reContext.Visible := ShowContext;
  SplitterContext.Visible := ShowContext;
  RefreshContextLines;
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
    if (ShowContext) and (GrepExpert.NumContextLines > 0) then
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

          BeginLineNo := MatchLineNo - GrepExpert.NumContextLines;
          BeginLineNo := Max(BeginLineNo, 0);
          EndLineNo := MatchLineNo + GrepExpert.NumContextLines;
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
  reContext.SelAttributes.Color := GrepExpert.ContextMatchLineColor;

  for i := StartLine + 1 to StartLine + reContext.Lines.Count + 1 do
  begin
    FileMatches.GetMatchesOnLine(i, Matches);
    for j := 0 to Length(Matches) - 1 do
    begin
      if Matches[j].ShowBold then
      begin
        reContext.SelStart := reContext.Perform(EM_LINEINDEX, i - StartLine - 1, 0) + Matches[j].SPos - 1;
        reContext.SelLength := Matches[j].EPos - Matches[j].SPos + 1;
        reContext.SelAttributes.Color := GrepExpert.ContextMatchColor;
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

procedure TfmGrepResults.ToggleFileResultExpanded(ListBoxIndex: Integer);
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
      if AFileResult.Expanded then
      begin
        while (ListBoxIndex + 1 <= lbResults.Items.Count - 1) and
              (not (lbResults.Items.Objects[ListBoxIndex + 1] is TFileResult)) do
        begin
          lbResults.Items.Delete(ListBoxIndex + 1);
        end;
        AFileResult.Expanded := False;
        AFileResult.ExpandState := False;
      end
      else
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
  begin
    // Paint a search match line number and highlighted match
    ALineResult := lbResults.Items.Objects[Index] as TLineResult;

    if odSelected in State then
    begin
      if GrepExpert.ListUseDefaultColors then
      begin
        nb := clHighLight;
        nf := clHighLightText;
        sb := clWindow;
        sf := clWindowText;
      end
      else
      begin
        nb := GrepExpert.ListMatchBrushColor;
        nf := GrepExpert.ListMatchTextColor;
        sb := clWindow;
        sf := GrepExpert.ListFont.Color;
      end;
    end
    else
    begin
      if GrepExpert.ListUseDefaultColors then
      begin
        sb := clHighLight;
        sf := clHighLightText;
        nb := clWindow;
        nf := clWindowText;
      end
      else
      begin
        sb := GrepExpert.ListMatchBrushColor;
        sf := GrepExpert.ListMatchTextColor;
        nb := clWindow;
        nf := GrepExpert.ListFont.Color;
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
    if ALineResult.Matches.Count > 0 then
    begin
      // Avoid trimming inside the first match :
      Trimmed := 0;
      while (Length(LineText) > Trimmed)
        and CharInSet(LineText[Trimmed + 1], [#9, #32])
        and (Trimmed < ALineResult.Matches[0].SPos - 1) do
          Inc(Trimmed);

      if Trimmed > 0 then
        Delete(LineText, 1, Trimmed);
    end
    else
      Trimmed := LeftTrimChars(LineText);

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

begin
  ResultsCanvas := lbResults.Canvas;
  if lbResults.Items.Objects[Index] is TFileResult then
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
begin
  PrintGrepResults(Self, lbResults.Items, grPrint);
end;

procedure TfmGrepResults.actFileCopyExecute(Sender: TObject);
begin
  if reContext.Focused and (reContext.SelLength > 0) then
    reContext.CopyToClipboard
  else
    PrintGrepResults(Self, lbResults.Items, grCopy);
end;

procedure TfmGrepResults.actFileOpenExecute(Sender: TObject);
var
  AIni: TGrepIniFile;
  AIndex: Integer;
begin
  if not OpenDialog.Execute then
    Exit;

  AIni := TGrepIniFile.Create(OpenDialog.FileName);
  try
    if AIni.SectionExists(TGrepHistoryList.KeyName) then
    begin
      if not GrepExpert.HistoryList.LoadFromSettings(FGrepSettings, AIni, GrepExpert.HistoryIniVersion, ifmSingle, '', True) then
        MessageDlg('Could not read history list', mtWarning, [mbOK], 0);
    end
    else if AIni.SectionExists(TGrepHistoryListItem.SubKeyNameHistory) then
    begin
      AIndex := GrepExpert.HistoryList.LoadItemFromIni(FGrepSettings, AIni, GrepExpert.HistoryIniVersion);
      if AIndex = -1 then
        MessageDlg('Could not read history list from file!', mtWarning, [mbOK], 0)
      else
        ViewHistoryListItems(AIndex, True);
    end;
  finally
    AIni.Free;
  end;
end;

procedure TfmGrepResults.actFileSaveExecute(Sender: TObject);
var
  AItem: TGrepHistoryListItem;
  AMode: TSaveToFileMode;
begin
  AItem := GrepExpert.HistoryList.Items[lbHistoryList.ItemIndex];
  if not Assigned(AItem) then
    Exit;

  if Sender = actFilePrintToFile then
    AMode := sfPrintToFile
  else if Sender = actFileSavePrint then
    AMode := sfBoth
  else
    AMode := sfSaveToLoadable;

  SaveGrepResultsToLoadableFile(Self, AItem, AMode, GrepExpert.HistoryIniVersion, 'GxGrep.' + FGrepSettings.Pattern);
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

  GoToMatchLine(CurrentLine, GrepExpert.GrepMiddle);

  // Hide the results window if the window is not configured to stay on top in D8+ and we are floating
  if GrepExpert.AutoHide and RunningDelphi8OrGreater then begin
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
begin
  inherited;
  FHistoryListClickedEntry := -1;

  SetToolbarGradient(ToolBar);

  FSearchInProgress := False;
  lbResults.DoubleBuffered := True;
  CenterForm(Self);
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
end;

destructor TfmGrepResults.Destroy;
begin
  // XE used to crash here with a "Component already destroyed" error due to the listbox handle being 0 and then recreated in a destructor
  if lbResults.HandleAllocated then
    ClearResultsListbox;

  Self.Abort;
  SaveSettings;

  FreeAndNil(FDragSource);

  inherited Destroy;

  fmGrepResults := nil;
end;

procedure TfmGrepResults.ActionsUpdate(Action: TBasicAction; var Handled: Boolean);
var
  HaveItems: Boolean;
  Processing: Boolean;
begin
  HaveItems := (lbResults.Items.Count > 0);
  Processing := DoingSearchOrReplace;
  actFileSearch.Enabled := not Processing;
  actFileRefresh.Enabled := not Processing;
  actViewOptions.Enabled := not Processing;
  actViewStayOnTop.Enabled := not Processing;
  actFilePrint.Enabled := not Processing and HaveItems;
  actFileSave.Enabled := not Processing and HaveItems;
  actFileCopy.Enabled := not Processing and HaveItems;
  actListGotoSelected.Enabled := not Processing and HaveItems;
  actListGotoSelectedAndClose.Enabled := not Processing and HaveItems;
  actListContract.Enabled := not Processing and HaveItems;
  actListExpand.Enabled := not Processing and HaveItems;
  actFileAbort.Enabled := Processing;
  actViewStayOnTop.Checked := StayOnTop;
  actViewShowContext.Checked := ShowContext;
  actViewToolBar.Checked := ToolBar.Visible;
  actReplaceSelected.Enabled := not Processing and HaveItems;
  actReplaceAll.Enabled := not Processing and HaveItems;
  actViewStayOnTop.Visible := IsStandAlone;
  actViewShowHistoryList.Checked := ShowHistoryList;
  actViewShowFullFilename.Checked := ShowFullFilename;
  tbnSep6.Visible := actViewStayOnTop.Visible;
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
  GrepExpert.Configure;
  AssignSettingsToForm;
  ResizeListBox;
  RefreshContextLines;
  lbHistoryList.Refresh;
end;

procedure TfmGrepResults.FormShow(Sender: TObject);
begin
  AssignSettingsToForm;
  ResizeListBox;
end;

procedure TfmGrepResults.AssignSettingsToForm;
begin
  Assert(Assigned(GrepExpert));
  reContext.Font.Assign(GrepExpert.ContextFont);
  lbResults.Font.Assign(GrepExpert.ListFont);
end;

procedure TfmGrepResults.actReplaceAllExecute(Sender: TObject);
var
  TimeStart: TDateTime;
  MatchesFound: Integer;
  Cursor: IInterface;
begin
  Assert(not DoingSearchOrReplace);

  if not QueryUserForReplaceOptions('All matched files') then
    Exit;

  FReplaceInProgress := True;
  try
    Cursor := TempHourGlassCursor;
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
  Cursor: IInterface;
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
      Cursor := TempHourGlassCursor;
      TimeStart := Now;
      MatchesFound := ReplaceAllInFiles(FileResult, FGrepSettings);
    end
    else if ResultObject is TLineResult then
    begin
      CurrentLine := ResultObject as TLineResult;
      MatchFile := TFileResult(CurrentLine.Collection).FileName;
      if not QueryUserForReplaceOptions(MatchFile + SReplaceLine + IntToStr(CurrentLine.LineNo)) then
        Exit;
      Cursor := TempHourGlassCursor;
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
  StatusBar.Panels.Items[0].Text := StatusStr;
end;

procedure TfmGrepResults.SetMatchString(const MatchStr: string);
begin
  StatusBar.Panels.Items[1].Text := MatchStr;
  if IsEmpty(MatchStr) then
    StatusBar.Panels.Items[1].Width := 0
  else
    StatusBar.Panels.Items[1].Width := StatusBar.Canvas.TextWidth(MatchStr) + 50;
  ResizeStatusBar;
end;

function TfmGrepResults.QueryUserForGrepOptions(AState: TGrepSearchState): Boolean;
resourcestring
  rsSearchAgainCaption = 'Grep Search Again';
  rsSearchModifyOptionCaption = 'Grep Search Modify Options';
var
  Dlg: TfmGrepSearch;
begin
  Result := False;
  Dlg := TfmGrepSearch.Create(nil);
  try
    case AState of
      gssSearchAgain: Dlg.Caption := rsSearchAgainCaption;
      gssSearchModifyOptions: Dlg.Caption := rsSearchModifyOptionCaption;
    end;
    if AState in [gssSearchAgain, gssSearchModifyOptions] then
      Dlg.AdjustSettings(FGrepSettings);
    if ShowModalForm(Dlg) <> mrOk then
      Exit;
    FGrepSettings.CanRefresh := True;
    SetMatchString('');
    Dlg.RetrieveSettings(FGrepSettings);
    if AState <> gssSearchModifyOptions then
      Dlg.GrepExpert.SaveSettings;
    Result := True;
  finally
    FreeAndNil(Dlg);
  end;
end;

function TfmGrepResults.QueryUserForReplaceOptions(const ReplaceInString: string): Boolean;
var
  Dlg: TfmGrepReplace;
begin
  ShowGxMessageBox(TShowUnicodeReplaceMessage);

  Result := False;
  Dlg := TfmGrepReplace.Create(nil);
  try
    Dlg.ReplaceInString := ReplaceInString;
    Dlg.SearchString := FGrepSettings.Pattern;
    if ShowModalForm(Dlg) <> mrOk then
      Exit;
    SetMatchString('');
    Dlg.RetrieveSettings(FGrepSettings);
    Result := True;
  finally
    FreeAndNil(Dlg);
  end;
end;

function TfmGrepResults.ConfigurationKey: string;
begin
  Result := TGrepExpert.ConfigurationKey;
end;

function TfmGrepResults.ConfigWindowKey: String;
begin
  Result := ConfigurationKey + '\Window';
end;

procedure TfmGrepResults.ResizeStatusBar;
begin
  StatusBar.Panels.Items[0].Width := StatusBar.ClientWidth - StatusBar.Panels.Items[1].Width;
end;

procedure TfmGrepResults.SetShowHistoryList(const Value: Boolean);
begin
  if FShowHistoryList <> Value then
  begin
    FShowHistoryList := Value;
    if Assigned(GrepExpert) then
      GrepExpert.HistoryList.Enabled := ShowHistoryList;
    lbHistoryList.Visible := ShowHistoryList;
    if Value then
      SplitterHistoryList.Left := lbHistoryList.Left + lbHistoryList.Width;
    SplitterHistoryList.Visible := ShowHistoryList;
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

procedure TfmGrepResults.ViewHistoryListItems(AIndex: Integer; AUsedExpandState: Boolean);
var
  AHistoryItem: TGrepHistoryListItem;
begin
  FHistoryListClickedEntry := AIndex;
  if AIndex <> -1 then
  begin
    AHistoryItem := GrepExpert.HistoryList.Items[AIndex];
    if Assigned(AHistoryItem) then
    begin
      reContext.Clear;
      ContractList(False);
      SetStatusString('');
      SetMatchString('');
      ClearResultsListbox;
      AHistoryItem.View(lbResults.Items);
      RefreshInformation(AHistoryItem.TotalMatchCount, True, AUsedExpandState, False);
      FGrepSettings := AHistoryItem.GrepSettings;
    end;
  end;
end;

procedure TfmGrepResults.lbHistoryListData(Control: TWinControl; Index: Integer; var Data: string);
begin
  Data := GrepExpert.HistoryList[Index];
end;

procedure TfmGrepResults.lbHistoryListDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  AItem: TGrepHistoryListItem;
begin
  AItem := GrepExpert.HistoryList.Items[Index];
  if not Assigned(AItem) or not (AItem is TGrepHistoryListItem) then
    Exit;

  if not GrepExpert.ListUseDefaultColors then
  begin
    if odSelected in State then
    begin
      lbHistoryList.Canvas.Font.Color := GrepExpert.ListMatchTextColor;
      lbHistoryList.Canvas.Brush.Color := GrepExpert.ListMatchBrushColor;
    end
    else
    begin
      lbHistoryList.Canvas.Font.Color := GrepExpert.ListFont.Color;
      lbHistoryList.Canvas.Brush.Color := clWindow;
    end;
  end;

  lbHistoryList.Canvas.FillRect(Rect);

  lbHistoryList.Canvas.TextOut(Rect.Left + 1, Rect.Top + 1, lbHistoryList.Items[Index]);
  lbHistoryList.Canvas.TextOut(Rect.Left + 10, Rect.Top + 12, Format('%d in %d', [AItem.TotalMatchCount, AItem.ResultList.Count]));
end;

procedure TfmGrepResults.lbHistoryListMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  ClickedEntry: Integer;
begin
  if Button = mbLeft then
  begin
    ClickedEntry := lbHistoryList.ItemAtPos(Point(X, Y), True);
    if ClickedEntry <> -1 then
      ViewHistoryListItems(ClickedEntry, True);
  end;
end;

procedure TfmGrepResults.lbHistoryListContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
var
  AIndex: Integer;
begin
  AIndex := lbHistoryList.ItemAtPos(MousePos, True);
  if AIndex = -1 then
    AIndex := lbHistoryList.ItemIndex;

  pmHistoryMenu.Tag := AIndex;

  if AIndex = -1 then
  begin
    miHistoryItemName.Caption := '[Search text=---]';
    Exit;
  end;

  miHistoryItemName.Caption := Format('[Search text=%s]', [lbHistoryList.Items[AIndex]]);
end;

procedure TfmGrepResults.lbHistoryListDblClick(Sender: TObject);
begin
//
end;

procedure TfmGrepResults.actHistoryViewExecute(Sender: TObject);
begin
  lbHistoryList.ItemIndex := pmHistoryMenu.Tag;
  ViewHistoryListItems(lbHistoryList.ItemIndex, True);
end;

procedure TfmGrepResults.actHistoryRefreshExecute(Sender: TObject);
var
  AItem: TGrepHistoryListItem;
  ASaveSettings: TGrepSettings;
begin
  AItem := GrepExpert.HistoryList.Items[pmHistoryMenu.Tag];
  if not Assigned(AItem) then
    Exit;

  ASaveSettings := FGrepSettings;
  FGrepSettings := AItem.GrepSettings;
  if Sender = actHistoryRefresh then
    actFileRefresh.Execute
  else if Sender = actHistorySearch then
  begin
    Execute(gssSearchAgain);
    RefreshContextLines;
  end
  else if Sender = actHistoryModifySearchOptions then
  begin
    try
      Execute(gssSearchModifyOptions);
    finally
      FGrepSettings := ASaveSettings;
    end;
  end;
end;

procedure TfmGrepResults.actHistorySaveExecute(Sender: TObject);
var
  AItem: TGrepHistoryListItem;
  AMode: TSaveToFileMode;
begin
  AItem := GrepExpert.HistoryList.Items[pmHistoryMenu.Tag];
  if not Assigned(AItem) then
    Exit;

  if Sender = actHistoryPrintToFile then
    AMode := sfPrintToFile
  else if Sender = actHistorySavePrint then
    AMode := sfBoth
  else
    AMode := sfSaveToLoadable;

  SaveGrepResultsToLoadableFile(Self, AItem, AMode, GrepExpert.HistoryIniVersion, 'GxGrep.' + AItem.GrepSettings.Pattern);
end;

procedure TfmGrepResults.actHistoryDeleteExecute(Sender: TObject);
var
  AIndex, ATopIndex: Integer;
  IsCurrent: Boolean;
begin
  AIndex := pmHistoryMenu.Tag;
  if AIndex = -1 then
   Exit;

  IsCurrent := AIndex = lbHistoryList.ItemIndex;
  if IsCurrent then
  begin
    reContext.Clear;
    ContractList(False);
    SetStatusString('');
    SetMatchString('');
    ClearResultsListbox;
  end;

  GrepExpert.HistoryListDeleteFromSettings(AIndex);

  GrepExpert.HistoryList.Items[AIndex].Free;
  GrepExpert.HistoryList.Delete(AIndex);

  ATopIndex := lbHistoryList.TopIndex;
  lbHistoryList.Count := GrepExpert.HistoryList.Count;
  if AIndex >= lbHistoryList.Count then
    Dec(AIndex);
  if (ATopIndex < lbHistoryList.Count) and (AIndex < lbHistoryList.Count-1) then
    lbHistoryList.TopIndex := ATopIndex;

  if IsCurrent and (AIndex > -1) then
  begin
    lbHistoryList.ItemIndex := AIndex;
    ViewHistoryListItems(lbHistoryList.ItemIndex, True);
  end ;
end;

procedure TfmGrepResults.actHistoryDeleteAllExecute(Sender: TObject);
begin
  if (lbHistoryList.Count = 0) or ( ShowGxMessageBox(TGxResultDeleteAllQuestion) <> mrYes )  then
    Exit;

  GrepExpert.HistoryListDeleteFromSettings;

  reContext.Clear;
  ContractList(False);
  SetStatusString('');
  SetMatchString('');
  ClearResultsListbox;

  lbHistoryList.Count := 0;
  GrepExpert.HistoryList.Clear;
end;

procedure TfmGrepResults.actHistorySaveAllExecute(Sender: TObject);
var
  AMode: TSaveToFileMode;
begin
  if lbHistoryList.Count = 0 then
    Exit;

  if Sender = actHistoryPrintAllToFile then
    AMode := sfPrintToFile
  else if Sender = actHistorySavePrintAll then
    AMode := sfBoth
  else
    AMode := sfSaveToLoadable;

  SaveGrepResultsToLoadableFile(Self, GrepExpert.HistoryList, AMode, GrepExpert.HistoryIniVersion, 'GxGrep.' + TGrepHistoryList.KeyName);
end;

procedure TfmGrepResults.actHistoryRefreshAllExecute(Sender: TObject);
begin
  if (lbHistoryList.Count = 0) or ( ShowGxMessageBox(TGxResultRefreshAllQuestion) <> mrYes ) then
    Exit;

  lbHistoryList.ItemIndex := 0;
  Execute(gssRefreshAll);

  ViewHistoryListItems(lbHistoryList.ItemIndex, True);
  RefreshContextLines;
end;

procedure TfmGrepResults.actContextSelSearchExecute(Sender: TObject);
begin
  FContextSearchText := reContext.SelText;
  Execute(gssNormal);
  FContextSearchText := '';
end;

procedure TfmGrepResults.actHistoryUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := lbHistoryList.Count > 0;
end;

end.

