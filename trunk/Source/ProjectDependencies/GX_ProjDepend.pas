unit GX_ProjDepend;

{$I GX_CondDefine.inc}

interface

uses
  Classes, Controls, ComCtrls, ActnList, Menus, ToolWin, ExtCtrls,
  Forms, GX_Experts, GX_OtaUtils, GX_ConfigurationInfo, Dialogs, GX_BaseForm, Actions;

type
  TfmProjDepend = class(TfmBaseForm)
    StatusBar: TStatusBar;
    tvUnits: TTreeView;
    Splitter: TSplitter;
    pnlPageControlHost: TPanel;
    pcData: TPageControl;
    tshUnitUses: TTabSheet;
    lvUnitUses: TListView;
    tshUsedBy: TTabSheet;
    lvUsedBy: TListView;
    tshIndirect: TTabSheet;
    lvIndirect: TListView;
    pmTreeview: TPopupMenu;
    pmList: TPopupMenu;
    pmIndirect: TPopupMenu;
    mitOpenUnitTree: TMenuItem;
    mitListOpenUnit: TMenuItem;
    N2: TMenuItem;
    mitListExportUsedUnits: TMenuItem;
    mitIndirectOpenUnit: TMenuItem;
    N1: TMenuItem;
    mitIndirectUnitProperties: TMenuItem;
    mitIndirectExportUsedUnits: TMenuItem;
    MainMenu: TMainMenu;
    mitFile: TMenuItem;
    mitFileRefresh: TMenuItem;
    mitFileExit: TMenuItem;
    mitHelp: TMenuItem;
    mitHelpHelp: TMenuItem;
    mitHelpSep1: TMenuItem;
    mitHelpAbout: TMenuItem;
    Actions: TActionList;
    ToolBar: TToolBar;
    tbnRefresh: TToolButton;
    tbnAbort: TToolButton;
    tbnSep1: TToolButton;
    tbnHelp: TToolButton;
    actFileRefresh: TAction;
    actFileAbort: TAction;
    actHelpHelp: TAction;
    actHelpContents: TAction;
    actHelpAbout: TAction;
    actFileExit: TAction;
    mitHelpContents: TMenuItem;
    mitFileAbort: TMenuItem;
    actOpenUnitTree: TAction;
    mitFileSep1: TMenuItem;
    actOpenUnitList: TAction;
    actViewIndirectUnitProperties: TAction;
    actFileExportUses: TAction;
    actFileExportIndirectDependencies: TAction;
    actFileFilter: TAction;
    tbnSep2: TToolButton;
    tbnExport: TToolButton;
    tbnSep3: TToolButton;
    tbnFilter: TToolButton;
    mitFileExport: TMenuItem;
    mitFileFilter: TMenuItem;
    mitFileSep2: TMenuItem;
    procedure tvUnitsExpanding(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
    procedure pnlPageControlHostResize(Sender: TObject);
    procedure tvUnitsChange(Sender: TObject; Node: TTreeNode);
    procedure pcDataChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure tvUnitsEditing(Sender: TObject; Node: TTreeNode; var AllowEdit: Boolean);
    procedure lvColumnClick(Sender: TObject; Column: TListColumn);
    procedure lvIndirectCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure actFileRefreshExecute(Sender: TObject);
    procedure actFileAbortExecute(Sender: TObject);
    procedure actFileExitExecute(Sender: TObject);
    procedure actHelpHelpExecute(Sender: TObject);
    procedure actHelpContentsExecute(Sender: TObject);
    procedure actHelpAboutExecute(Sender: TObject);
    procedure actOpenUnitTreeExecute(Sender: TObject);
    procedure actOpenUnitListExecute(Sender: TObject);
    procedure actViewIndirectUnitPropertiesExecute(Sender: TObject);
    procedure tvUnitsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ActionsUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure actFileExportUsesExecute(Sender: TObject);
    procedure actFileFilterExecute(Sender: TObject);
    procedure actFileExportIndirectDependenciesExecute(Sender: TObject);
  private
    FRootNode: TTreeNode;
    ProjectNotifier: TBaseIdeNotifier;
    FLastProject: string;
    FDoRefreshList: Boolean;
    FUnitList: TStringList;
    FFileList: TStringList;
    FSearchInProgress: Boolean;
    FAbortSignalled: Boolean;
    FFilterList: TStringList;
    FSearchPaths: TStringList;
    UnitUsesSortColumn: Integer;
    UsedBySortColumn: Integer;
    IndirectSortColumn: Integer;
    function TryGetSelectedUnit(out _SelectedNode: TTreeNode): Boolean;
    procedure ShowUnitsUsed;
    procedure ShowUsedBy;
    procedure IndirectDepend;
    function GetFileName(const UName: string): string;
    procedure LoadSettings;
    procedure SaveSettings;
    procedure OpenUnit(const UnitName: string);
    function LoadFileDepend(FileName: string; const UnitName, FormName: string): Boolean;
    procedure LoadFileDependencies;
    procedure BuildUses;
    procedure ClearFileList;
    procedure ClearUnitList;
    procedure UpdateFormActions;
    procedure ExportAllDependencies;
    procedure ExportIndirectDependencies;
    function FindUnitInUnitList(const _UnitName: string; out _UsesList: TStringList): Boolean;
    ///<summary>
    /// Fills ProcessedUnitsList with a list of all directly and indirectly used units and
    /// a StringList of the units they are used by in the objects property. The latter
    /// StringLists must be freed by the caller </summary>
    procedure GetIndirectDependencies(_SelectedNode: TTreeNode; _ProcessedUnitsList: TStringList);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

type
  TProjectDependenciesExpert = class(TGX_Expert)
  protected
    procedure UpdateAction(Action: TCustomAction); override;
    procedure InternalSaveSettings(_Settings: IExpertSettings); override;
    procedure InternalLoadSettings(_Settings: IExpertSettings); override;
    procedure SetActive(New: Boolean); override;
  private
    FScanEntireUnit: Boolean;
    FSearchLibraryPath: Boolean;
    FSearchBrowsingPath: Boolean;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetActionCaption: string; override;
    class function GetName: string; override;
    procedure Configure; override;
    procedure Execute(Sender: TObject); override;
    function IsDefaultActive: Boolean; override;
  end;

implementation

{$R *.dfm}

uses
  SysUtils, Windows, ToolsAPI, Math,
  mPasLex, mwPasParserTypes,
  GX_EditReader, GX_ProjDependProp, GX_GExperts, GX_ProjDependFilter,
  GX_GenericUtils, GX_GxUtils, GX_SharedImages, GX_IdeUtils,
  u_dzClassUtils, u_dzVclUtils, GX_ProjDependOptions;

var
  fmProjDepend: TfmProjDepend = nil;
  DependExpert: TProjectDependenciesExpert = nil;

type
  TProjectNotifier = class(TBaseIdeNotifier)
  private
    fmDepend: TfmProjDepend;
  public
    constructor Create(Owner: TfmProjDepend);
    procedure FileNotification(NotifyCode: TOTAFileNotification;
      const FileName: string; var Cancel: Boolean); override;
  end;

constructor TProjectNotifier.Create(Owner: TfmProjDepend);
begin
  inherited Create;
  fmDepend := Owner;
end;

procedure TProjectNotifier.FileNotification(NotifyCode: TOTAFileNotification;
  const FileName: string; var Cancel: Boolean);
begin
  // Is this auto-update a good thing?  Should the user just click Refresh?
  if NotifyCode = ofnActiveProjectChanged then
  begin
    // Don't refresh if the project hasn't actually changed
    if GxOtaGetCurrentProjectFileName = fmDepend.FLastProject then
      Exit;

    fmDepend.ClearFileList;
    fmDepend.ClearUnitList;

    if fmDepend.Visible then
      fmDepend.BuildUses
    else
      fmDepend.FDoRefreshList := True;
  end;
end;

procedure TfmProjDepend.ClearFileList;
var
  i: Integer;
begin
  if FFileList <> nil then
  begin
    for i := 0 to FFileList.Count-1 do
      FFileList.Objects[i].Free;
    FFileList.Clear;
  end;
end;

procedure TfmProjDepend.ClearUnitList;
var
  i: Integer;
begin
  if FUnitList <> nil then
  begin
    for i := 0 to FUnitList.Count-1 do
      FUnitList.Objects[i].Free;
    FUnitList.Clear;
  end;
end;

function TfmProjDepend.LoadFileDepend(FileName: string; const UnitName, FormName: string): Boolean;
var
  FileContent: string;
  Parser: TmwPasLex;
  nUses: Integer;
  Index: Integer;
  Node: TTreeNode;
  UList: TStringList;
  UInfo: TUnitInfo;
  UnitIdentifier: string;
begin
  Result := True;
  if FileName = GxOtaGetCurrentProjectFileName then
    Exit;

  UpdateFormActions;
  Application.ProcessMessages;

  if FAbortSignalled then
  begin
    Result := False;
    Exit;
  end;

  nUses := 0;
  UList := nil;

  StatusBar.SimpleText := FileName;
  StatusBar.Repaint;

  if (FileName = '') and Assigned(FSearchPaths) then begin
    if not FindFileInSearchPath(Unitname+ '.pas', FSearchPaths, FileName) then
      Exit; //==>
  end;


  if not IsDprOrPas(FileName) then
    Exit;

  if FormName = '' then
    Index := ImageIndexUnit
  else
    Index := ImageIndexWindow;

  UInfo := TUnitInfo.Create;
  UInfo.SourceName := UnitName;
  UInfo.FileName := FileName;
  FFileList.AddObject(UnitName, UInfo);

  // Since this edit reader is destroyed almost
  // immediately, do not call FreeFileData
  try
    FileContent := TEditReader.GetText(FileName);
  except
    on E: Exception do
    begin
      // Warn, but skip project files that don't exist (dcu only, etc.?)
      MessageDlg(E.Message, mtWarning, [mbOK], 0);
      Exit;
    end;
  end;
  Parser := TmwPasLex.Create;
  try
    Parser.Origin := @FileContent[1];
    while not (Parser.TokenID in [tkUnit, tkNull, tkLibrary, tkProgram]) do
    begin
      Parser.NextNoJunk;
    end;
    if Parser.TokenID in [tkUnit, tkLibrary, tkProgram] then
    begin
      Parser.NextNoJunk;
      if Parser.TokenID = tkIdentifier then
      begin
        UnitIdentifier := Parser.GetDottedIdentifierAtPos(True);
        Node := tvUnits.Items.AddChild(FRootNode, UnitIdentifier);
        Node.HasChildren := True;
        Node.ImageIndex := Index;
        Node.SelectedIndex := Index;
        UList := TStringList.Create;
        FUnitList.AddObject(UnitIdentifier, UList);
      end;
    end;
    if UList = nil then
      Exit;
    while Parser.TokenID <> tkNull do
    begin
      if Parser.TokenID = tkUses then
      begin
        Inc(nUses);
        Parser.NextNoJunk;
        while not (Parser.TokenID in [tkSemiColon, tkNull]) do
        begin
          if Parser.TokenID = tkIdentifier then
            UList.Add(Parser.GetDottedIdentifierAtPos(True));
          Parser.NextNoJunk;
          if Parser.TokenID = tkIn then
            while not (Parser.TokenID in [tkSemiColon, tkComma, tkNull]) do
              Parser.NextNoJunk;
        end;
      end;
      if (DependExpert <> nil) and (not DependExpert.FScanEntireUnit) and
         (nUses >= 2) then
      begin
        Break;
      end;
      Parser.NextNoJunk;
    end;
  finally
    FreeAndNil(Parser);
  end;
end;

procedure TfmProjDepend.LoadFileDependencies;
var
  i: Integer;
  CurrentProject: IOTAProject;
  ModuleInfo: IOTAModuleInfo;
  ContinueProcessing: Boolean;
begin
  CurrentProject := GxOtaGetCurrentProject;
  if not Assigned(CurrentProject) then
    Exit; //==>

  if Assigned(DependExpert) and DependExpert.FSearchLibraryPath then begin
    FSearchPaths := TStringList.Create;
    if DependExpert.FSearchBrowsingPath then
      GxOtaGetAllPossiblePaths(FSearchPaths, CurrentProject)
    else
      GxOtaGetEffectiveLibraryPath(FSearchPaths, CurrentProject);
  end else
    FreeAndNil(FSearchPaths);

  for i := 0 to CurrentProject.GetModuleCount-1 do
  begin
    ModuleInfo := CurrentProject.GetModule(i);
    UpdateFormActions;
    Application.ProcessMessages;
    ContinueProcessing := LoadFileDepend(ModuleInfo.FileName, ModuleInfo.Name, ModuleInfo.FormName);
    if not ContinueProcessing then
      Break;
  end;
end;

procedure TfmProjDepend.BuildUses;
resourcestring
  SParsingUnits = 'Parsing units...';
var
  Cursor: IInterface;
begin
  ClearFileList;
  ClearUnitList;

  FSearchInProgress := True;
  FAbortSignalled := False;

  tvUnits.Items.BeginUpdate;
  Cursor := TempHourGlassCursor;
  try
    // Clear current scroll box.
    lvIndirect.Items.Clear;
    tvUnits.Items.Clear;

    // Start parsing.
    StatusBar.SimpleText := SParsingUnits;
    StatusBar.Repaint;

    FRootNode := tvUnits.Items.Add(nil, ExtractFileName(GxOtaGetCurrentProjectFileName));

    FRootNode.ImageIndex := ImageIndexWindows;
    FRootNode.SelectedIndex := ImageIndexWindows;

    LoadFileDependencies;

    FRootNode.Expand(False);
    tvUnits.AlphaSort;

    FLastProject := GxOtaGetCurrentProjectFileName;
  finally
    tvUnits.Items.EndUpdate;

    FSearchInProgress := False;
    FAbortSignalled := False;
  end;
end;

function TfmProjDepend.GetFileName(const UName: string): string;
var
  UInfo: TUnitInfo;
  i: Integer;
begin
  Result := '';
  i := FFileList.IndexOf(UName);
  if i >= 0 then
  begin
    UInfo := TUnitInfo(FFileList.Objects[i]);
    Result := UInfo.FileName;
  end;
end;

procedure TfmProjDepend.tvUnitsExpanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
var
  UsesIdx: Integer;
  UseUnitsList: TStringList;
  CNode: TTreeNode;
  UnitName: string;
  SubUsesList: TStringList;
begin
  AllowExpansion := True;
  if Node = FRootNode then
    Exit; //==>

  while Node.Count > 0 do
    Node.Item[0].Free;

  if not  FindUnitInUnitList(Node.Text, UseUnitsList) or (UseUnitsList.Count = 0) then begin
    Node.HasChildren := False;
    Exit; //==>
  end;

  for UsesIdx := 0 to UseUnitsList.Count - 1 do
  begin
    UnitName := UseUnitsList[UsesIdx];
    CNode := tvUnits.Items.AddChild(Node, UnitName);
    CNode.SelectedIndex := ImageIndexUnit;
    CNode.ImageIndex := ImageIndexUnit;
    CNode.HasChildren := FindUnitInUnitList(UnitName, SubUsesList) and (SubUsesList.Count > 0);
  end;
end;

procedure TfmProjDepend.pnlPageControlHostResize(Sender: TObject);
begin
  with lvUnitUses do
    Columns[1].Width := Max(ClientWidth - Columns[0].Width, 0);
  with lvUsedBy do
    Columns[1].Width := Max(ClientWidth - Columns[0].Width, 0);
  with lvIndirect do
    Columns[1].Width := Max(ClientWidth - Columns[0].Width, 0);
end;

procedure TfmProjDepend.tvUnitsChange(Sender: TObject; Node: TTreeNode);
begin
  pcDataChange(pcData);
end;

procedure TfmProjDepend.pcDataChange(Sender: TObject);
begin
  if pcData.ActivePage = tshUnitUses then
    ShowUnitsUsed
  else
  if pcData.ActivePage = tshUsedBy then
    ShowUsedBy
  else
  if pcData.ActivePage = tshIndirect then
    IndirectDepend;

  // A VCL5 bug prevents headers from drawing right unless we poke around here
  pnlPageControlHost.Width := pnlPageControlHost.Width - 1;
end;

function TfmProjDepend.TryGetSelectedUnit(out _SelectedNode: TTreeNode): Boolean;
begin
  _SelectedNode := tvUnits.Selected;
  Result := Assigned(_SelectedNode);
end;

procedure TfmProjDepend.ShowUnitsUsed;
var
  UsesIdx: Integer;
  List: TStringList;
  ListItem: TListItem;
  SelectedNode: TTreeNode;
begin
  lvUnitUses.Items.BeginUpdate;
  try
    lvUnitUses.Items.Clear;

    if not TryGetSelectedUnit(SelectedNode) then
      Exit; //==>

    if SelectedNode = FRootNode then
      List := FUnitList
    else begin
      if not FindUnitInUnitList(SelectedNode.Text, List) then
        Exit; //==>
    end;
    for UsesIdx := 0 to List.Count - 1 do
    begin
      if FFilterList.IndexOf(List.Strings[UsesIdx]) = -1 then
      begin
        ListItem := lvUnitUses.Items.Add;
        ListItem.Caption := List.Strings[UsesIdx];
        ListItem.SubItems.Add(GetFileName(List.Strings[UsesIdx]));
      end;
    end;
  finally
    lvUnitUses.Items.EndUpdate;
  end;
end;

procedure TfmProjDepend.ShowUsedBy;
var
  UnitIdx, UsesIdx: Integer;
  List: TStringList;
  ListItem: TListItem;
  SelectedNode: TTreeNode;
  SelectedText: string;
begin
  lvUsedBy.Items.BeginUpdate;
  try
    lvUsedBy.Items.Clear;

    if not TryGetSelectedUnit(SelectedNode) or (SelectedNode = FRootNode) then
      Exit; //==>

    SelectedText := SelectedNode.Text;
    for UnitIdx := 0 to FUnitList.Count - 1 do
    begin
      List := TStringList(FUnitList.Objects[UnitIdx]);
      for UsesIdx := 0 to List.Count - 1 do
      begin
        if SameText(SelectedText, List.Strings[UsesIdx]) then
        begin
          ListItem := lvUsedBy.Items.Add;
          ListItem.Caption := FUnitList.Strings[UnitIdx];
          ListItem.SubItems.Add(GetFileName(FUnitList.Strings[UnitIdx]));
        end;
      end;
    end;
  finally
    lvUsedBy.Items.EndUpdate;
  end;
end;

procedure TfmProjDepend.GetIndirectDependencies(_SelectedNode: TTreeNode; _ProcessedUnitsList: TStringList);

  procedure AddItemsForUnit(_ProcessedUnitsList: TStringList; const _UnitName: string); forward;

  procedure AddItems(_ProcessedUnitsList: TStringList; _UsedUnitsList: TStrings; const _UsingUnit: string);
  var
    UnitIdx: Integer;
    UsedUnit: string;
    Idx: Integer;
    UsedByUnitsList: TStringList;
  begin
    for UnitIdx := 0 to _UsedUnitsList.Count - 1 do begin
      UsedUnit := _UsedUnitsList[UnitIdx];
      if _ProcessedUnitsList.Find(UsedUnit, Idx) then begin
        UsedByUnitsList := TStringList(_ProcessedUnitsList.Objects[Idx]);
        UsedByUnitsList.Add(_UsingUnit);
      end else begin
        UsedByUnitsList := TStringList_CreateSorted(dupIgnore);
        _ProcessedUnitsList.AddObject(UsedUnit, UsedByUnitsList);
        UsedByUnitsList.Add(_UsingUnit);
        AddItemsForUnit(_ProcessedUnitsList, UsedUnit);
      end;
    end;
  end;

  procedure AddItemsForUnit(_ProcessedUnitsList: TStringList; const _UnitName: string);
  var
    UseUnitsList: TStringList;
    Idx: Integer;
  begin
    // Since we are parsing the uses tree depth first, it is possible that
    // a unit listed in the project has already been processed, so here is an additional
    // check for that case.
    if not _ProcessedUnitsList.Find(_UnitName, Idx) then
      Exit; //==>
    if FindUnitInUnitList(_UnitName, UseUnitsList) then
      AddItems(_ProcessedUnitsList, UseUnitsList, _UnitName);
  end;

  procedure AddItemsForProject(_ProcessedUnitsList: TStringList; const _ProjectName: string);
  begin
    AddItems(_ProcessedUnitsList, FUnitList, ChangeFileExt(_ProjectName, ''));
  end;

begin
  _ProcessedUnitsList.Clear;
  Assert(_ProcessedUnitsList.Sorted);
  if _SelectedNode = FRootNode then begin
    AddItemsForProject(_ProcessedUnitsList, _SelectedNode.Text);
  end else begin
    _ProcessedUnitsList.AddObject(_SelectedNode.Text, TStringList_CreateSorted(dupIgnore));
    AddItemsForUnit(_ProcessedUnitsList, _SelectedNode.Text);
  end;
end;

procedure TfmProjDepend.IndirectDepend;
var
  ProcessedUnitsList: TStringList;
  Cursor: IInterface;
  SelectedNode: TTreeNode;
  i: Integer;
  ListItems: TListItems;
  ListItem: TListItem;
  UsedByList: TStringList;
begin
  if not TryGetSelectedUnit(SelectedNode) then
    Exit; //==>

  Cursor := TempHourGlassCursor;
  ProcessedUnitsList := nil;
  ListItems := lvIndirect.Items;
  ListItems.BeginUpdate;
  try
    ListItems.Clear;
    ProcessedUnitsList := TStringList_CreateSorted(dupError);
    GetIndirectDependencies(SelectedNode, ProcessedUnitsList);
    for i := 0 to ProcessedUnitsList.Count - 1 do begin
      ListItem := ListItems.Add;
      ListItem.Caption := ProcessedUnitsList[i];
      UsedByList := TStringList(ProcessedUnitsList.Objects[i]);
      ListItem.SubItems.Add(UsedByList.CommaText);
      UsedByList.Free;
    end;
  finally
    ListItems.EndUpdate;
    FreeAndNil(ProcessedUnitsList);
  end;
end;

procedure TfmProjDepend.SaveSettings;
var
  Settings: IExpertSettings;
begin
  // Do not localize.
  Settings := TProjectDependenciesExpert.GetSettings;
  Settings.SaveForm('Window', Self);
  Settings.WriteString('ExcludedFiles', FFilterList.CommaText);
  Settings := Settings.Subkey('Window');
  Settings.WriteInteger('Splitter', tvUnits.Width);
end;

procedure TfmProjDepend.LoadSettings;
var
  Settings: IExpertSettings;
begin
  // Do not localize.
  Settings := TProjectDependenciesExpert.GetSettings;
  FFilterList.CommaText := Settings.ReadString('ExcludedFiles', '');
  Settings.LoadForm('Window', Self);
  Settings :=Settings.Subkey('Window');
  tvUnits.Width := Settings.ReadInteger('Splitter', tvUnits.Width);
end;

procedure TfmProjDepend.OpenUnit(const UnitName: string);
resourcestring
  SFileDoesNotExist = '%s does not exist';
  SOpenError = 'Unable to open unit ';
var
  i: Integer;
  CurrentFileName: string;
  ActionServices: IOTAActionServices;
begin
  i := FFileList.IndexOf(UnitName);
  if i >= 0 then
  begin
    CurrentFileName := TUnitInfo(FFileList.Objects[i]).FileName;
    if FileExists(CurrentFileName) then
    begin
      ActionServices := BorlandIDEServices as IOTAActionServices;
      Assert(Assigned(ActionServices));
      if not ActionServices.OpenFile(CurrentFileName) then
        MessageDlg(Format(SOpenError, [CurrentFileName]), mtInformation, [mbOK], 0);
    end
    else
      MessageDlg(Format(SFileDoesNotExist, [CurrentFileName]), mtError, [mbOK], 0);
  end
  else
    if not GxOtaOpenFileFromPath(UnitName + '.pas') then
      MessageDlg(SOpenError + UnitName, mtInformation, [mbOK], 0);
end;

procedure TfmProjDepend.FormActivate(Sender: TObject);
begin
  if FDoRefreshList then
  begin
    FDoRefreshList := False;
    BuildUses;
  end;
end;

procedure TfmProjDepend.tvUnitsEditing(Sender: TObject; Node: TTreeNode;
  var AllowEdit: Boolean);
begin
  AllowEdit := False;
end;

procedure TfmProjDepend.lvColumnClick(Sender: TObject; Column: TListColumn);
begin
  if Sender = lvUnitUses then
    UnitUsesSortColumn := Column.Index;
  if Sender = lvUsedBy then
    UsedBySortColumn   := Column.Index;
  if Sender = lvIndirect then
    IndirectSortColumn := Column.Index;

  (Sender as TCustomListView).AlphaSort;
end;

procedure TfmProjDepend.lvIndirectCompare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
var
  ix: Integer;
  SortColumn: Integer;
begin
  SortColumn := 0;

  if Sender = lvUnitUses then
    SortColumn := UnitUsesSortColumn;
  if Sender = lvUsedBy then
    SortColumn := UsedBySortColumn;
  if Sender = lvIndirect then
    SortColumn := IndirectSortColumn;

  if not (SortColumn in [0, 1]) then
    SortColumn := 0;

  if SortColumn = 0 then
    Compare := AnsiCompareText(Item1.Caption, Item2.Caption)
  else
  begin
    ix := SortColumn - 1;
    Compare := AnsiCompareText(Item1.SubItems[ix], Item2.SubItems[ix]);
  end;
end;

constructor TfmProjDepend.Create(AOwner: TComponent);
begin
  inherited;

  TControl_SetMinConstraints(Self);

  SetToolbarGradient(ToolBar);
  SetNonModalFormPopupMode(Self);
  FFilterList := TStringList.Create;

  pcData.ActivePageIndex := 0;

  FUnitList := TStringList.Create;
  FFileList := TStringList.Create;

  UnitUsesSortColumn := -1;
  IndirectSortColumn := -1;
  UsedBySortColumn := -1;

  CenterForm(Self);

  LoadSettings;

  ProjectNotifier := TProjectNotifier.Create(Self);
  ProjectNotifier.AddNotifierToIDE;
end;

destructor TfmProjDepend.Destroy;
begin
  SaveSettings;

  ClearUnitList;
  ClearFileList;
  FreeAndNil(FSearchPaths);
  FreeAndNil(FUnitList);
  FreeAndNil(FFileList);
  FreeAndNil(FFilterList);

  ProjectNotifier.RemoveNotifierFromIDE;
  ProjectNotifier := nil; // freed by IDE

  inherited Destroy;

  fmProjDepend := nil;
end;

procedure TfmProjDepend.actFileRefreshExecute(Sender: TObject);
begin
  BuildUses;
end;

procedure TfmProjDepend.actFileAbortExecute(Sender: TObject);
begin
  FAbortSignalled := True;
end;

procedure TfmProjDepend.actFileExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfmProjDepend.actHelpHelpExecute(Sender: TObject);
begin
  GxContextHelp(Self, 6);
end;

procedure TfmProjDepend.actHelpContentsExecute(Sender: TObject);
begin
  GxContextHelpContents(Self);
end;

procedure TfmProjDepend.actHelpAboutExecute(Sender: TObject);
begin
  ShowGXAboutForm;
end;

procedure TfmProjDepend.actOpenUnitTreeExecute(Sender: TObject);
var
  SelectedNode: TTreeNode;
begin
  if TryGetSelectedUnit(SelectedNode) then
    OpenUnit(SelectedNode.Text);
end;

procedure TfmProjDepend.actOpenUnitListExecute(Sender: TObject);

  procedure CallOpenUnit(lv: TListView);
  begin
    if lv.Selected <> nil  then
      OpenUnit(lv.Selected.Caption);
  end;

begin
  if pcData.ActivePage = tshUnitUses then
    CallOpenUnit(lvUnitUses)
  else
  if pcData.ActivePage = tshUsedBy then
    CallOpenUnit(lvUsedBy)
  else
  if pcData.ActivePage = tshIndirect then
    CallOpenUnit(lvIndirect);
end;

procedure TfmProjDepend.actViewIndirectUnitPropertiesExecute(
  Sender: TObject);
begin
  if lvIndirect.Selected = nil then
    Exit;

  with TfmProjDependProp.Create(nil) do
  try
    laFileName.Caption := lvIndirect.Selected.Caption;
    lbxSource.Items.CommaText := lvIndirect.Selected.SubItems[0];
    ShowModal;
  finally
    Free;
  end;
end;

procedure TfmProjDepend.tvUnitsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  // Don't allow Grey-'*' for expand all, since we might have circular dependencies
  if Key = VK_MULTIPLY then
    Key := 0;
end;

procedure TfmProjDepend.ActionsUpdate(Action: TBasicAction; var Handled: Boolean);
begin
  UpdateFormActions;
end;

procedure TfmProjDepend.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #27 then
  begin
    Key := #0;
    actFileExit.Execute;
  end;
end;

procedure TfmProjDepend.UpdateFormActions;
begin
  actOpenUnitTree.Enabled := (tvUnits.Selected <> nil);
  actViewIndirectUnitProperties.Enabled := (pcData.ActivePage = tshIndirect);
  actFileRefresh.Enabled := not FSearchInProgress;
  actFileAbort.Enabled := FSearchInProgress;
  actFileExportUses.Enabled := not FSearchInProgress;
  actFileExportIndirectDependencies.Enabled := not FSearchInProgress;
  actFileFilter.Enabled := not FSearchInProgress;
end;

procedure TfmProjDepend.actFileExportIndirectDependenciesExecute(Sender: TObject);
begin
  ExportIndirectDependencies;
end;

procedure TfmProjDepend.actFileExportUsesExecute(Sender: TObject);
begin
  ExportAllDependencies;
end;

procedure TfmProjDepend.actFileFilterExecute(Sender: TObject);
begin
  with TfmProjDependFilter.Create(nil) do
  try
    UnitList := FFilterList;
    if ShowModal = mrOk then
    begin
      FFilterList.Assign(UnitList);
      pcDataChange(pcData);
    end;
  finally
    Free;
  end;
end;

function TfmProjDepend.FindUnitInUnitList(const _UnitName: string; out _UsesList: TStringList): Boolean;
var
  Idx: Integer;
begin
  Idx := FUnitList.IndexOf(_UnitName);
  Result := (Idx >= 0);
  if Result then
    _UsesList := TStringList(FUnitList.Objects[Idx]);
end;

procedure TfmProjDepend.ExportAllDependencies;

  procedure AddItems(ProcessedUnitsList: TStringList; ExportList: TStrings; const BaseUnit, UnitName: string);
  var
    i: Integer;
    UseUnitsList: TStringList;
    ThisUnit: string;
    Idx: Integer;
  begin
    if not FindUnitInUnitList(UnitName, UseUnitsList) then
      Exit; //==>
    // Get list of items used by this project module, and recursively add for each one
    for i := 0 to UseUnitsList.Count-1 do
    begin
      ThisUnit := UseUnitsList.Strings[i];
      if FFilterList.IndexOf(ThisUnit) = -1 then begin
        if ExportList.IndexOf(ThisUnit) = -1 then
          ExportList.Add(ThisUnit);
      end;

      if not ProcessedUnitsList.Find(ThisUnit, Idx) then
      begin
        ProcessedUnitsList.Add(ThisUnit);
        AddItems(ProcessedUnitsList, ExportList, BaseUnit, ThisUnit);
      end;
    end;
  end;

var
  ProcessedUnitsList: TStringList;
  OutputData: TStringList;
  TempData: TStringList;
  i: Integer;
  Cursor: IInterface;
  fn: string;
  UnitName: string;
  j: Integer;
begin
  if not ShowSaveDialog('Export Dependencies', 'csv', fn,
    'CSV Files (*.csv)|*.csv|TXT Files (*.txt)|*.txt') then
    Exit; //==>

  ProcessedUnitsList := nil;
  TempData := nil;
  OutputData := TStringList.Create;
  try
    Cursor := TempHourGlassCursor;
    TempData := TStringList.Create;
    ProcessedUnitsList := TStringList_CreateSorted(dupError);
    for i := 0 to tvUnits.Items.Count - 1 do
    begin
      TempData.Clear;
      UnitName := tvUnits.Items[i].Text;
      AddItems(ProcessedUnitsList, TempData, UnitName, UnitName);
      UnitName := UnitName + ',';
      for j := 0 to TempData.Count - 1 do begin
        OutputData.Add(UnitName + TempData[j]);
      end;
    end;
    OutputData.SaveToFile(fn);
  finally
    FreeAndNil(ProcessedUnitsList);
    FreeAndNil(TempData);
    FreeAndNil(OutputData);
  end;
end;

procedure TfmProjDepend.ExportIndirectDependencies;
var
  ProcessedUnitsList: TStringList;
  i: Integer;
  UnitName: string;
  UsedByList: TStringList;
  OutputData: TStringList;
  UsedByStr: string;
  fn: string;
begin
  if not ShowSaveDialog('Export Indirect Dependencies', 'csv', fn,
    'CSV Files (*.csv)|*.csv|TXT Files (*.txt)|*.txt') then
    Exit; //==>

  OutputData := nil;
  ProcessedUnitsList := TStringList_CreateSorted(dupError);
  try
    GetIndirectDependencies(FRootNode, ProcessedUnitsList);
    OutputData := TStringList.Create;
    for i := 0 to ProcessedUnitsList.Count - 1 do begin
      UnitName := ProcessedUnitsList[i];
      UsedByList := TStringList(ProcessedUnitsList.Objects[i]);
      UsedByStr := StringReplace(UsedByList.CommaText, ',', ' ', [rfReplaceAll]);
      OutputData.Add(UnitName + ',' + UsedByStr);
      UsedByList.Free;
    end;
    OutputData.SaveToFile(fn);
  finally
    FreeAndNil(OutputData);
    FreeAndNil(ProcessedUnitsList);
  end;
end;

{ TProjectDependenciesExpert }

constructor TProjectDependenciesExpert.Create;
begin
  inherited Create;
  DependExpert := Self;
end;

destructor TProjectDependenciesExpert.Destroy;
begin
  DependExpert := nil;

  FreeAndNil(fmProjDepend);

  inherited Destroy;
end;

procedure TProjectDependenciesExpert.SetActive(New: Boolean);
begin
  if New <> Active then
  begin
    inherited SetActive(New);
    if New then //FI:W505
      // Nothing to initialize here.
    else
      FreeAndNil(fmProjDepend);
  end;
end;

function TProjectDependenciesExpert.GetActionCaption: string;
resourcestring
  SMenuCaption = 'Project &Dependencies';
begin
  Result := SMenuCaption;
end;

class function TProjectDependenciesExpert.GetName: string;
begin
  Result := 'ProjectDependencies';
end;

procedure TProjectDependenciesExpert.Configure;
begin
  TfmProjDependOptions.Execute(nil, FScanEntireUnit, FSearchLibraryPath, FSearchBrowsingPath);
end;

procedure TProjectDependenciesExpert.InternalSaveSettings(_Settings: IExpertSettings);
begin
  inherited InternalSaveSettings(_Settings);
  _Settings.WriteBool('ScanEntireUnit', FScanEntireUnit);
  _Settings.WriteBool('SearchLibraryPath', FSearchLibraryPath);
  _Settings.WriteBool('SearchBrowsingPath', FSearchBrowsingPath);
end;

procedure TProjectDependenciesExpert.InternalLoadSettings(_Settings: IExpertSettings);
begin
  inherited InternalLoadSettings(_Settings);
  FScanEntireUnit := _Settings.ReadBool('ScanEntireUnit', False);
  FSearchLibraryPath := _Settings.ReadBool('SearchLibraryPath', False);
  FSearchBrowsingPath := _Settings.ReadBool('SearchBrowsingPath', False);
end;

procedure TProjectDependenciesExpert.Execute(Sender: TObject);
begin
  if fmProjDepend = nil then
  begin
    fmProjDepend := TfmProjDepend.Create(nil);
    SetFormIcon(fmProjDepend);
    fmProjDepend.BuildUses;
  end;
  if fmProjDepend.WindowState = wsMinimized then
    fmProjDepend.WindowState := wsNormal;
  fmProjDepend.Show;

  IncCallCount;
end;

function TProjectDependenciesExpert.IsDefaultActive: Boolean;
begin
  Result := not RunningCPPBuilder;
end;

procedure TProjectDependenciesExpert.UpdateAction(Action: TCustomAction);
begin
  Action.Enabled := GxOtaHaveCurrentProject;
end;

initialization
  RegisterGX_Expert(TProjectDependenciesExpert);
end.

