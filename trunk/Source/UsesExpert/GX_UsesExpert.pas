unit GX_UsesExpert;

interface

{$I 'GX_CondDefine.inc'}

uses
  Windows, SysUtils,
  Classes, Controls, Forms, Menus, ComCtrls, Buttons, ImageList, ImgList,
  ExtCtrls, ActnList, Actions, Dialogs, StdCtrls, Grids, Types,
  GX_ConfigurationInfo, GX_Experts, GX_GenericUtils, GX_BaseForm,
  GX_KbdShortCutBroker, GX_UnitExportsParser, GX_dzCompilerAndRtlVersions,
  GX_OtaUtils;

{$IFOPT D+}
{$IF RTLVersion > RtlVersionDelphiXE}
// System.Diagnostics, which exports TStopWatch, was added to the RTL in DelphiXE2
{$DEFINE DO_TIMING}
{.$DEFINE DEBUG_GRID_DRAWING}
{$IFEND RTLVersion}
{$ELSE}
{$UNDEF DEBUG_GRID_DRAWING}
{$ENDIF}

type
  TPageControl = class(ComCtrls.TPageControl)
  end;

type
  TStringGrid = class(Grids.TStringGrid)
  private
    function GetAssociatedList: TStringList;
    procedure SetAssociatedList(const Value: TStringList);
  published
  public
    property AssociatedList: TStringList read GetAssociatedList write SetAssociatedList;
  end;

type
  TUsesExpert = class(TGX_Expert)
  private
    FAvailTabIndex: Integer;
    FReplaceFileUseUnit: Boolean;
    FParseAll: Boolean;
    FDisableCache: Boolean;
    FOrigFileAddUnitExecute: TNotifyEvent;
    FReadMap: Boolean;
    FProjectChangedNotifier: TProjectChangedNotifier;
    FUnitExportParserThread: TUnitExportParserThread;
    procedure InternalExecute;
    function FindAction(out _Action: TBasicAction): Boolean;
    procedure HandleProjectChanged(_Sender: TObject);
    procedure StartUnitParserThread;
  protected
    procedure InternalLoadSettings(_Settings: IExpertSettings); override;
    procedure InternalSaveSettings(_Settings: IExpertSettings); override;
    function GetBitmapFileName: string; override;
  public
    class function GetName: string; override;
    constructor Create; override;
    destructor Destroy; override;
    // Do any delayed setup after the IDE is done initializing
    procedure AfterIDEInitialized; override;
    // Various methods that will be called
    // at appropriate times
    procedure Configure; override;
    function GetDefaultShortCut: TShortCut; override;
    function GetActionCaption: string; override;
    function HasConfigOptions: Boolean; override;
    procedure Execute(Sender: TObject); override;
  end;

type
  TfmUsesManager = class(TfmBaseForm)
    pnlUnits: TPanel;
    Splitter: TSplitter;
    pm_Intf: TPopupMenu;
    pm_Impl: TPopupMenu;
    m_IntfDelete: TMenuItem;
    m_ImplDelete: TMenuItem;
    m_IntfMove: TMenuItem;
    m_ImplMove: TMenuItem;
    pmuAvail: TPopupMenu;
    mitAvailAddToUses: TMenuItem;
    pnlUses: TPanel;
    pcUnits: TPageControl;
    tabProject: TTabSheet;
    pnlProject: TPanel;
    sg_Project: TStringGrid;
    tabCommon: TTabSheet;
    pnlCommon: TPanel;
    sg_Common: TStringGrid;
    tabFavorite: TTabSheet;
    pnlFavorite: TPanel;
    sg_Favorite: TStringGrid;
    p_Interface: TPanel;
    p_InterfaceTitle: TPanel;
    sg_Interface: TStringGrid;
    p_Implementation: TPanel;
    p_ImplementationTitle: TPanel;
    sg_Implementation: TStringGrid;
    m_IntfOpenUnit: TMenuItem;
    m_ImplOpenUnit: TMenuItem;
    m_IntfSep1: TMenuItem;
    m_IntfSep2: TMenuItem;
    m_ImplSep: TMenuItem;
    m_ImplSep2: TMenuItem;
    pnlFooter: TPanel;
    pnlFavFooter: TPanel;
    btnFavoriteAddToInterface: TButton;
    btnFavoriteAddToImplementation: TButton;
    btnFavoriteAddToFavorites: TButton;
    btnFavoriteDeleteFromFavorites: TButton;
    pnlProjFooter: TPanel;
    btnProjectAddToInterface: TButton;
    btnProjectAddToImplementation: TButton;
    pnlCommonFooter: TPanel;
    btnCommonAddToInterface: TButton;
    btnCommonAddToImplementation: TButton;
    dlgOpen: TOpenDialog;
    ActionList: TActionList;
    actImplDelete: TAction;
    actIntfDelete: TAction;
    actIntfMove: TAction;
    actImplMove: TAction;
    actFavDelUnit: TAction;
    actAvailAddToFav: TAction;
    actAvailAddToImpl: TAction;
    actAvailAddToIntf: TAction;
    b_DeleteFromIntf: TButton;
    b_DeleteFromImpl: TButton;
    b_MoveToImpl: TButton;
    b_MoveToIntf: TButton;
    actOpenUnit: TAction;
    tabSearchPath: TTabSheet;
    pnlSearchPathFooter: TPanel;
    btnSearchPathAddToIntf: TButton;
    btnSearchPathAddToImpl: TButton;
    pnlSearchPath: TPanel;
    sg_SearchPath: TStringGrid;
    pnlAvailableHeader: TPanel;
    edtIdentifierFilter: TEdit;
    edtUnitFilter: TEdit;
    lblFilter: TLabel;
    mitAvailAddToFav: TMenuItem;
    mitAvailSep1: TMenuItem;
    mitAvailSep2: TMenuItem;
    mitAvailOpenUnit: TMenuItem;
    actIntfAddToFavorites: TAction;
    actImplAddToFavorites: TAction;
    m_IntfAddToFavorites: TMenuItem;
    m_ImplAddToFavorites: TMenuItem;
    lblUnits: TPanel;
    lblUses: TPanel;
    pnlButtonsRight: TPanel;
    btnCancel: TButton;
    actOK: TAction;
    btnOK: TButton;
    btnOpen: TButton;
    pnlUsesBottom: TPanel;
    btnAddDots: TButton;
    btnRemoveDots: TButton;
    actUnAlias: TAction;
    m_IntfUnalias: TMenuItem;
    m_ImplUnAlias: TMenuItem;
    tabIdentifiers: TTabSheet;
    pnlIdentifiers: TPanel;
    sg_Identifiers: TStringGrid;
    pnlIdentifiersFooter: TPanel;
    btnIdentifiersAddToIntf: TButton;
    btnIdentifiersAddToImpl: TButton;
    actFocusInterface: TAction;
    actFocusImplementation: TAction;
    tim_Progress: TTimer;
    btnAddSearchPathlToFavorites: TButton;
    btnAddProjectToFavorites: TButton;
    btnAddRtlToFavorites: TButton;
    pm_Favorite: TPopupMenu;
    mi_FavAddToImpl: TMenuItem;
    mi_FavAddtoIntf: TMenuItem;
    actFavAddUnit: TAction;
    N1: TMenuItem;
    mi_FavAddUnit: TMenuItem;
    mi_FavDelUnit: TMenuItem;
    N2: TMenuItem;
    mi_FavOpenUnit: TMenuItem;
    mi_AvailAddToIntf: TMenuItem;
    actAvailAddAllToFav: TAction;
    pnlIdentifiersProgress: TPanel;
    lblIdentifiers: TLabel;
    lblUnitsParsed: TLabel;
    lblUnitsLoaded: TLabel;
    lblUnitsFound: TLabel;
    btnCopySaveCurrentList: TButton;
    btnCopySaveProjectListMenu: TSpeedButton;
    pmCopySaveProjectList: TPopupMenu;
    mi_CopyProjectListToClipboard: TMenuItem;
    mi_SaveProjectList: TMenuItem;
    il_MenuIcons: TImageList;
    dlgSave: TSaveDialog;
    pnlMatchIdentifier: TPanel;
    rbMatchAnyware: TRadioButton;
    rbMatchAtStart: TRadioButton;
    p_NoMapFile: TPanel;
    l_NoMapFile: TLabel;
    b_NoMapFileClose: TButton;
    sbUCM: TStatusBar;
    pmUCMStatusBar: TPopupMenu;
    mCopyThisTextToTheClipboard: TMenuItem;
    mCopyThisFileToTheClipboard: TMenuItem;
    mShowThisFileInWindowsExplorer: TMenuItem;
    mitAvailSep3: TMenuItem;
    mCopyThisIdentifierToTheClipboard: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure sg_ImplementationDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure sg_InterfaceDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure sg_InterfaceDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean);
    procedure sg_ImplementationDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean);
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure actImplDeleteExecute(Sender: TObject);
    procedure actIntfDeleteExecute(Sender: TObject);
    procedure actFavDelUnitExecute(Sender: TObject);
    procedure actAvailAddToFavExecute(Sender: TObject);
    procedure actAvailAddToIntfExecute(Sender: TObject);
    procedure actAvailAddToImplExecute(Sender: TObject);
    procedure sg_InterfaceDblClick(Sender: TObject);
    procedure lbxAvailDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure lbxAvailDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure sg_ImplementationDblClick(Sender: TObject);
    procedure actOpenUnitExecute(Sender: TObject);
    procedure lbxAvailDblClick(Sender: TObject);
    procedure edtIdentifierFilterChange(Sender: TObject);
    procedure edtIdentifierFilterKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure edtUnitFilterChange(Sender: TObject);
    procedure edtUnitFilterKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure actOKExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure actIntfAddToFavoritesExecute(Sender: TObject);
    procedure actImplAddToFavoritesExecute(Sender: TObject);
    procedure btnRemoveDotsClick(Sender: TObject);
    procedure btnAddDotsClick(Sender: TObject);
    procedure actIntfUnAliasExecute(Sender: TObject);
    procedure tabIdentifiersResize(Sender: TObject);
    procedure pcUnitsChange(Sender: TObject);
    procedure sg_MouseDownForDragging(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure pcUnitsResize(Sender: TObject);
    procedure pcUsesResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure pnlUsesResize(Sender: TObject);
    procedure SplitterMoved(Sender: TObject);
    procedure pnlAvailableHeaderResize(Sender: TObject);
    procedure sg_UsedDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect;
      State: TGridDrawState);
    procedure sg_AvailDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect;
      State: TGridDrawState);
    procedure edtIdentifierFilterEnter(Sender: TObject);
    procedure edtIdentifierFilterExit(Sender: TObject);
    procedure edtUnitFilterEnter(Sender: TObject);
    procedure edtUnitFilterExit(Sender: TObject);
    procedure actImplMoveExecute(Sender: TObject);
    procedure actIntfMoveExecute(Sender: TObject);
    procedure pnlUsesBottomResize(Sender: TObject);
    procedure actFocusInterfaceExecute(Sender: TObject);
    procedure actFocusImplementationExecute(Sender: TObject);
    procedure tim_ProgressTimer(Sender: TObject);
    procedure actFavAddUnitExecute(Sender: TObject);
    procedure actAvailAddAllToFavExecute(Sender: TObject);
    procedure btnCopySaveProjectListClick(Sender: TObject);
    procedure btnCopySaveProjectListMenuClick(Sender: TObject);
    procedure mi_CopyProjectListToClipboardClick(Sender: TObject);
    procedure rbMatchAnywareClick(Sender: TObject);
    procedure rbMatchAtStartClick(Sender: TObject);
    procedure mi_SaveProjectListClick(Sender: TObject);
    procedure b_NoMapFileCloseClick(Sender: TObject);
    procedure mCopyThisFileToTheClipboardClick(Sender: TObject);
    procedure mCopyThisIdentifierToTheClipboardClick(Sender: TObject);
    procedure mCopyThisTextToTheClipboardClick(Sender: TObject);
    procedure mShowThisFileInWindowsExplorerClick(Sender: TObject);
    procedure pmuAvailPopup(Sender: TObject);
    procedure pmUCMStatusBarPopup(Sender: TObject);
    procedure pm_FavoritePopup(Sender: TObject);
    procedure sbUCMDblClick(Sender: TObject);
    procedure sg_CommonSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure sg_FavoriteSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure sg_IdentifiersSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure sg_ProjectSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure sg_SearchPathSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure rbMatchAnywareEnter(Sender: TObject);
    procedure rbMatchAtStartEnter(Sender: TObject);
  private
    FLeftRatio: Double;
    FAliases: TStringList;
    FFindThread: TFileFindThread;
    // maintains a list unit name mappings from "originally used" to "currently used"
    // this is necessary to put units which have been switched between using prefixes and
    // not in the correct place of the unit list.
    FOldToNewUnitNameMap: TStringList;
    FCaption_lblFilter: string;
    FForceFocusToIdentifierFilter: Boolean;
    procedure GetCommonUnits;
    procedure GetProjectUnits;
    function TryGetMapFileUnits: Boolean;
    function AddToImplSection(const UnitName: string; RemoveFromInterface: Boolean): Integer;
    function AddToIntfSection(const UnitName: string): Integer;
    procedure DeleteFromIntfSection(const UnitName: string);
    procedure DeleteFromImplSection(const UnitName: string);
    function OpenUnit(const UnitName: string): Boolean;
    procedure ReadUsesList;
    function ApplyAlias(const UnitName: string): string;
    procedure UnAlias(sg: TStringGrid);
    procedure AddListToIntfSection(sg: TObject);
    procedure AddListToImplSection(sg: TObject; RemoveFromInterface: Boolean);
    procedure AddListToFavorites(sg: TStringGrid);
    function HaveSelectedItem(sg: TStringGrid): Boolean;
    procedure DeleteItemIndex(sg: TStringGrid; FromInterface: Boolean);
    procedure DeleteSelected(sg: TStringGrid);
    procedure MoveSelected(Src, Dest: TStringGrid; ToInterface: Boolean);
    function GetAvailableSourceList: TStringGrid;
    procedure SearchPathReady;
    procedure DeleteFromFavorites(const Item: string);
    procedure AddToFavorites(const Item: string);
    procedure FilterVisibleUnits;
    procedure FilterIdentifiers;
    procedure SelectFirstItemInLists;
    procedure SaveChanges;
    function GetListForOpen: TStringGrid;
    procedure OpenSelectedUnit(List: TStringGrid);
    procedure lbxInterfaceFilesDropped(_Sender: TObject; _Files: TStrings);
    procedure lbxImplementationFilesDropped(_Sender: TObject; _Files: TStrings);
    procedure lbxFavoriteFilesDropped(_Sender: TObject; _Files: TStrings);
    procedure LoadFavorites;
    procedure SaveFavorites;
    procedure ResizeIdentiferGrid;
    procedure SwitchUnitsTab(_Direction: Integer);
    function AddToStringGrid(sg: TStringGrid; const UnitName: string): Integer;
    procedure CopyProjectListToClipboard;
    procedure CopyStatusBarTextToClipboard;
    procedure DeleteFromStringGrid(sg: TStringGrid; const UnitName: string);
    function IndexInStringGrid(sg: TStringGrid; const UnitName: string): integer;
    procedure FilterStringGrid(Filter: string; sg: TStringGrid);
    procedure DrawStringGridCell(_sg: TStringGrid; const _Text: string; const _Rect: TRect;
      _State: TGridDrawState; _Focused: Boolean; _Tag: integer);
    procedure SaveProjectListToDisk;
    procedure ShowIdentifiersFilterResult(const cnt: Integer);
    procedure ShowSelectedUnitPathInStatusBar(const ARow: Integer);
  protected
    FProjectUnits: TStringList;
    FCommonUnits: TStringList;
    FFavoriteUnits: TStringList;
    FSearchPathUnits: TStringList;
    FFavUnitsExports: TStringList;
    FUsesExpert: TUsesExpert;
  public
    constructor Create(_Owner: TComponent; _UsesExpert: TUsesExpert); reintroduce;
  end;

implementation

{$R *.dfm}

uses
  Messages, Graphics, StrUtils, Math, ToolsAPI, Clipbrd,
  GX_IdeUtils, GX_UsesManager, GX_dzVclUtils, GX_dzMapFileReader, GX_dzFileUtils,
{$IFOPT D+}
  GX_DbugIntf,
{$ENDIF D+}
  GX_UsesExpertOptions, GX_MessageBox, GX_dzOsUtils;

{ TUsesExpert }

constructor TUsesExpert.Create;
begin
  inherited;
  LoadSettings;
end;

destructor TUsesExpert.Destroy;
var
  act: TBasicAction;
begin
  if Assigned(FUnitExportParserThread) then begin
{$IFOPT D+}
  SendDebug('Freeing UnitExportParserThread');
{$ENDIF D+}
  FreeAndNil(FUnitExportParserThread);
{$IFOPT D+}
  SendDebug('done freeing UnitExportParserThread');
{$ENDIF D+}
  end;

  if Assigned(FProjectChangedNotifier) then begin
    FProjectChangedNotifier.RemoveNotifierFromIDE;
    FProjectChangedNotifier := nil;
  end;

  SaveSettings;

  if Assigned(FOrigFileAddUnitExecute) then
    if FindAction(act) then
      act.OnExecute := FOrigFileAddUnitExecute;

  inherited;
end;

procedure TUsesExpert.AfterIDEInitialized;
var
  act: TBasicAction;
begin
  inherited;

  if FReplaceFileUseUnit then begin
    if FindAction(act) then begin
      FOrigFileAddUnitExecute := act.OnExecute;
      act.OnExecute := Self.Execute;
    end;
  end;
  FProjectChangedNotifier := TProjectChangedNotifier.Create(HandleProjectChanged);
  FProjectChangedNotifier.AddNotifierToIDE;
  HandleProjectChanged(FProjectChangedNotifier);
end;

function TUsesExpert.FindAction(out _Action: TBasicAction): Boolean;
var
  MainMenu: TMainMenu;
  mi: TMenuItem;
begin
  Result := False;
  MainMenu := GxOtaGetIdeMainMenu;
  if not Assigned(MainMenu) then
    Exit;
  if not TMainMenu_FindMenuItem(MainMenu, 'FileUseUnitItem', mi) or not Assigned(mi.Action) then
    Exit;
  if mi.Action.Name = 'FileUseUnitCommand' then begin
    _Action := mi.Action;
    Result := True;
  end;
end;

procedure TUsesExpert.Execute(Sender: TObject);
begin
  InternalExecute;
end;

function TUsesExpert.GetDefaultShortCut: TShortCut;
begin
  Result := scShift + scAlt + Ord('U');
end;

function TUsesExpert.GetActionCaption: string;
resourcestring
  SUsesExpert = '&Uses Clause Manager ...';
begin
  Result := SUsesExpert;
end;

//procedure TUsesExpert.GetHelpString(List: TStrings);
//resourcestring
//  SUsesExpertHelp =
//    '  This expert is designed to help you manage the uses clauses of your Delphi files.  ' +
//    'You can delete and move units between the interface and implementation sections of ' +
//    'the file you are editing using the buttons or drag and drop.  You can also add units from:'#13 +
//    '  - The effective search path (project root, project search paths, IDE library paths)'#13 +
//    '  - The current project'#13 +
//    '  - Delphi''s common VCL/RTL units'#13 +
//    '  - A user-defined favorite units list'#13 +
//    '  The filter control allows case-insensitive filtering of the available unit lists.'#13 +
//    '  There is a "Single action/quick add mode" checkbox that automatically closes ' +
//    'the form when you perform an action on a uses clause or automatically adds the ' +
//    'selected units to the active uses clause when the OK button is selected.';
//begin
//  List.Text := SUsesExpertHelp;
//end;

class function TUsesExpert.GetName: string;
begin
  Result := 'UsesClauseMgr';
end;

function TUsesExpert.GetBitmapFileName: string;
begin
  Result := ClassName;
end;

procedure TUsesExpert.HandleProjectChanged(_Sender: TObject);
begin
  StartUnitParserThread;
end;

procedure TUsesExpert.StartUnitParserThread;
var
  Paths: TStrings;
  CacheDir: string;
  FavoriteUnits: TStringList;
  fn: string;
begin
  FreeAndNil(FUnitExportParserThread);

  if FDisableCache then
    CacheDir := ''
  else
    CacheDir := ConfigInfo.CachingPath + 'UsesExpertCache';

  Paths := TStringList.Create;
  try
    GxOtaGetAllPossiblePaths(Paths);

    if FDisableCache then
      CacheDir := ''
    else
      CacheDir := ConfigInfo.CachingPath + 'UsesExpertCache';
    if FParseAll then begin
{$IFOPT D+}
      SendDebug('Running UnitExportParser thread to get identifiers from all units in search path');
{$ENDIF D+}
      FUnitExportParserThread := TUnitExportParserThread.Create(nil, Paths, CacheDir);
    end else begin
{$IFOPT D+}
      SendDebug('Loading favorites');
{$ENDIF D+}
      FavoriteUnits := TStringList.Create;
      FavoriteUnits.Sorted := False;
      FavoriteUnits.Clear;
      fn := ConfigInfo.ConfigPath + 'FavoriteUnits.txt'; // do not localize
      if FileExists(fn) then
        FavoriteUnits.LoadFromFile(fn);
      FavoriteUnits.Sorted := True;
{$IFOPT D+}
      SendDebugFmt('Done loading %d favorites', [FavoriteUnits.Count]);
{$ENDIF D+}
      if FavoriteUnits.Count > 0 then begin
{$IFOPT D+}
        SendDebug('Running UnitExportParser thread to get identifiers from favorites');
{$ENDIF D+}
        FUnitExportParserThread := TUnitExportParserThread.Create(FavoriteUnits, Paths, CacheDir);
      end;
    end;
  finally
    FreeAndNil(Paths);
  end;
end;

function TUsesExpert.HasConfigOptions: Boolean;
begin
  Result := True;
end;

procedure TUsesExpert.Configure;
var
  act: TBasicAction;
  Found: Boolean;
  CacheDir: string;
begin
  Found := FindAction(act);
  CacheDir := ConfigInfo.CachingPath + 'UsesExpertCache';

  if TfmUsesExpertOptions.Execute(Application, Found, CacheDir, FReadMap, FReplaceFileUseUnit,
    FParseAll, FDisableCache) then begin
    SaveSettings;
    if Found then begin
      if FReplaceFileUseUnit then begin
        if not Assigned(FOrigFileAddUnitExecute) then begin
          FOrigFileAddUnitExecute := act.OnExecute;
          act.OnExecute := Execute;
        end;
      end else begin
        act.OnExecute := FOrigFileAddUnitExecute;
        FOrigFileAddUnitExecute := nil;
      end;
    end;
  end;
end;

procedure TUsesExpert.InternalExecute;
var
  Form: TfmUsesManager;
begin
  AssertIsPasOrInc(GxOtaGetCurrentSourceFile);
  Form := TfmUsesManager.Create(Application, Self);
  try
    if (FAvailTabIndex >= 0) and (FAvailTabIndex < Form.pcUnits.PageCount) then begin
      Form.pcUnits.ActivePageIndex := FAvailTabIndex;
      Form.pcUnits.Change;
    end;

    if Form.ShowModal = mrOk then
    begin
      FAvailTabIndex := Form.pcUnits.ActivePageIndex;

      IncCallCount;
    end;
  finally
    FreeAndNil(Form);
  end;
end;

procedure TUsesExpert.InternalLoadSettings(_Settings: IExpertSettings);
begin
  inherited;
  FReplaceFileUseUnit := _Settings.ReadBool('ReplaceFileUseUnit', False);
  FReadMap := _Settings.ReadBool('ReadMap', True);
  FAvailTabIndex := _Settings.ReadInteger('AvailTabIndex', 0);
  FParseAll := _Settings.ReadBool('ParseAll', True);
  FDisableCache := _Settings.ReadBool('DisableCache', False);
end;

procedure TUsesExpert.InternalSaveSettings(_Settings: IExpertSettings);
begin
  inherited;
  _Settings.WriteBool('ReplaceFileUseUnit', FReplaceFileUseUnit);
  _Settings.WriteBool('ReadMap', FReadMap);
  _Settings.WriteInteger('AvailTabIndex', FAvailTabIndex);
  _Settings.WriteBool('ParseAll', FParseAll);
  _Settings.WriteBool('DisableCache', FDisableCache);
end;

{ TfmUsesManager }

constructor TfmUsesManager.Create(_Owner: TComponent; _UsesExpert: TUsesExpert);
var
  Bitmap: TBitmap;
begin
{$IFOPT D+}
  SendDebug('Creating UsesManager form');
{$ENDIF D+}
  FUsesExpert := _UsesExpert;

  // To ensure we get the latest version of all units, start the thread again
  // it will run fast if everything is already cached.
  FUsesExpert.StartUnitParserThread;

  inherited Create(_Owner);

  DoubleBuffered := True;
  pnlUnits.DoubleBuffered := True;
  pnlUses.DoubleBuffered := True;

  FLeftRatio :=  pnlUses.Width / ClientWidth;

  Bitmap := FUsesExpert.GetBitmap;
  if Assigned(Bitmap) then
    ConvertBitmapToIcon(Bitmap, Icon);

  sg_Identifiers.Cells[0, 0] := 'Identifier';
  sg_Identifiers.Cells[1, 0] := 'Unit';

  TStringGrid_AdjustRowHeight(sg_Interface);
  TStringGrid_AdjustRowHeight(sg_Implementation);
  TStringGrid_AdjustRowHeight(sg_SearchPath);
  TStringGrid_AdjustRowHeight(sg_Project);
  TStringGrid_AdjustRowHeight(sg_Common);
  TStringGrid_AdjustRowHeight(sg_Favorite);
  TStringGrid_AdjustRowHeight(sg_Identifiers);
end;

procedure TfmUsesManager.GetProjectUnits;
var
  IProject: IOTAProject;
  IModuleInfo: IOTAModuleInfo;
  i: Integer;
  FileName: string;
  UnitName: string;
begin
{$IFOPT D+}
  SendDebug('Reading project units');
{$ENDIF D+}
  FProjectUnits.Clear;
  if not FUsesExpert.FReadMap or not TryGetMapFileUnits then begin
    IProject := GxOtaGetCurrentProject;
    if not Assigned(IProject) then
      Exit;
    for i := 0 to IProject.GetModuleCount - 1 do begin
      IModuleInfo := IProject.GetModule(i);
      Assert(IModuleInfo <> nil);

      FileName := IModuleInfo.FileName;
      // We don't want blank names, packages, etc.
      if IsPas(FileName) then begin
        UnitName := ExtractPureFileName(FileName);
        FProjectUnits.Add(UnitName);
      end;
    end;
  end;
{$IFOPT D+}
  SendDebug('Done reading project units');
{$ENDIF D+}
end;

function TfmUsesManager.TryGetMapFileUnits: boolean;
var
  Reader: TMapFileReader;
  MapFile: string;
begin
  Result := GxOtaGetCurrentMapFileName(MapFile);
  try
    if Result then begin
      Reader := TMapFileReader.Create(MapFile);
      try
        FProjectUnits.Assign(Reader.Units);
        Result := (FProjectUnits.Count > 0)
      finally
        FreeAndNil(Reader);
      end;
    end;
  finally
    p_NoMapFile.Visible := not Result;
  end;
end;

procedure TfmUsesManager.GetCommonUnits;
var
  BaseDir: string;
  i: Integer;
  UnitName: string;
begin
{$IFOPT D+}
  SendDebug('Reading common files');
{$ENDIF D+}

  // Read all dcu files from the $(DELPHI)\lib directory (for XE+ use the Win32\Release subdir)
  BaseDir := AddSlash(ExtractFilePath(GetIdeRootDirectory));
  BaseDir := BaseDir + AddSlash('lib');
{$IFDEF GX_DELPHIXE_UP}
  BaseDir := BaseDir + AddSlash('Win32') + AddSlash('Release');
{$ENDIF}
  TSimpleDirEnumerator.EnumFilesOnly(BaseDir + '*.dcu', FCommonUnits, False);
  for i := 0 to FCommonUnits.Count - 1 do begin
    UnitName := ExtractPureFileName(FCommonUnits[i]);
    FCommonUnits[i] := UnitName;
  end;

{$IFOPT D+}
  SendDebug('Done reading common files');
{$ENDIF D+}
end;

procedure TfmUsesManager.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  if ModalResult = mrOk then
    SaveFavorites;
end;

procedure TfmUsesManager.FormCreate(Sender: TObject);

  function RetrieveEditorBlockSelection: string;
  var
    Temp: string;
    i: Integer;
  begin
    Temp := GxOtaGetCurrentSelection;
    // Only use the currently selected text if the length is between 1 and 80
    if (Length(Trim(Temp)) >= 1) and (Length(Trim(Temp)) <= 80) then
    begin
      i := Min(Pos(#13, Temp), Pos(#10, Temp));
      if i > 0 then
        Temp := Copy(Temp, 1, i - 1);
    end else
      Temp := '';
    Result := Temp;
  end;

var
  Selection: string;
begin
  FCaption_lblFilter := lblFilter.Caption; // for showing Filter results
  lblFilter.Tag := -1; // Initialize

  TControl_SetMinConstraints(Self);
  pnlUses.Constraints.MinWidth := pnlUses.Width;

  FProjectUnits := TStringList.Create;
  sg_Project.AssociatedList := FProjectUnits;

  FCommonUnits := TStringList.Create;
  sg_Common.AssociatedList := FCommonUnits;

  FFavoriteUnits := TStringList.Create;
  sg_Favorite.AssociatedList := FFavoriteUnits;

  FSearchPathUnits := TStringList.Create;
  sg_SearchPath.AssociatedList := FSearchPathUnits;

  FFavUnitsExports := TStringList.Create;
  FAliases := TStringList.Create;
  FOldToNewUnitNameMap := TStringList.Create;

  LoadFavorites;

  tim_Progress.Enabled := True;

  FFindThread := TFileFindThread.Create;
  FFindThread.FileMasks.Add('*.pas');
  FFindThread.FileMasks.Add('*.dcu');
  GxOtaGetEffectiveLibraryPath(FFindThread.SearchDirs);
  FFindThread.OnFindComplete := SearchPathReady;
  FFindThread.StartFind;
{$IFOPT D+}
  SendDebug('Started SearchPath FindThread');
{$ENDIF D+}

  pcUnits.ActivePage := tabSearchPath;
  GxOtaGetUnitAliases(FAliases);

  GetCommonUnits;
  GetProjectUnits;

  ReadUsesList;

  TWinControl_ActivateDropFiles(sg_Interface, lbxInterfaceFilesDropped);
  TWinControl_ActivateDropFiles(sg_Implementation, lbxImplementationFilesDropped);
  TWinControl_ActivateDropFiles(sg_Favorite, lbxFavoriteFilesDropped);

  Selection := RetrieveEditorBlockSelection;
  if Trim(Selection) = '' then begin
    try
      Selection := GxOtaGetCurrentIdent;
    except
       // if access violation created
      on E: Exception do
        Selection := '';
    end;
  end;
  edtIdentifierFilter.Text := Selection;
  edtIdentifierFilter.SelectAll;
end;

procedure FreePChars(_sl: TStrings);
var
  i: Integer;
  p: PChar;
begin
  if not Assigned(_sl) then
    Exit; //==>
  for i := 0 to _sl.Count - 1 do begin
    p := PChar(_sl.Objects[i]);
    _sl.Objects[i] := nil;
    if Assigned(p) then
     try
      StrDispose(p);
     except
       on e: Exception do begin
         {$IFOPT D+}SendDebugError(E.Message);{$ENDIF}
       end;
     end;
  end;
end;

procedure TfmUsesManager.FormDestroy(Sender: TObject);
begin
  tim_Progress.Enabled := False;

  if Assigned(FFindThread) then begin
    FFindThread.OnFindComplete := nil;
    FFindThread.Terminate;
  end;

  FreeAndNil(FOldToNewUnitNameMap);
  FreeAndNil(FAliases);
  FreeAndNil(FFindThread);
  FreeAndNil(FProjectUnits);
  FreeAndNil(FCommonUnits);
  FreeAndNil(FFavoriteUnits);

  FreePChars(FSearchPathUnits);
  FreeAndNil(FSearchPathUnits);

  FreeAndNil(FFavUnitsExports);
end;

procedure TfmUsesManager.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (ssAlt in Shift) and edtIdentifierFilter.Focused then begin
    if Key = Ord('A') then begin
      FForceFocusToIdentifierFilter := True;
    end else if Key = Ord('T') then begin
      FForceFocusToIdentifierFilter := True;
    end;
  end;
end;

procedure TfmUsesManager.FormResize(Sender: TObject);
begin
  pnlUses.Width := Round(FLeftRatio * ClientWidth);
end;

procedure TfmUsesManager.lbxInterfaceFilesDropped(_Sender: TObject; _Files: TStrings);
var
  i: Integer;
  s: string;
begin
  for i := 0 to _Files.Count - 1 do begin
    s := _Files[i];
    AddToIntfSection(ChangeFileExt(ExtractFileName(s), ''));
  end;
end;

procedure TfmUsesManager.lbxImplementationFilesDropped(_Sender: TObject; _Files: TStrings);
var
  i: Integer;
  s: string;
begin
  for i := 0 to _Files.Count - 1 do begin
    s := _Files[i];
    AddToImplSection(ChangeFileExt(ExtractFileName(s), ''), True);
  end;
end;

procedure TfmUsesManager.lbxFavoriteFilesDropped(_Sender: TObject; _Files: TStrings);
var
  i: Integer;
  s: string;
begin
  for i := 0 to _Files.Count - 1 do begin
    s := _Files[i];
    AddToFavorites(ChangeFileExt(ExtractFileName(s), ''));
  end;
end;

procedure TfmUsesManager.AddListToIntfSection(sg: TObject);
var
  i: Integer;
  grid: TStringGrid;
  col: Integer;
begin
  if sg is TStringGrid then begin
    grid := TStringGrid(sg);
    col := grid.ColCount - 1;
    for i := grid.Selection.Top to grid.Selection.Bottom do
      AddToIntfSection(grid.Cells[col, i]);
  end;
end;

function TfmUsesManager.AddToStringGrid(sg: TStringGrid; const UnitName: string): Integer;
var
  i: Integer;
  cnt: Integer;
  Found: Boolean;
begin
  Result := -1;
  Found := False;
  cnt := sg.RowCount;
  for i := sg.FixedRows to cnt - 1 do begin
    if SameText(sg.Cells[0, i], UnitName) then begin
      Result := i;
      Found := True;
      Break;
    end;
  end;
  if not found then begin
    if (cnt = sg.FixedRows + 1) and (sg.Cells[0, sg.FixedRows] = '') then
      cnt :=sg.FixedRows;
    sg.RowCount := cnt + 1;
    sg.Cells[0, cnt] := UnitName;
    Result := cnt;
  end;
  sg.Row:= Result;
end;

procedure TfmUsesManager.DeleteFromStringGrid(sg: TStringGrid; const UnitName: string);
var
  i: Integer;
  cnt: Integer;
  j: Integer;
begin
  cnt := sg.RowCount;
  for i := sg.FixedRows to cnt - 1 do begin
    if SameText(sg.Cells[0, i], UnitName) then begin
      for j := i + 1 to cnt - 1 do
        sg.Cells[0, j - 1] := sg.Cells[0, j];
      Dec(cnt);
      TGrid_SetNonfixedRowCount(sg, cnt);
      if cnt = 0 then
        sg.Cells[0, sg.FixedRows] := '';
      Exit; //==>
    end;
  end;
end;

function TfmUsesManager.AddToIntfSection(const UnitName: string): Integer;
begin
  Result := AddToStringGrid(sg_Interface, UnitName);
  DeleteFromStringGrid(sg_Implementation, UnitName);
end;

procedure TfmUsesManager.DeleteFromIntfSection(const UnitName: string);
begin
  DeleteFromStringGrid(sg_Interface, UnitName);
end;

function TfmUsesManager.OpenUnit(const UnitName: string): Boolean;
var
  FileName: string;
begin
  FileName := UnitName + '.pas';
  Result := GxOtaOpenFileFromPath(FileName);
end;

procedure TfmUsesManager.pcUnitsChange(Sender: TObject);
begin
  sbUCM.SimpleText := ''; // keep it consistent

  if pcUnits.ActivePage = tabIdentifiers then
  begin
    if lblFilter.Tag <> -1 then // if identifier filter results number has previously been set
      ShowIdentifiersFilterResult(lblFilter.Tag) // then restore identifier filter results number
    else // if identifier filter results number has NOT previously been set
    begin
      // if identifier filter term has been set e.g. automatically by IDE editor selection:
      if Trim(edtIdentifierFilter.Text) <> '' then
        FilterIdentifiers;
    end;
    edtIdentifierFilter.Visible := True;
    TWinControl_SetFocus(edtIdentifierFilter);
    edtUnitFilter.Visible := False;
    lblFilter.FocusControl := edtIdentifierFilter;
  end
  else
  begin
    lblFilter.Caption := FCaption_lblFilter; // reset filter label caption
    edtUnitFilter.Visible := True;
    TWinControl_SetFocus(edtUnitFilter);
    edtIdentifierFilter.Visible := False;
    lblFilter.FocusControl := edtUnitFilter;
  end;
end;

procedure TfmUsesManager.pcUnitsResize(Sender: TObject);
begin
  TGrid_Resize(sg_SearchPath, [roUseGridWidth, roUseAllRows]);
  TGrid_Resize(sg_Project, [roUseGridWidth, roUseAllRows]);
  TGrid_Resize(sg_Common, [roUseGridWidth, roUseAllRows]);
  TGrid_Resize(sg_Favorite, [roUseGridWidth, roUseAllRows]);
end;

procedure TfmUsesManager.pcUsesResize(Sender: TObject);
begin
  TGrid_Resize(sg_Interface, [roUseGridWidth, roUseAllRows]);
  TGrid_Resize(sg_Implementation, [roUseGridWidth, roUseAllRows]);
end;

procedure TfmUsesManager.pnlAvailableHeaderResize(Sender: TObject);
begin
  edtUnitFilter.Left := 0;
  edtUnitFilter.Width := pnlAvailableHeader.Width;
  edtIdentifierFilter.Left := 0;
  edtIdentifierFilter.Width := pnlAvailableHeader.Width;
end;

procedure TfmUsesManager.pnlUsesBottomResize(Sender: TObject);
var
  w: Integer;
begin
  w := (pnlUsesBottom.ClientWidth - 3 * 8) div 4;
  b_DeleteFromIntf.Width := w;
  b_MoveToImpl.Width := w;
  b_MoveToImpl.Left := w + 8;
  b_MoveToIntf.Width := w;
  b_MoveToIntf.Left := 2 * (w + 8);
  b_DeleteFromImpl.Width := w;
  b_DeleteFromImpl.Left := 3 * (w + 8);

  btnAddDots.Width := 2 * w + 8;
  btnRemoveDots.Width := 2 * w + 8;
  btnRemoveDots.Left := 2 * (w + 8);
end;

procedure TfmUsesManager.pnlUsesResize(Sender: TObject);
begin
  p_Interface.Width := pnlUses.ClientWidth div 2;
end;

procedure TfmUsesManager.AddListToImplSection(sg: TObject; RemoveFromInterface: Boolean);
var
  i: Integer;
  col: Integer;
  grid: TStringGrid;
begin
  if sg is TStringGrid then begin
    grid := TStringGrid(sg);
    col := grid.ColCount - 1;
    for i := grid.Selection.Top to grid.Selection.Bottom do
      AddToImplSection(grid.Cells[col, i], RemoveFromInterface);
  end;
end;

function TfmUsesManager.AddToImplSection(const UnitName: string; RemoveFromInterface: Boolean): Integer;
begin
  if RemoveFromInterface then
    DeleteFromStringGrid(sg_Interface, UnitName);
  Result := AddToStringGrid(sg_Implementation, UnitName);
end;

procedure TfmUsesManager.DeleteFromImplSection(const UnitName: string);
begin
  DeleteFromStringGrid(sg_Implementation, UnitName);
end;

const
  ALIAS_PREFIX = ' (-> ';

function TfmUsesManager.ApplyAlias(const UnitName: string): string;
var
  i: Integer;
begin
  Result := UnitName;
  i := FAliases.IndexOfName(Result);
  if i <> -1 then
    Result := UnitName + ALIAS_PREFIX + FAliases.Values[Result] + ')';
end;

procedure TfmUsesManager.UnAlias(sg: TStringGrid);
var
  i: Integer;
  s: string;
  p: Integer;
begin
  for i := sg.RowCount - 1 downto sg.FixedRows do begin
    s := sg.cells[0, i];
    p := Pos(' ', s);
    if p > 0 then begin
      s := Copy(s, 1, p - 1);
      sg.cells[0, i] := s;
    end;
  end;
end;

function TfmUsesManager.IndexInStringGrid(sg: TStringGrid; const UnitName: string): integer;
var
  col: Integer;
  i: Integer;
begin
  col := sg.ColCount - 1;
  for i := sg.FixedRows to sg.RowCount - 1 do begin
    if SameText(sg.Cells[col, i], UnitName) then begin
      Result := i;
      Exit;
    end;
  end;
  Result := -1;
end;

procedure TfmUsesManager.ReadUsesList;
var
  i: Integer;
  UsesManager: TUsesManager;
  sl: TStringList;
begin
{$IFOPT D+}
  SendDebug('Reading uses lists');
{$ENDIF D+}
  TStringGrid_Clear(sg_Interface);
  TStringGrid_Clear(sg_Implementation);

  sl := nil;
  UsesManager := TUsesManager.Create(GxOtaGetCurrentSourceEditor);
  try
    sl := TStringList.Create;
    UsesManager.InterfaceUses.AssignTo(sl);
    sl.Sort;
    for i := 0 to sl.Count - 1 do
      sl[i] := ApplyAlias(sl[i]);
    TStringGrid_AssignCol(sg_Interface, 0, sl);
    TGrid_Resize(sg_Interface, [roUseGridWidth, roUseAllRows]);

    UsesManager.ImplementationUses.AssignTo(sl);
    sl.Sort;
    for i := 0 to sl.Count - 1 do
      sl[i] := ApplyAlias(sl[i]);
    TStringGrid_AssignCol(sg_Implementation, 0, sl);
    TGrid_Resize(sg_Implementation, [roUseGridWidth, roUseAllRows]);
  finally
    FreeAndNil(sl);
    FreeAndNil(UsesManager);
  end;
{$IFOPT D+}
  SendDebug('Done reading uses lists');
{$ENDIF D+}
end;

procedure TfmUsesManager.sg_ImplementationDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  AddListToImplSection(Source, True);
end;

procedure TfmUsesManager.sg_InterfaceDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  AddListToIntfSection(Source);
end;

procedure TfmUsesManager.sg_ImplementationDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept := (Source = sg_Project) or (Source = sg_Common) or
    (Source = sg_Favorite) or (Source = sg_SearchPath) or (Source = sg_Interface);
end;

procedure TfmUsesManager.sg_InterfaceDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept := (Source = sg_Project) or (Source = sg_Common) or
    (Source = sg_Favorite) or (Source = sg_SearchPath) or (Source = sg_Implementation);
end;

procedure TfmUsesManager.DrawStringGridCell(_sg: TStringGrid; const _Text: string; const _Rect: TRect;
  _State: TGridDrawState; _Focused: Boolean; _Tag: Integer);
var
  cnv: TCanvas;
begin
  cnv := _sg.Canvas;
  if _Text = '' then
    cnv.Brush.Color := _sg.Color
  else begin
    if gdSelected in _State then begin
      if not _Focused then begin
        cnv.Brush.Color := clDkGray;
        // I would have used clHighlightText but that becomes unreadable when theming is active
        cnv.Font.Color := clWhite;
      end;
    end else begin
      if _Tag <> 0  then begin
        cnv.Brush.Color := clYellow;
      end;
    end;
  end;
{$IFDEF DEBUG_GRID_DRAWING}
  SendDebugFmt('Drawing grid %s: DefaultRowHeight: %d Rect.Left: %d .Top: %d  .Width: %d .Height: %d',
    [_sg.Name, _sg.DefaultRowHeight, _Rect.Left, _Rect.Top, _Rect.Right - _Rect.Left, _Rect.Bottom - _Rect.Top]);
{$ENDIF}
  cnv.FillRect(_Rect);
  cnv.TextRect(_Rect, _Rect.Left + 2, _Rect.Top + 2, _Text);
end;

procedure TfmUsesManager.sg_UsedDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect;
  State: TGridDrawState);
var
  sg: TStringGrid absolute Sender;
begin
  DrawStringGridCell(sg, sg.Cells[ACol, ARow], Rect, State, sg.Focused, Integer(sg.Objects[ACol, ARow]));
end;

procedure TfmUsesManager.sg_MouseDownForDragging(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  sg: TStringGrid;
  SourceCol: Integer;
  SourceRow: Integer;
begin
  if button = mbLeft then begin
    sg := Sender as TStringGrid;
    // Convert mouse coordinates X, Y to col and row indices
    sg.MouseToCell(X, Y, SourceCol, SourceRow);
    // Allow dragging only if a non fixed cell was clicked
    if (SourceCol >= sg.FixedCols) and (SourceRow >= sg.FixedCols) then begin
      // Begin dragging after mouse has moved 16 pixels
      sg.BeginDrag(False, 16);
    end;
  end;
end;

procedure TfmUsesManager.ActionListUpdate(Action: TBasicAction; var Handled: Boolean);

  procedure SetShortcut(_Shortcut: TShortCut; _act: TCustomAction);
  var
    i: Integer;
    act: TCustomAction;
  begin
    for i := 0 to ActionList.ActionCount-1 do begin
      act := ActionList.Actions[i] as TCustomAction;
      if act.ShortCut = _Shortcut then
        act.ShortCut := 0;
    end;
    if Assigned(_act) then
      _act.ShortCut := _Shortcut;
  end;

var
  HasSelectedItem: Boolean;
  AvailableSourceList: TStringGrid;
  ActiveLBHasSelection: Boolean;
begin
  HasSelectedItem :=     HaveSelectedItem(sg_Interface);
  actIntfMove.Enabled :=HasSelectedItem;
  actIntfDelete.Enabled := HasSelectedItem;
  actIntfAddToFavorites.Enabled := HasSelectedItem;

  HasSelectedItem := HaveSelectedItem(sg_Implementation);
  actImplMove.Enabled := HasSelectedItem;
  actImplDelete.Enabled := HasSelectedItem;
  actImplAddToFavorites.Enabled := HasSelectedItem;

  AvailableSourceList := GetAvailableSourceList;
  ActiveLBHasSelection := HaveSelectedItem(AvailableSourceList);

  actAvailAddToImpl.Enabled := ActiveLBHasSelection;
  actAvailAddToIntf.Enabled := ActiveLBHasSelection;
  actAvailAddToFav.Enabled := ActiveLBHasSelection;
  actFavDelUnit.Enabled := HaveSelectedItem(sg_Favorite);

  if ActiveControl = sg_Interface then begin
    SetShortcut(VK_INSERT, actIntfAddToFavorites);
    SetShortcut(VK_DELETE, actIntfDelete);
    SetShortcut(ShortCut(Ord('M'), [ssCtrl]), actIntfMove);
  end else if ActiveControl = sg_Implementation then begin
    SetShortcut(VK_INSERT, actImplAddToFavorites);
    SetShortcut(VK_DELETE, actImplDelete);
    SetShortcut(ShortCut(Ord('M'), [ssCtrl]), actImplMove);
  end else if ActiveControl = sg_Favorite then begin
    SetShortcut(VK_INSERT, actFavAddUnit);
    SetShortcut(VK_DELETE, actFavDelUnit);
    SetShortcut(ShortCut(Ord('M'), [ssCtrl]), nil);
  end else begin
    SetShortcut(VK_INSERT, nil);
    SetShortcut(VK_DELETE, nil);
    SetShortcut(ShortCut(Ord('M'), [ssCtrl]), nil);
  end;

  actOpenUnit.Enabled := HaveSelectedItem(GetListForOpen);
end;

function TfmUsesManager.HaveSelectedItem(sg: TStringGrid): Boolean;
begin
  Result := False;
  if not Assigned(sg) then
    Exit; //==>

  Result := sg.Selection.Bottom - sg.Selection.Top >= 0;
end;

procedure TfmUsesManager.DeleteSelected(sg: TStringGrid);
var
  i: Integer;
  col: Integer;
begin
  Assert(Assigned(sg));
  col := sg.ColCount - 1;
  for i := sg.Selection.Bottom downto sg.Selection.Top do begin
    DeleteFromStringGrid(sg, sg.Cells[col, i])
  end;
end;

procedure TfmUsesManager.MoveSelected(Src, Dest: TStringGrid; ToInterface: Boolean);
var
  i: Integer;
  col: Integer;
  UnitName: string;
begin
  Assert(Assigned(Src) and Assigned(Dest));
  col := Src.ColCount - 1;
  for i := Src.Selection.Bottom downto Src.Selection.Top do
  begin
    UnitName := Src.Cells[col, i];

    if ToInterface then
      AddToIntfSection(UnitName)
    else
      AddToImplSection(UnitName, True);
    end;
end;

procedure TfmUsesManager.actFavDelUnitExecute(Sender: TObject);
var
  i: Integer;
begin
  for i := sg_Favorite.Selection.Bottom downto sg_Favorite.Selection.Top do
    DeleteFromFavorites(sg_Favorite.Cells[0, i]);
end;

procedure TfmUsesManager.actFocusImplementationExecute(Sender: TObject);
begin
  TWinControl_SetFocus(sg_Implementation);
end;

procedure TfmUsesManager.actFocusInterfaceExecute(Sender: TObject);
begin
  TWinControl_SetFocus(sg_Interface);
end;

procedure TfmUsesManager.actFavAddUnitExecute(Sender: TObject);
var
  i: Integer;
  FileName: string;
begin
  dlgOpen.InitialDir := ExtractFilePath(GetIdeRootDirectory);
  if dlgOpen.Execute then begin
    for i := 0 to dlgOpen.Files.Count - 1 do begin
      FileName := ExtractPureFileName(dlgOpen.Files[i]);
      AddToFavorites(FileName);
    end;
  end
end;

procedure TfmUsesManager.actAvailAddAllToFavExecute(Sender: TObject);
var
  OnSelCell: TSelectCellEvent;
  i: Integer;
  FileName: string;
  Src: TStringGrid;
begin
  FileName := '';
  OnSelCell := sg_Favorite.OnSelectCell;
  try
    sg_Favorite.OnSelectCell := nil;

    Src := GetAvailableSourceList;
    for i := Src.FixedRows to Src.RowCount - 1 do begin
      FileName := Src.Cells[0, i];
      if FileName <> '' then
        AddToFavorites(FileName);
    end;
  finally
    sg_Favorite.OnSelectCell := OnSelCell;
  end;
  edtUnitFilter.Text := '';
  pcUnits.ActivePage := tabFavorite;
  pcUnits.Change;
end;

procedure TfmUsesManager.actAvailAddToFavExecute(Sender: TObject);
var
  i: Integer;
  FileName: string;
  Src: TStringGrid;
begin
  FileName := '';
  Src := GetAvailableSourceList;
  for i := Src.Selection.Bottom downto Src.Selection.Top do begin
    FileName := Src.Cells[0, i];
    if FileName <> '' then
      AddToFavorites(FileName);
  end;
  edtUnitFilter.Text := '';
  pcUnits.ActivePage := tabFavorite;
  pcUnits.Change;
end;

procedure TfmUsesManager.actAvailAddToIntfExecute(Sender: TObject);
var
  Src: TStringGrid;
  i: Integer;
  Col: Integer;
  Row: Integer;
begin
  src := GetAvailableSourceList;
  Col := src.ColCount - 1;
  Row := -1;
  for i := Src.Selection.Bottom downto Src.Selection.Top do
    Row := AddToIntfSection(Src.Cells[Col, i]);
  if Row <> -1 then
    sg_Interface.Row := Row;
end;

procedure TfmUsesManager.actAvailAddToImplExecute(Sender: TObject);
var
  i: Integer;
  Src: TStringGrid;
  Col: Integer;
  Row: Integer;
begin
  src := GetAvailableSourceList;
  Col := src.ColCount - 1;
  Row := -1;
  for i := Src.Selection.Bottom downto Src.Selection.Top do begin
    Row := AddToImplSection(Src.Cells[col, i], True);
  end;
  if Row <> -1 then
    sg_Implementation.Row := Row;
end;

function TfmUsesManager.GetAvailableSourceList: TStringGrid;
begin
  Result := nil;
  if pcUnits.ActivePage = tabProject then
    Result := sg_Project
  else if pcUnits.ActivePage = tabCommon then
    Result := sg_Common
  else if pcUnits.ActivePage = tabFavorite then
    Result := sg_Favorite
  else if pcUnits.ActivePage = tabSearchPath then
    Result := sg_SearchPath
  else if pcUnits.ActivePage = tabIdentifiers then
    Result := sg_Identifiers;
  Assert(Assigned(Result));
end;

procedure TfmUsesManager.sg_InterfaceDblClick(Sender: TObject);
begin
  if IsCtrlDown then
    OpenSelectedUnit(sg_Interface)
  else
    DeleteItemIndex(sg_Interface, True);
end;

procedure TfmUsesManager.sg_AvailDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect;
  State: TGridDrawState);
var
  sg: TStringGrid absolute Sender;
  GridFocused: Boolean;
begin
  GridFocused := sg.Focused or edtUnitFilter.Focused or edtIdentifierFilter.Focused;
  DrawStringGridCell(sg, sg.Cells[ACol, ARow], Rect, State, GridFocused, 0);
end;

procedure TfmUsesManager.sg_ImplementationDblClick(Sender: TObject);
begin
  if IsCtrlDown then
    OpenSelectedUnit(sg_Implementation)
  else
    DeleteItemIndex(sg_Implementation, False);
end;

procedure TfmUsesManager.DeleteItemIndex(sg: TStringGrid; FromInterface: Boolean);
var
  col: Integer;
  row: Integer;
begin
  row := sg.row;
  if (row >= sg.FixedRows) and (row < sg.RowCount) then
    begin
      col := sg.ColCount - 1;
      if FromInterface then
        DeleteFromIntfSection(sg.Cells[col, row])
      else
        DeleteFromImplSection(sg.Cells[col, row]);
    end;
end;

procedure TfmUsesManager.lbxAvailDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := (Source = sg_Interface) or (Source = sg_Implementation);
end;

procedure TfmUsesManager.lbxAvailDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  if Sender = sg_Favorite then begin
    AddListToFavorites(Source as TStringGrid)
  end;
  if Source = sg_Interface then begin
    actIntfDelete.Execute;
  end else if Source = sg_Implementation then begin
    actImplDelete.Execute;
  end;
end;

function TfmUsesManager.GetListForOpen: TStringGrid;
begin
  if ActiveControl = sg_Implementation then
    Result := sg_Implementation
  else if ActiveControl = sg_Interface then
    Result := sg_Interface
  else
    Result :=  GetAvailableSourceList;
end;

{ TSourcetNotFoundMessage }

type
  TSourcetNotFoundMessage = class(TGxMsgBoxAdaptor)
  protected
    function GetMessage: string; override;
  end;

function TSourcetNotFoundMessage.GetMessage: string;
resourcestring
  SUnitNotFound =
    'The source code for this unit could not be found.';
begin
  Result := SUnitNotFound;
end;

procedure TfmUsesManager.OpenSelectedUnit(List: TStringGrid);
var
  UnitName: string;
  col: Integer;
begin
  Assert(Assigned(List));

  col := List.ColCount-1;
  UnitName := List.Cells[col, List.row];
  if OpenUnit(UnitName) then
    ModalResult := mrCancel
  else
    ShowGxMessageBox(TSourcetNotFoundMessage);
end;

procedure TfmUsesManager.actOpenUnitExecute(Sender: TObject);
begin
  OpenSelectedUnit(GetListForOpen);
end;

procedure TfmUsesManager.lbxAvailDblClick(Sender: TObject);
// Todo: make it work also on Identifiers tab!
var
  Src: TStringGrid;
  col: Integer;
  UnitName: string;
begin
  Src := GetAvailableSourceList;
  Assert(Assigned(Src));
  col := Src.ColCount - 1;
  UnitName := Src.Cells[col, Src.row];
  if IsCtrlDown then begin
    OpenUnit(UnitName);
    ModalResult := mrCancel;
  end else begin
    AddToImplSection(UnitName, True)
  end;
end;

procedure TfmUsesManager.SearchPathReady;
var
  PathFiles: TStringList;
  PathUnits: TStringList;
  i: Integer;
  FileName: string;
  IsDotNet: Boolean;

  procedure AddPathUnit(FileName: string);
  var
    UnitName: string;
  begin
    if IsDotNet then begin
      // do we really care about dotNET in Delphi any more?
      if IsDCU(FileName) then
        Exit; //==>
    end else if IsDCUIL(FileName) then
      Exit; //==>
    UnitName := ExtractPureFileName(FileName);
    if IsPas(FileName) then begin
      PathUnits.AddObject(UnitName, Pointer(StrNew(PChar(FileName))));
    end else begin
      PathUnits.Add(UnitName);
    end;
  end;

begin
  if not Assigned(FFindThread) then begin
    // If it is not assigned, something went wrong in the form's constructor
    // which freed the thread but a synchronise call was still waiting to be executed.
    Exit; //==>
  end;
{$IFOPT D+}
  SendDebug('SarchPath is ready');
{$ENDIF D+}
  IsDotNet := GxOtaCurrentProjectIsDotNet;
  PathFiles := nil;
  PathUnits := TStringList.Create;
  try
    PathFiles := TStringList.Create;
    PathUnits.Sorted := True;
    PathUnits.Duplicates := dupIgnore;
    FFindThread.LockResults;
    try
      PathFiles.Assign(FFindThread.Results);
    finally
      FFindThread.ReleaseResults;
    end;
{$IFOPT D+}
  SendDebugFmt('Found %d files in SarchPath', [PathFiles.Count]);
{$ENDIF D+}
    for i := 0 to PathFiles.Count - 1 do
      AddPathUnit(PathFiles[i]);
    GxOtaGetProjectFileNames(GxOtaGetCurrentProject, PathFiles);
    for i := 0 to PathFiles.Count - 1 do
    begin
      FileName := PathFiles[i];
      if IsPas(FileName) then
        AddPathUnit(FileName);
    end;
    FSearchPathUnits.Assign(PathUnits);
    FSearchPathUnits.Sorted := True;
  finally
    FreeAndNil(PathUnits);
    FreeAndNil(PathFiles);
  end;
  FilterVisibleUnits;
  sg_SearchPath.Color := clWindow;
  sg_SearchPath.Enabled := True;
end;

procedure TfmUsesManager.DeleteFromFavorites(const Item: string);
begin
  DeleteStringFromList(FFavoriteUnits, Item);
  DeleteFromStringGrid(sg_Favorite, Item);
end;

procedure TfmUsesManager.AddToFavorites(const Item: string);
begin
  EnsureStringInList(FFavoriteUnits, Item);
  AddToStringGrid(sg_Favorite, Item);
end;

procedure TfmUsesManager.edtIdentifierFilterChange(Sender: TObject);
begin
  FilterIdentifiers;
end;

procedure TfmUsesManager.edtIdentifierFilterEnter(Sender: TObject);
begin
  if FForceFocusToIdentifierFilter then begin
    // keep the edit from selecting everything which would be annoying
    edtIdentifierFilter.SelStart := edtIdentifierFilter.SelStart + edtIdentifierFilter.SelLength;
    FForceFocusToIdentifierFilter := False;
  end;
  sg_Identifiers.Invalidate;
end;

procedure TfmUsesManager.edtIdentifierFilterExit(Sender: TObject);
begin
  sg_Identifiers.Invalidate;
end;

procedure TfmUsesManager.SplitterMoved(Sender: TObject);
begin
  FLeftRatio :=  pnlUses.Width / ClientWidth;
end;

procedure TfmUsesManager.SwitchUnitsTab(_Direction: Integer);
var
  i: Integer;
begin
  i := pcUnits.ActivePageIndex + _Direction;
  if i = pcUnits.PageCount then
    i := 0
  else if i < 0 then
    i := pcUnits.PageCount - 1;
  pcUnits.ActivePageIndex := i;
  pcUnits.Change;
end;

procedure TfmUsesManager.edtIdentifierFilterKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_TAB) and (Shift = [ssCtrl]) then
  begin
    SwitchUnitsTab(1);
  end else if (Key = VK_TAB) and (Shift = [ssCtrl, ssShift]) then begin
    SwitchUnitsTab(-1);
  end else begin
    if (Key in [VK_DOWN, VK_UP, VK_NEXT, VK_PRIOR]) then
    begin
      sg_Identifiers.Perform(WM_KEYDOWN, Key, 0);
      Key := 0;
    end;
  end;
end;

procedure TfmUsesManager.edtUnitFilterChange(Sender: TObject);
begin
  FilterVisibleUnits;
end;

procedure TfmUsesManager.edtUnitFilterEnter(Sender: TObject);
begin
  sg_Project.Invalidate;
  sg_Common.Invalidate;
  sg_Favorite.Invalidate;
  sg_SearchPath.Invalidate;
end;

procedure TfmUsesManager.edtUnitFilterExit(Sender: TObject);
begin
  sg_Project.Invalidate;
  sg_Common.Invalidate;
  sg_Favorite.Invalidate;
  sg_SearchPath.Invalidate;
end;

procedure TfmUsesManager.FilterStringGrid(Filter: string; sg: TStringGrid);
var
  FilterList: TStrings;
  List: TStrings;
begin
  FilterList := TStringList.Create;
  try
    List := sg.AssociatedList;
    FilterStringList(List, FilterList, Filter, False);
    TStringGrid_AssignCol(sg, 0, FilterList, True);
    TGrid_Resize(sg, [roUseGridWidth, roUseAllRows]);
  finally
    FreeAndNil(FilterList);
  end;
end;

procedure TfmUsesManager.FilterVisibleUnits;
var
  Filter: string;
begin
  Filter := Trim(edtUnitFilter.Text);
  FilterStringGrid(Filter, sg_Favorite);
  FilterStringGrid(Filter, sg_Project);
  FilterStringGrid(Filter, sg_Common);
  FilterStringGrid(Filter, sg_SearchPath);

  SelectFirstItemInLists;
end;

procedure TfmUsesManager.FilterIdentifiers;
var
  i: Integer;
  Identifier: string;
  UnitName: PChar;
  Filter: string;
  FixedRows: Integer;
  FilterList: TStrings;
  MatchAnywhere: Boolean;
  cnt: Integer;
begin
{$IFOPT D+}
  SendDebug('Filtering identifiers');
{$ENDIF D+}
  Filter := Trim(edtIdentifierFilter.Text);

  MatchAnywhere := rbMatchAnyware.Checked;
  FilterList := TStringList.Create;
  try
    if Filter = '' then
      FilterList.Assign(FFavUnitsExports)
    else
      FilterStringList(FFavUnitsExports, FilterList, Filter, False, MatchAnywhere);
    cnt := FilterList.Count;
    TGrid_SetNonfixedRowCount(sg_Identifiers, cnt);
    if cnt = 0 then
      TStringGrid_Clear(sg_Identifiers)
    else begin
      FixedRows := sg_Identifiers.FixedRows;
      TGrid_SetNonfixedRowCount(sg_Identifiers, cnt);
      for i := 0 to cnt - 1 do begin
        Identifier := FilterList[i];
        sg_Identifiers.Cells[0, FixedRows + i] := Identifier;
        UnitName := PChar(FilterList.Objects[i]);
        sg_Identifiers.Cells[1, FixedRows + i] := ExtractPureFileName(UnitName);
      end;
    end;
    ShowIdentifiersFilterResult(cnt);
  finally
    FreeAndNil(FilterList);
  end;
{$IFOPT D+}
  SendDebug('Done filtering identifiers');
{$ENDIF D+}
  ResizeIdentiferGrid;
end;

procedure TfmUsesManager.ShowIdentifiersFilterResult(const cnt: Integer);
begin
  if pcUnits.ActivePage = tabIdentifiers then
  begin
    lblFilter.Caption := FCaption_lblFilter + ': ' + IntToStr(cnt) + ' results';
    lblFilter.Tag := cnt; // remember filter results number
  end;
end;

procedure TfmUsesManager.ResizeIdentiferGrid;
begin
  TGrid_Resize(sg_Identifiers, [roUseGridWidth, roUseAllRows]);
  TGrid_RestrictToGridWdith(sg_Identifiers, [1]);
  pnlIdentifiersProgress.Left := (sg_Identifiers.Width - pnlIdentifiersProgress.Width) div 2;
end;

procedure TfmUsesManager.edtUnitFilterKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  sg: TStringGrid;
begin
  if (Key = VK_TAB) and (Shift = [ssCtrl]) then
  begin
    SwitchUnitsTab(1);
  end else if (Key = VK_TAB) and (Shift = [ssCtrl, ssShift]) then begin
    SwitchUnitsTab(-1);
  end else begin
    if (Key in [VK_DOWN, VK_UP, VK_NEXT, VK_PRIOR]) then
    begin
      sg := GetAvailableSourceList;
      Assert(Assigned(sg));
      if sg.RowCount > sg.FixedRows then
        sg.Perform(WM_KEYDOWN, Key, 0)
      else
        sg.Row := sg.FixedRows;
      Key := 0;
    end;
  end;
end;

procedure TfmUsesManager.SelectFirstItemInLists;

  procedure SelectBestItem(sg: TStringGrid);
  var
    Filter: string;
    MatchIndex: Integer;
  begin
    if sg.RowCount > sg.FixedRows then
    begin
      Filter := Trim(edtUnitFilter.Text);
      MatchIndex := IndexInStringGrid(sg, Filter);
      if MatchIndex = -1 then
        MatchIndex := sg.FixedRows;
      sg.Row := MatchIndex;
    end;
  end;

begin
  SelectBestItem(sg_Common);
  SelectBestItem(sg_Favorite);
  SelectBestItem(sg_SearchPath);
  SelectBestItem(sg_Project);
end;

procedure TfmUsesManager.tabIdentifiersResize(Sender: TObject);
begin
  ResizeIdentiferGrid;
end;

procedure TfmUsesManager.LoadFavorites;
var
  fn: string;
begin
{$IFOPT D+}
  SendDebug('Loading favorites');
{$ENDIF D+}
  FFavoriteUnits.Sorted := False;
  FFavoriteUnits.Clear;
  fn := ConfigInfo.ConfigPath + 'FavoriteUnits.txt'; // do not localize
  if FileExists(fn) then
    FFavoriteUnits.LoadFromFile(fn);
  FFavoriteUnits.Sorted := True;
{$IFOPT D+}
  SendDebugFmt('Done loading %d favorites', [FFavoriteUnits.Count]);
{$ENDIF D+}
end;

procedure TfmUsesManager.tim_ProgressTimer(Sender: TObject);
var
  IdentIdx: Integer;
  FixedRows: Integer;
  UnitFile: string;
  PCharUnitFile: PChar;
  UnitName: string;
  Identifier: string;
  sl: TStrings;
  Idx: Integer;
  upt: TUnitExportParserThread;
begin
  upt := FUsesExpert.FUnitExportParserThread;
  if Assigned(upt) and not upt.HasFinished then begin
    lblUnitsFound.Caption := Format('Units found: %d', [upt.FoundUnitsCount]);
    lblUnitsParsed.Caption := Format('Units parsed: %d', [upt.ParsedUnitsCount]);
    lblUnitsLoaded.Caption := Format('Units loaded: %d', [upt.LoadedUnitsCount]);
    lblIdentifiers.Caption := Format('Identifiers found: %d', [upt.Identifiers.Count]);
    pnlIdentifiersProgress.Visible := True;
  end else begin
    pnlIdentifiersProgress.Visible := False;
    tim_Progress.Enabled := False;
    pnlIdentifiersProgress.Visible := False;
    if not Assigned(upt) then
      Exit; //==>

    FixedRows := sg_Identifiers.FixedRows;
    sl := upt.Identifiers;

{$IFOPT D+}
    SendDebugFmt('UnitExportParser finished, found %d identifiers', [sl.Count]);
    SendDebugFmt('UnitExportParser found %d units', [upt.FoundUnitsCount]);
    SendDebugFmt('UnitExportParser loaded %d units', [upt.LoadedUnitsCount]);
    SendDebugFmt('UnitExportParser parsed %d units', [upt.ParsedUnitsCount]);
{$IFDEF DO_TIMING}
    SendDebugFmt('UnitExportParser searching time %d ms', [upt.SearchingTimeMS]);
    SendDebugFmt('UnitExportParser loading time %d ms', [upt.LoadingTimeMS]);
    SendDebugFmt('UnitExportParser inserting time %d ms', [upt.InsertingTimeMS]);
    SendDebugFmt('UnitExportParser parsing time %d ms', [upt.ParsingTimeMS]);
    SendDebugFmt('UnitExportParser processing time %d ms', [upt.ProcessingTimeMS]);
    SendDebugFmt('UnitExportParser sorting time %d ms', [upt.SortingTimeMS]);
    SendDebugFmt('UnitExportParser total time %d ms', [upt.TotalTimeMS]);
{$ENDIF}
{$ENDIF D+}

{$IFOPT D+}
    SendDebug('Preprocessing identifiers');
{$ENDIF D+}
    sg_Identifiers.RowCount := FixedRows + 1;
    sg_Identifiers.Cells[0, FixedRows] := '';
    sg_Identifiers.Cells[1, FixedRows] := '';
    for IdentIdx := 0 to sl.Count - 1 do begin
      Identifier := sl[IdentIdx];
      // make sure the string is valid and not freed in the thread
      UniqueString(Identifier);
      UnitFile := PChar(sl.Objects[IdentIdx]);
      UnitName := ExtractPureFileName(UnitFile);
      if FSearchPathUnits.Find(UnitName, Idx) then begin
        UnitName := FSearchPathUnits[Idx];
        PCharUnitFile := PChar(FSearchPathUnits.Objects[Idx]);
        if PCharUnitFile = nil then begin
          PCharUnitFile := StrNew(PChar(UnitFile));
          FSearchPathUnits.Objects[Idx] := Pointer(PCharUnitFile);
        end;
        FFavUnitsExports.AddObject(Identifier, Pointer(PCharUnitFile));
      end else begin
{$IFOPT D+}
        SendDebugFmt('Unit for identifier %s (%s) not found in SearchPathUnits', [Identifier, UnitName]);
{$ENDIF D+}
      end;
    end;
{$IFOPT D+}
    SendDebug('Done preprocessing identifiers');
{$ENDIF D+}
    FilterIdentifiers;
  end;
end;

type
  TShowAddDotsMessage = class(TGxQuestionBoxAdaptor)
  protected
    function GetMessage: string; override;
  end;

{ TShowAddDotsMessage }

function TShowAddDotsMessage.GetMessage: string;
resourcestring
  SConfirmAddDots =
    'This will try to add namespace qualifiers to all unit names ' + sLineBreak
    + 'in both uses clauses.' + sLineBreak
    + sLineBreak
    + 'Example:' + sLineBreak
    + '"Registry" will be changed to "System.Win.Registry".' + sLineBreak
    + sLineBreak
    + 'This is meant to update projects to newer Delphi versions.' + sLineBreak
    + sLineBreak
    + 'Do you want to proceed?';
begin
  Result := SConfirmAddDots;
end;

procedure TfmUsesManager.btnAddDotsClick(Sender: TObject);
var
  DefaultNamespace: string;
  NameSpaces: TStringList;

  procedure AddDotsTo(sg: TStringGrid);
  var
    UnitIdx: Integer;
    OrigUnitName: string;
    NewUnitName: string;
    s: string;
    NsIdx: Integer;
    SearchPathUnitsIndex: Integer;
  begin
    for UnitIdx := sg.FixedRows to sg.RowCount - 1 do begin
      OrigUnitName := sg.Cells[0 ,UnitIdx];
      for NsIdx := 0 to NameSpaces.Count - 1 do begin
        s := NameSpaces[NsIdx] + '.' + OrigUnitName;
        SearchPathUnitsIndex := FSearchPathUnits.IndexOf(s);
        if SearchPathUnitsIndex <> -1 then begin
          NewUnitName := FSearchPathUnits[SearchPathUnitsIndex];
          if OrigUnitName <> NewUnitName then begin
            FOldToNewUnitNameMap.Values[OrigUnitName] := NewUnitName;
            sg.Cells[0, UnitIdx] := NewUnitName;
            sg.Objects[0, UnitIdx] := Pointer(1);
          end;
          Break;
        end;
      end;
    end;
  end;

var
  CurrentUnitName: string;
  p: Integer;
begin
  if ShowGxMessageBox(TShowAddDotsMessage) <> mrYes then
    Exit;

  NameSpaces := TStringList.Create;
  try
    GxOtaGetProjectNamespaces(DefaultNamespace, NameSpaces);
    CurrentUnitName := GxOtaGetCurrentSourceFile;
    if CurrentUnitName <> '' then begin
      // remove .pas
      CurrentUnitName := ChangeFileExt(ExtractFilename(CurrentUnitName), '');
      p := Pos('.', CurrentUnitName);
      if p > 0 then begin
        CurrentUnitName := Copy(CurrentUnitName, 1, p - 1);
        NameSpaces.Insert(0, CurrentUnitName);
      end;
      AddDotsTo(sg_Interface);
      AddDotsTo(sg_Implementation);
    end;
  finally
    FreeAndNil(NameSpaces);
  end;
end;

procedure TfmUsesManager.actOKExecute(Sender: TObject);
begin
  SaveChanges;
  ModalResult := mrOk;
end;

type
  TShowRemoveDotsMessage = class(TGxQuestionBoxAdaptor)
  protected
    function GetMessage: string; override;
  end;

{ TShowRemoveDotsMessage }

function TShowRemoveDotsMessage.GetMessage: string;
resourcestring
  SConfirmRemoveDots =
    'This will remove the namespace qualifiers from all unit names ' + sLineBreak
    + 'in both uses clauses.' + sLineBreak
    + sLineBreak
    + 'Example:' + sLineBreak
    + '"System.Win.Registry" will be shortended to "Registry".' + sLineBreak
    + sLineBreak
    + 'This is meant to keep backwards compatibility with older Delphi versions.' + sLineBreak
    + sLineBreak
    + 'Do you want to proceed?';
begin
  Result := SConfirmRemoveDots;
end;

procedure TfmUsesManager.btnRemoveDotsClick(Sender: TObject);

  procedure RemoveDotsfrom(sg: TStringGrid);
  var
    i: Integer;
    OrigUnitName: string;
    NewUnitName: string;
    p: Integer;
  begin
    for i := sg.FixedRows to sg.RowCount - 1 do begin
      OrigUnitName := sg.Cells[0, i];
      NewUnitName := OrigUnitName;
      p := Pos('.', NewUnitName);
      while p > 0 do begin
        NewUnitName := Copy(NewUnitName, p + 1, 255);
        p := Pos('.', NewUnitName);
      end;
      if NewUnitName <> OrigUnitName then begin
        FOldToNewUnitNameMap.Values[OrigUnitName] := NewUnitName;
        sg.Cells[0, i] := NewUnitName;
        sg.Objects[0, i] := pointer(1);
      end;
    end;
  end;

begin
  if ShowGxMessageBox(TShowRemoveDotsMessage) <> mrYes then
    Exit;

  RemoveDotsfrom(sg_Interface);
  RemoveDotsfrom(sg_Implementation);
end;

procedure TfmUsesManager.b_NoMapFileCloseClick(Sender: TObject);
begin
  inherited;
  p_NoMapFile.Visible := False;
end;

procedure TfmUsesManager.SaveChanges;
var
  i: Integer;
  OldUnitName: string;
  NewUnitName: string;
  Units: TStringList;
  NewToOldUnitNameMap: TStringList;
begin
  UnAlias(sg_Interface);
  UnAlias(sg_Implementation);

  NewToOldUnitNameMap := nil;
  Units := TStringList.Create;
  try
    GetInterfaceUnits(Units);
    for i := 0 to Units.Count - 1 do begin
      OldUnitName := Units[i];
      if IndexInStringGrid(sg_Interface, OldUnitName) = -1 then begin
        NewUnitName := FOldToNewUnitNameMap.Values[OldUnitName];
        if (NewUnitName = '') or (IndexInStringGrid(sg_Interface, NewUnitName) = -1) then
          RemoveUnitFromInterface(OldUnitName);
      end;
    end;

    GetImplementationUnits(Units);
    for i := 0 to Units.Count - 1 do begin
      OldUnitName := Units[i];
      if IndexInStringGrid(sg_Implementation, OldUnitName) = -1 then begin
        NewUnitName := FOldToNewUnitNameMap.Values[OldUnitName];
        if (NewUnitName = '') or (IndexInStringGrid(sg_Implementation, NewUnitName) = -1) then
          RemoveUnitFromImplementation(OldUnitName);
      end;
    end;

    NewToOldUnitNameMap := TStringList.Create;
    for i := 0 to FOldToNewUnitNameMap.Count - 1 do begin
      OldUnitName := FOldToNewUnitNameMap.Names[i];
      NewUnitName := FOldToNewUnitNameMap.Values[OldUnitName];
      NewToOldUnitNameMap.Values[NewUnitName] := OldUnitName;
    end;

    for i := sg_Interface.FixedRows to sg_Interface.RowCount - 1 do begin
      NewUnitName := sg_Interface.Cells[0, i];
      if (NewUnitName <> '') and not SameText(NewUnitName, 'System') then begin
        OldUnitName := NewToOldUnitNameMap.Values[NewUnitName];
        if OldUnitName = NewUnitName then
          OldUnitName := '';
        case GetUsesStatus(NewUnitName) of
          usNonExisting: begin
              if OldUnitName = '' then begin
                UseUnitInInterface(NewUnitName);
              end else begin
                case GetUsesStatus(OldUnitName) of
                  usNonExisting: begin
                      UseUnitInInterface(NewUnitName);
                    end;
                  usImplementation: begin
                      RemoveUnitFromImplementation(OldUnitName);
                      UseUnitInInterface(NewUnitName);
                    end;
                  usInterface: begin
                      ReplaceUnitInInterface(OldUnitName, NewUnitName);
                    end;
                end;
              end;
            end;
          usInterface: begin
              // the new unit name is already in the interface uses
              // there is a slim chance that the old one is also used
              if OldUnitName <> '' then begin
                case GetUsesStatus(OldUnitName) of
                  usImplementation:
                    RemoveUnitFromImplementation(OldUnitName);
                  usInterface:
                    RemoveUnitFromInterface(OldUnitName);
                end;
              end;
            end;
          usImplementation: begin
              // also removes it from the implementation uses
              UseUnitInInterface(NewUnitName);
              // the new unit name is now in the interface uses
              // there is a slim chance that the old one is also used
              if OldUnitName <> '' then begin
                case GetUsesStatus(OldUnitName) of
                  usImplementation:
                    RemoveUnitFromImplementation(OldUnitName);
                  usInterface:
                    RemoveUnitFromInterface(OldUnitName);
                end;
              end;
            end;
        end; // case New
      end;
    end; // end for interface

    for i := sg_Implementation.FixedRows to sg_Implementation.RowCount - 1 do begin
      NewUnitName := sg_Implementation.Cells[0, i];
      if (NewUnitName <> '') and not SameText(NewUnitName, 'System') then begin
        OldUnitName := NewToOldUnitNameMap.Values[NewUnitName];
        if OldUnitName = NewUnitName then
          OldUnitName := '';
        case GetUsesStatus(NewUnitName) of
          usNonExisting: begin
              if OldUnitName = '' then begin
                UseUnitInImplementation(NewUnitName);
              end else begin
                case GetUsesStatus(OldUnitName) of
                  usNonExisting: begin
                      UseUnitInImplementation(NewUnitName);
                    end;
                  usInterface: begin
                      RemoveUnitFromInterface(OldUnitName);
                      UseUnitInImplementation(NewUnitName);
                    end;
                  usImplementation: begin
                      ReplaceUnitInImplementation(OldUnitName, NewUnitName);
                    end;
                end;
              end;
            end;
          usInterface: begin
              // also removes it from the interface uses
              UseUnitInImplementation(NewUnitName);
              // the new unit name is now in the implementation uses
              // there is a slim chance that the old one is also used
              if OldUnitName <> '' then begin
                case GetUsesStatus(OldUnitName) of
                  usImplementation:
                    RemoveUnitFromImplementation(OldUnitName);
                  usInterface:
                    RemoveUnitFromInterface(OldUnitName);
                end;
              end;
            end;
          usImplementation: begin
              // the new unit name is already in the implementation uses
              // there is a slim chance that the old one is also used
              if OldUnitName <> '' then begin
                case GetUsesStatus(OldUnitName) of
                  usImplementation:
                    RemoveUnitFromImplementation(OldUnitName);
                  usInterface:
                    RemoveUnitFromInterface(OldUnitName);
                end;
              end;
            end;
        end;
      end;
    end;
  finally
    FreeAndNil(NewToOldUnitNameMap);
    FreeAndNil(Units);
  end;
end;

procedure TfmUsesManager.SaveFavorites;
var
  fn: string;
begin
  // Do not localize.
  fn := ConfigInfo.ConfigPath + 'FavoriteUnits.txt'; // do not localize
  FFavoriteUnits.SaveToFile(fn);
end;

procedure TfmUsesManager.FormShow(Sender: TObject);

procedure SelectInGrid(_sg: TStringGrid; const _Unit: string);
  var
    Idx: Integer;
  begin
    Idx := IndexInStringGrid(_sg, _Unit);
    if Idx <> -1 then
      _sg.row := Idx
    else
      _sg.row := 0;
  end;

var
  s: string;
begin
  FilterVisibleUnits;

  s := edtIdentifierFilter.Text;
  SelectInGrid(sg_Interface, s);
  SelectInGrid(sg_Implementation, s);
end;

procedure TfmUsesManager.actImplMoveExecute(Sender: TObject);
begin
  MoveSelected(sg_Implementation, sg_Interface, True);
end;

procedure TfmUsesManager.actImplAddToFavoritesExecute(Sender: TObject);
begin
  AddListToFavorites(sg_Implementation);
end;

procedure TfmUsesManager.actImplDeleteExecute(Sender: TObject);
begin
  DeleteSelected(sg_Implementation);
end;

procedure TfmUsesManager.actIntfAddToFavoritesExecute(Sender: TObject);
begin
  AddListToFavorites(sg_Interface);
end;

procedure TfmUsesManager.actIntfDeleteExecute(Sender: TObject);
begin
  DeleteSelected(sg_Interface);
end;

procedure TfmUsesManager.actIntfMoveExecute(Sender: TObject);
begin
  MoveSelected(sg_Interface, sg_Implementation, False);
end;

type
  TShowUnaliasMessage = class(TGxQuestionBoxAdaptor)
  protected
    function GetMessage: string; override;
  end;

{ TShowUnaliasMessage }

function TShowUnaliasMessage.GetMessage: string;
resourcestring
  SConfirmUnalias =
    'This will replace aliases with the actual unit ' + sLineBreak
    + 'in both uses clauses.' + sLineBreak
    + sLineBreak
    + 'Example:' + sLineBreak
    + '"dbiTypes" will be replaced by "BDE" '  + sLineBreak
    + '(if default aliases are in effect).' + sLineBreak
    + sLineBreak
    + 'Do you want to proceed?';
begin
  Result := SConfirmUnalias;
end;

procedure TfmUsesManager.actIntfUnAliasExecute(Sender: TObject);

  procedure ReplaceByAlias(sg: TStringGrid; AlsoSearch: TStringList);
  var
    i: Integer;
    s: string;
    p: Integer;
    sl: TStringList;
    FixedRows: Integer;
  begin
    sl := TStringList.Create;
    try
      for i := sg.FixedRows to sg.RowCount - 1  do begin
        s := sg.Cells[0, i];
        p := Pos(ALIAS_PREFIX, s);
        if p > 0 then begin
          p := p + Length(ALIAS_PREFIX);
          s := Copy(s, p, Length(s) - p);
        end;
        // only if the unit is not already in the list or in AlsoSearch, add it
        if (AlsoSearch.IndexOf(s) = -1) and (sl.IndexOf(s) = -1) then
          sl.Add(s);
      end;
      FixedRows := sg.FixedRows;
      for i := 0 to sl.Count - 1 do
        sg.Cells[0, i + FixedRows] := sl[i];
      TGrid_SetNonfixedRowCount(sg, sl.Count);
      if sl.Count = 0 then
        sg.Cells[0, FixedRows] := '';
    finally
      FreeAndNil(sl);
    end;
  end;

var
  IntSl: TStringList;
begin
  if ShowGxMessageBox(TShowUnaliasMessage) <> mrYes then
    Exit;

  IntSl := TStringList.Create;
  try
    // IntSl is empty
    ReplaceByAlias(sg_Interface, IntSl);

    // fill IntSl with the units from interface list
    TStringGrid_GetCol(sg_Interface, 0, IntSl);
    IntSl.Sort;
    ReplaceByAlias(sg_Implementation, IntSl);
  finally
    FreeAndNil(IntSl);
  end;
end;

procedure TfmUsesManager.AddListToFavorites(sg: TStringGrid);
var
  i: Integer;
  col: Integer;
begin
  Assert(Assigned(sg));
  col := sg.ColCount - 1;
  for i := sg.Selection.Top to sg.Selection.Bottom do
    AddToFavorites(sg.Cells[col, i]);
end;

function PAKeyPressed(const Key: Integer): Boolean;
begin
  Result := (GetAsyncKeyState(Key) and $8000 <> 0);
end;

procedure TfmUsesManager.btnCopySaveProjectListClick(Sender: TObject);
begin
  // Save instead of Copy if the CTRL modifier key is pressed:
  if PAKeyPressed(VK_CONTROL) then
    SaveProjectListToDisk
  else
    CopyProjectListToClipboard;
end;

procedure TfmUsesManager.btnCopySaveProjectListMenuClick(Sender: TObject);
var
  Pt: TPoint;
begin
  Pt.X := btnCopySaveCurrentList.Left + 0;
  Pt.Y := btnCopySaveCurrentList.Top + btnCopySaveCurrentList.Height;
  Pt := pnlProjFooter.ClientToScreen(Pt);
  pmCopySaveProjectList.Popup(Pt.X, Pt.Y);
end;

procedure TfmUsesManager.CopyProjectListToClipboard;
begin
  Clipboard.AsText := sg_Project.AssociatedList.Text;
end;

procedure TfmUsesManager.mi_CopyProjectListToClipboardClick(Sender: TObject);
begin
  CopyProjectListToClipboard;
end;

procedure TfmUsesManager.rbMatchAnywareClick(Sender: TObject);
begin
  // Re-filter:
  Forms.Screen.Cursor := crHourGlass;
  try
    FilterIdentifiers;
  finally
    Forms.Screen.Cursor := crDefault;
  end;

  // Todo: Save this setting
end;

procedure TfmUsesManager.rbMatchAnywareEnter(Sender: TObject);
begin
  if FForceFocusToIdentifierFilter then begin
    rbMatchAnyware.Checked := True;
    edtIdentifierFilter.SetFocus;
  end;
end;

procedure TfmUsesManager.rbMatchAtStartClick(Sender: TObject);
begin
  // Re-filter:
  Forms.Screen.Cursor := crHourGlass;
  try
    FilterIdentifiers;
  finally
    Forms.Screen.Cursor := crDefault;
  end;

  // Todo: Save this setting
end;

procedure TfmUsesManager.rbMatchAtStartEnter(Sender: TObject);
begin
  if FForceFocusToIdentifierFilter then begin
    rbMatchAtStart.Checked := True;
    edtIdentifierFilter.SetFocus;
  end;
end;

procedure TfmUsesManager.mi_SaveProjectListClick(Sender: TObject);
begin
  SaveProjectListToDisk;
end;

procedure TfmUsesManager.SaveProjectListToDisk;
begin
  dlgSave.Title := 'Save the List of Project Units';
  dlgSave.InitialDir := GetCurrentDir;
  dlgSave.Filter := 'Text file|*.txt';
  dlgSave.DefaultExt := 'txt';
  dlgSave.FilterIndex := 1;
  if dlgSave.Execute then
  begin
    sg_Project.AssociatedList.SaveToFile(dlgSave.FileName);
  end;
end;

procedure TfmUsesManager.sbUCMDblClick(Sender: TObject);
begin
  if sbUCM.SimpleText <> '' then
  begin
    // Todo: if IsCtrlDown then show and select the file in Windows Explorer
    CopyStatusBarTextToClipboard;
  end;
end;

procedure TfmUsesManager.CopyStatusBarTextToClipboard;
begin
  Clipboard.AsText := sbUCM.SimpleText;
end;

procedure TfmUsesManager.mCopyThisFileToTheClipboardClick(Sender: TObject);
begin
  CopyFileToClipboard(sbUCM.SimpleText);
end;

procedure TfmUsesManager.mCopyThisIdentifierToTheClipboardClick(Sender: TObject);
begin
  Clipboard.AsText := sg_Identifiers.Cells[0, sg_Identifiers.Row];
end;

procedure TfmUsesManager.mCopyThisTextToTheClipboardClick(Sender: TObject);
begin
  CopyStatusBarTextToClipboard;
end;

procedure TfmUsesManager.mShowThisFileInWindowsExplorerClick(Sender: TObject);
begin
  OpenExplorerAndSelectFile(sbUCM.SimpleText);
end;

procedure TfmUsesManager.pmuAvailPopup(Sender: TObject);
// before popup of the units-list context menu
var
  ThisSrc: TStringGrid;
begin
  ThisSrc := GetAvailableSourceList; // sg_*
  Assert(Assigned(ThisSrc));
  mCopyThisIdentifierToTheClipboard.Visible := (ThisSrc.Name = sg_Identifiers.Name);
end;

procedure TfmUsesManager.pmUCMStatusBarPopup(Sender: TObject);
var
  StatusBarFileExists: Boolean;
begin
  if sbUCM.SimpleText = '' then
    Abort;

  StatusBarFileExists := FileExists(sbUCM.SimpleText);
  mCopyThisFileToTheClipboard.Enabled := StatusBarFileExists;
  mShowThisFileInWindowsExplorer.Enabled := StatusBarFileExists;
end;

procedure TfmUsesManager.pm_FavoritePopup(Sender: TObject);
var
  FavoriteListIsEmpty: Boolean;
begin
  // in the Favorite tab, disable irrelevant Favorite list popup menu items if the list is empty:
  if sg_Favorite.RowCount = 1 then
    FavoriteListIsEmpty := (sg_Favorite.Cells[0, 0] = '') // if Favorite list is empty
  else
    FavoriteListIsEmpty := False;

  mi_FavAddToImpl.Enabled := not FavoriteListIsEmpty;
  mi_FavAddtoIntf.Enabled := not FavoriteListIsEmpty;
  mi_FavOpenUnit.Visible := not FavoriteListIsEmpty;
  mi_FavDelUnit.Enabled := not FavoriteListIsEmpty;
end;

procedure TfmUsesManager.sg_CommonSelectCell(Sender: TObject; ACol, ARow: Integer;
  var CanSelect: Boolean);
begin
  ShowSelectedUnitPathInStatusBar(ARow);
end;

procedure TfmUsesManager.sg_FavoriteSelectCell(Sender: TObject; ACol, ARow: Integer;
  var CanSelect: Boolean);
begin
  ShowSelectedUnitPathInStatusBar(ARow);
end;

procedure TfmUsesManager.sg_IdentifiersSelectCell(Sender: TObject; ACol, ARow: Integer;
  var CanSelect: Boolean);
begin
  ShowSelectedUnitPathInStatusBar(ARow);
end;

procedure TfmUsesManager.sg_ProjectSelectCell(Sender: TObject; ACol, ARow: Integer;
  var CanSelect: Boolean);
begin
  ShowSelectedUnitPathInStatusBar(ARow);
end;

procedure TfmUsesManager.sg_SearchPathSelectCell(Sender: TObject; ACol, ARow: Integer;
  var CanSelect: Boolean);
begin
  ShowSelectedUnitPathInStatusBar(ARow);
end;

procedure TfmUsesManager.ShowSelectedUnitPathInStatusBar(const ARow: Integer);
var
  ThisSrc: TStringGrid;
  ThisUnitCol: Integer;
  ThisUnitName: string;
  ThisFullFileName: string;
  Obj: PChar;
  Idx: Integer;
begin
  // Activate this if you are concerned about performance:
  //if not IsCtrlDown then EXIT;
  ThisSrc := GetAvailableSourceList; // sg_*
  Assert(Assigned(ThisSrc));
  // the unit name is always in the rightmost column
  ThisUnitCol := ThisSrc.ColCount - 1;
  ThisUnitName := ThisSrc.Cells[ThisUnitCol, ARow];
  if ThisUnitName = '' then begin
    sbUCM.SimpleText := '';
  end else begin
    if FSearchPathUnits.Find(ThisUnitName, Idx) then
      Obj := PChar(FSearchPathUnits.Objects[Idx])
    else begin
      Idx := -1;
      Obj := nil;
    end;
    if Assigned(Obj) then begin
      ThisFullFileName := Obj;
      sbUCM.SimpleText := ThisFullFileName;
    end else if GxOtaTryFindPathToFile(ThisUnitName + '.pas', ThisFullFileName)
      or GxOtaTryFindPathToFile(ThisUnitName + '.dcu', ThisFullFileName) then begin
      sbUCM.SimpleText := ThisFullFileName;
      if Idx = -1 then begin
        {$IFOPT d+}SendDebugFmt('Unit %s not found in SearchPathUnits', [ThisUnitName]); {$ENDIF}
      end else begin
        {$IFOPT d+}SendDebugFmt('Adding full filename for unit %s', [ThisUnitName]); {$ENDIF}
        Obj := StrNew(PChar(ThisFullFileName));
        FSearchPathUnits.Objects[Idx] := Pointer(Obj);
      end;
    end else
      sbUCM.SimpleText := '';
  end;
end;

{ TStringGrid }

function TStringGrid.GetAssociatedList: TStringList;
begin
  Result := TStringList(Tag);
end;

procedure TStringGrid.SetAssociatedList(const Value: TStringList);
begin
  Tag := Integer(Value);
end;

initialization
  RegisterGX_Expert(TUsesExpert);
end.

