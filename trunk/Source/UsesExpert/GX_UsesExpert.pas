unit GX_UsesExpert;

interface

{$I 'GX_CondDefine.inc'}

{.$DEFINE DEBUG_NOT_FOUND_UNITS}

{$IFOPT D-}
{$UNDEF DEBUG_NOT_FOUND_UNITS}
{$ENDIF}

uses
  Windows, SysUtils,
  Classes, Controls, Forms, Menus, ComCtrls, Buttons, ImgList, ImageList,
  ExtCtrls, ActnList, Actions, Dialogs, StdCtrls, Grids, Types,
  u_dzSpeedBitBtn, u_dzStopwatch,
{$IFDEF IDE_IS_HIDPI_AWARE}
  u_dzDpiScaleUtils,
{$ENDIF}
  GX_ConfigurationInfo, GX_Experts, GX_GenericUtils, GX_BaseForm,
  GX_UnitExportsParser, GX_UsesExpertOptions, GX_UnitExportList,
  GX_OtaUtils;

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
  TUsesClauseMgrExpert = class(TGX_Expert)
  private
    FAvailTabIndex: Integer;
    FReplaceFileUseUnit: Boolean;
    FParseAll: Boolean;
    FDisableCache: Boolean;
    FOrigFileAddUnitExecute: TNotifyEvent;
    FReadMap: Boolean;
    FFilterIdentifiers: TFilterIdentifiersEnum;
    FProjectChangedNotifier: TProjectChangedNotifier;
    FUnitExportParserThread: TUnitExportParserThread;
    FSearchPathFavorites: Boolean;
    FFastAdd: Boolean;
    FIdentifierTabTimer: TStopwatch;
    procedure InternalExecute;
    function FindAction(out _Action: TBasicAction): Boolean;
    procedure HandleProjectChanged(_Sender: TObject);
    procedure StartUnitParserThread;
    procedure GetSearchPathFavorites(_sl: TStrings);
  protected
    procedure InternalLoadSettings(_Settings: IExpertSettings); override;
    procedure InternalSaveSettings(_Settings: IExpertSettings); override;
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
    grp_FavoriteAdd: TGroupBox;
    btnFavoriteAddToInterface: TButton;
    btnFavoriteAddToImplementation: TButton;
    btnFavoriteAddToFavorites: TButton;
    btnFavoriteDeleteFromFavorites: TButton;
    pnlProjFooter: TPanel;
    grp_ProjectAdd: TGroupBox;
    btnProjectAddToInterface: TButton;
    btnProjectAddToImplementation: TButton;
    grp_IdentifiersAdd: TGroupBox;
    pnlCommonFooter: TPanel;
    grp_CommonAdd: TGroupBox;
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
    actAddToImplementationMenu: TAction;
    actAddToInterfaceMenu: TAction;
    actAddToInterfaceBtn: TAction;
    actAddToInterfaceAndClose: TAction;
    actAddToImplementationBtn: TAction;
    actAddToImplemenationAndClose: TAction;
    b_DeleteFromIntf: TButton;
    b_DeleteFromImpl: TButton;
    b_MoveToImpl: TButton;
    b_MoveToIntf: TButton;
    actOpenUnit: TAction;
    tabSearchPath: TTabSheet;
    pnlSearchPathFooter: TPanel;
    grp_SearchPathAdd: TGroupBox;
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
    pnlUnitsCaption: TPanel;
    lblUses: TPanel;
    pnlButtonsRight: TPanel;
    btnCancel: TButton;
    actOK: TAction;
    btnOK: TButton;
    btnOpen: TButton;
    pnlUsesBottom: TPanel;
    btnAddDots: TButton;
    btnRemoveDots: TButton;
    chk_FastAdd: TCheckBox;
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
    grp_IdentifiersMatch: TGroupBox;
    b_IdentifierMatchAnywhere: TBitBtn;
    b_IdentifierMatchStart: TBitBtn;
    b_IdentifierMatchSort: TBitBtn;
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
    mSearchThisOnTheWeb: TMenuItem;
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
    procedure actAddToInterfaceExecute(Sender: TObject);
    procedure actAddToImplementationExecute(Sender: TObject);
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
    procedure mSearchThisOnTheWebClick(Sender: TObject);
    procedure actAddToInterfaceAndCloseExecute(Sender: TObject);
    procedure actAddToImplemenationAndCloseExecute(Sender: TObject);
    procedure chk_FastAddClick(Sender: TObject);
  private
    FLeftRatio: Double;
    FAliases: TStringList;
    FSearchPathThread: TFileFindThread;
    // maintains a list unit name mappings from "originally used" to "currently used"
    // this is necessary to put units which have been switched between using prefixes and
    // not in the correct place of the unit list.
    FOldToNewUnitNameMap: TStringList;
    FCaption_lblFilter: string;
    FForceFocusToIdentifierFilter: Boolean;
    FIdentifierMatchGrp: TdzSpeedBitBtnGroup;
    FSelectedUnitLineNo: Integer;
    FSelectedUnitFn: string;
    FDefaultToInterfaceList: Boolean;
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
    procedure DrawStringGridCell(_sg: TStringGrid; const _Text: string; const _Rect: TRect;
      _State: TGridDrawState; _Focused: Boolean; _Tag: Integer; _StrikeThrough: Boolean);
    procedure SaveProjectListToDisk;
    procedure ShowIdentifiersFilterResult(const cnt: Integer);
    procedure ShowSelectedUnitPathInStatusBar(const ARow: Integer);
    procedure sb_MatchWhereClick(Sender: TObject);
    procedure AssignMenuIcons(_il: TImageList);
    procedure InitializeForm;
    procedure FinalizeForm;
    procedure SetSelectedFile(const _fn: string; _LineNo: Integer = -1);
    procedure UpdateDefaultButton;
  protected
    FProjectUnits: TStringList;
    FCommonUnits: TStringList;
    FFavoriteUnits: TStringList;
    FSearchPathUnits: TStringList;
    FIdentifiers: TUnitExportlist;
    FUsesExpert: TUsesClauseMgrExpert;
{$IFDEF IDE_IS_HIDPI_AWARE}
    FOldDPI: Integer;
    // FImageScaler descends from TComponents and gets freed automatically
    FImageScaler: TImageListScaler;
    procedure ApplyDpi(_NewDpi: Integer; _NewBounds: PRect); override;
{$ENDIF}
  public
    constructor Create(_Owner: TComponent; _UsesExpert: TUsesClauseMgrExpert); reintroduce;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

uses
  CommCtrl, Messages, Graphics, StrUtils, Math, ToolsAPI, Clipbrd, FileCtrl,
  u_dzVclUtils, u_dzMapFileReader, u_dzFileUtils, u_dzOsUtils, u_dzClassUtils, u_dzStringUtils,
  GX_IdeUtils, GX_UsesManager, GX_GetIdeVersion,
{$IFOPT D+}
  GX_DbugIntf,
{$ENDIF D+}
  GX_MessageBox, GX_IdeSearchPathEnhancer, u_dzTypes, u_dzStringArrayUtils,
  GX_StringGridDrawFix;

{ TUsesClauseMgrExpert }

constructor TUsesClauseMgrExpert.Create;
begin
  inherited;
  LoadSettings;
end;

destructor TUsesClauseMgrExpert.Destroy;
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

procedure TUsesClauseMgrExpert.AfterIDEInitialized;
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

function TUsesClauseMgrExpert.FindAction(out _Action: TBasicAction): Boolean;
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

procedure TUsesClauseMgrExpert.Execute(Sender: TObject);
begin
  InternalExecute;
end;

function TUsesClauseMgrExpert.GetDefaultShortCut: TShortCut;
begin
  Result := scShift + scAlt + Ord('U');
end;

function TUsesClauseMgrExpert.GetActionCaption: string;
resourcestring
  SUsesExpert = '&Uses Clause Manager ...';
begin
  Result := SUsesExpert;
end;

//procedure TUsesClauseMgrExpert.GetHelpString(List: TStrings);
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

class function TUsesClauseMgrExpert.GetName: string;
begin
  Result := 'UsesClauseMgr';
end;

procedure TUsesClauseMgrExpert.HandleProjectChanged(_Sender: TObject);
begin
  StartUnitParserThread;
end;

procedure TUsesClauseMgrExpert.GetSearchPathFavorites(_sl: TStrings);
var
  SearchPathFavorites: TStringList;
  i: Integer;
begin
  if not FSearchPathFavorites then begin
    _sl.Clear;
  end else begin
    SearchPathFavorites := TStringList.Create;
    try
      TGxIdeSearchPathEnhancer.GetSearchPathFavorites(SearchPathFavorites);
      for i := 0 to SearchPathFavorites.Count - 1 do begin
        TStrings_AppendStringArray(_sl, SplitString(Trim(TailStrOf(SearchPathFavorites[i], '=')), ';'));
      end;
      for i := 0 to _sl.Count - 1 do begin
        _sl[i] := AddSlash(_sl[i]);
      end;
    finally
      FreeAndNil(SearchPathFavorites);
    end;
  end;
end;

procedure TUsesClauseMgrExpert.StartUnitParserThread;
var
  Paths: TStrings;
  Symbols: TStrings;
  CacheDir: string;
  FavoriteUnits: TStringList;
  fn: string;
  sl: TStringList;
{$IFDEF GX_DELPHIXE2_UP}
  PlatformName: string;
{$ENDIF}
begin
  FreeAndNil(FUnitExportParserThread);

  Symbols := nil;
  Paths := TStringList.Create;
  try
    Symbols := TStringList.Create;
    TUnitExportsParser.AddDefaultSymbols(Symbols);

  // since these symbols are project and configuration specific, we cannot really use them
  // to generate a generic cache of symbols.
  // The cache must also be project and configuration specific which would make it rather
  // cumbersome.
    // source:
    // http://docwiki.embarcadero.com/RADStudio/Sydney/en/Conditional_compilation_(Delphi)
{$IFDEF GX_DELPHIXE2_UP}
    PlatformName := GxOtaGetProjectPlatform;
    if SameText(PlatformName, cWin64Platform) then begin
      CacheDir := PlatformName;
      Symbols.Add('ASSEMBLER');
      Symbols.Add('CPU64BITS');
      Symbols.Add('CPUX64');
      Symbols.Add('MSWINDOWS');
      Symbols.Add('WIN64');
    end else if SameText(PlatformName, cOSX32Platform) then begin
      CacheDir := PlatformName;
      Symbols.Add('MACOS');
      Symbols.Add('MACOS32');
      Symbols.Add('POSIX');
      Symbols.Add('POSIX32');
      Symbols.Add('CPU386');
      Symbols.Add('CPUX86');
      Symbols.Add('CPU32BITS');
      Symbols.Add('ALIGN_STACK');
      Symbols.Add('ASSEMBLER');
      Symbols.Add('PC_MAPPED_EXCEPTIONS');
      Symbols.Add('PIC');
      Symbols.Add('UNDERSCOREIMPORTNAME');
    end else if SameText(PlatformName, cOSX64Platform) then begin
      CacheDir := PlatformName;
      Symbols.Add('CPU64BITS');
      Symbols.Add('CPU386');
      Symbols.Add('CPUX64');
      Symbols.Add('EXTERNALLINKER');
      Symbols.Add('MACOS');
      Symbols.Add('MACOS64');
      Symbols.Add('PIC');
      Symbols.Add('POSIX');
      Symbols.Add('POSIX64');
      Symbols.Add('WEAKINTFREF');
      Symbols.Add('WEAKREF');
{$IFNDEF GX_DELPHI_TOKYO_UP}
      Symbols.Add('AUTOREFCOUNT'); // only up to 10.3
      Symbols.Add('NEXTGEN'); // only up to 10.3
      Symbols.Add('WEAKINSTREF'); // only up to 10.3
{$ENDIF}
    end else if SameText(PlatformName, ciOSDevice32Platform) then begin
      CacheDir := PlatformName;
      Symbols.Add('CPU32BITS');
      Symbols.Add('CPUARM');
      Symbols.Add('CPUARM32');
      Symbols.Add('EXTERNALLINKER');
      Symbols.Add('IOS');
      Symbols.Add('IOS32');
      Symbols.Add('MACOS');
      Symbols.Add('MACOS32');
      Symbols.Add('PIC');
      Symbols.Add('POSIX');
      Symbols.Add('POSIX32');
      Symbols.Add('WEAKINTFREF');
      Symbols.Add('WEAKREF');
{$IFNDEF GX_DELPHI_TOKYO_UP}
      Symbols.Add('AUTOREFCOUNT'); // only up to 10.3
      Symbols.Add('NEXTGEN'); // only up to 10.3
      Symbols.Add('WEAKINSTREF'); // only up to 10.3
{$ENDIF}
    end else if SameText(PlatformName, ciOSSimulatorPlatform) then begin
      CacheDir := PlatformName;
      Symbols.Add('ALIGN_STACK');
      Symbols.Add('ASSEMBLER');
      Symbols.Add('CPU32BITS');
      Symbols.Add('CPU386');
      Symbols.Add('CPUX86');
      Symbols.Add('IOS');
      Symbols.Add('IOS32');
      Symbols.Add('MACOS');
      Symbols.Add('MACOS32');
      Symbols.Add('PIC');
      Symbols.Add('POSIX');
      Symbols.Add('POSIX32');
      Symbols.Add('UNDERSCOREIMPORTNAME');
      Symbols.Add('WEAKINTFREF');
      Symbols.Add('WEAKREF');
{$IFNDEF GX_DELPHI_TOKYO_UP}
      Symbols.Add('AUTOREFCOUNT'); // only up to 10.3
      Symbols.Add('NEXTGEN'); // only up to 10.3
      Symbols.Add('WEAKINSTREF'); // only up to 10.3
{$ENDIF}
    end else if SameText(PlatformName, cAndroidPlatform) then begin
      CacheDir := PlatformName;
      Symbols.Add('ANDROID');
      Symbols.Add('ANDROID32');
      Symbols.Add('CPU32BITS');
      Symbols.Add('CPUARM');
      Symbols.Add('CPUARM32');
      Symbols.Add('EXTERNALLINKER');
      Symbols.Add('PIC');
      Symbols.Add('POSIX');
      Symbols.Add('POSIX32');
      Symbols.Add('WEAKINTFREF');
      Symbols.Add('WEAKREF');
{$IFNDEF GX_DELPHI_TOKYO_UP}
      Symbols.Add('AUTOREFCOUNT'); // only up to 10.3
      Symbols.Add('NEXTGEN'); // only up to 10.3
      Symbols.Add('WEAKINSTREF'); // only up to 10.3
{$ENDIF}
    end else if SameText(PlatformName, ciOSDevice64Platform) then begin
      CacheDir := PlatformName;
      Symbols.Add('CPU64BITS');
      Symbols.Add('CPUARM');
      Symbols.Add('CPUARM64');
      Symbols.Add('EXTERNALLINKER');
      Symbols.Add('IOS');
      Symbols.Add('IOS64');
      Symbols.Add('MACOS');
      Symbols.Add('MACOS64');
      Symbols.Add('PIC');
      Symbols.Add('POSIX');
      Symbols.Add('POSIX64');
      Symbols.Add('WEAKINTFREF');
      Symbols.Add('WEAKREF');
{$IFNDEF GX_DELPHI_TOKYO_UP}
      Symbols.Add('AUTOREFCOUNT'); // only up to 10.3
      Symbols.Add('NEXTGEN'); // only up to 10.3
      Symbols.Add('WEAKINSTREF'); // only up to 10.3
{$ENDIF}
    end else if SameText(PlatformName, cLinux64Platform) then begin
      CacheDir := PlatformName;
      Symbols.Add('LINUX');
      Symbols.Add('LINUX64');
      Symbols.Add('POSIX');
      Symbols.Add('POSIX64');
      Symbols.Add('ASSEMBLER');
      Symbols.Add('CPU64BITS');
      Symbols.Add('CPUX64');
      Symbols.Add('EXTERNALLINKER');
      Symbols.Add('ELF');
      Symbols.Add('PIC');
      Symbols.Add('WEAKINTFREF');
      Symbols.Add('WEAKREF');
{$IFDEF GX_DELPHI_SYDNEY_UP}
      Symbols.Add('AUTOREFCOUNT'); // only in 10.2
      Symbols.Add('NEXTGEN'); // only up to 10.2
      Symbols.Add('WEAKINSTREF'); // only up to 10.
{$ENDIF}
    end else if SameText(PlatformName, cAndroid64Platform) then begin
      CacheDir := PlatformName;
      Symbols.Add('POSIX');
      Symbols.Add('POSIX64');
      Symbols.Add('ANDROID');
      Symbols.Add('ANDROID64');
      Symbols.Add('CPU64BITS');
      Symbols.Add('CPUARM');
      Symbols.Add('CPUARM64');
      Symbols.Add('EXTERNALLINKER');
      Symbols.Add('PIC');
      Symbols.Add('WEAKINTFREF');
      Symbols.Add('WEAKREF');
    end else if SameText(PlatformName, cWin32Platform) or (PlatformName = '') then
{$ENDIF}begin
      // CONSOLE?
      CacheDir := '';
      Symbols.Add('ASSEMBLER');
      Symbols.Add('CPU32BITS');
      Symbols.Add('CPU386');
      Symbols.Add('CPUX86');
      Symbols.Add('DCC');
      Symbols.Add('MSWINDOWS');
      Symbols.Add('UNDERSCOREIMPORTNAME');
      Symbols.Add('WIN32');
    end;

    if FDisableCache then
      CacheDir := ''
    else
      CacheDir := ConfigInfo.CachingPath + 'UsesExpertCache\' + CacheDir;

  // We only take identifiers from units listed in the search path, so one could be tempted
  // to only parse these units. But the effective library path does not contain the browsing
  // path for units for which only dcu files are available in the library path.
//    GxOtaGetEffectiveLibraryPath(Paths);
    sl := TStringList.Create;
    try
      GetSearchPathFavorites(sl);
      GxOtaGetAllPossiblePaths(Paths, nil, sl);
    finally
      FreeAndNil(sl);
    end;

    if FParseAll then begin
{$IFOPT D+}
      SendDebug('Running UnitExportParser thread to get identifiers from all units in search path');
{$ENDIF D+}
      FUnitExportParserThread := TUnitExportParserThread.Create(nil, Paths, CacheDir, Symbols);
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
        FUnitExportParserThread := TUnitExportParserThread.Create(FavoriteUnits, Paths, CacheDir, Symbols);
      end;
    end;
  finally
    FreeAndNil(Symbols);
    FreeAndNil(Paths);
  end;
end;

function TUsesClauseMgrExpert.HasConfigOptions: Boolean;
begin
  Result := True;
end;

procedure TUsesClauseMgrExpert.Configure;
var
  act: TBasicAction;
  Found: Boolean;
  CacheDir: string;
begin
  Found := FindAction(act);
  CacheDir := ConfigInfo.CachingPath + 'UsesExpertCache';

  if TfmUsesExpertOptions.Execute(Application, Found, CacheDir, FReadMap, FReplaceFileUseUnit,
    FParseAll, FDisableCache, FSearchPathFavorites, FFilterIdentifiers, FFastAdd) then begin
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

procedure TUsesClauseMgrExpert.InternalExecute;
var
  frm: TfmUsesManager;
begin
  FIdentifierTabTimer := TStopwatch_StartNew;

  AssertIsPasOrInc(GxOtaGetCurrentSourceFile);

  frm := TfmUsesManager.Create(Application, Self);
  try
    if (FAvailTabIndex >= 0) and (FAvailTabIndex < frm.pcUnits.PageCount) then begin
      frm.pcUnits.ActivePageIndex := FAvailTabIndex;
      frm.pcUnits.Change;
    end;

    if frm.ShowModal = mrOk then
    begin
      FAvailTabIndex := frm.pcUnits.ActivePageIndex;

      IncCallCount;
    end;
  finally
    FreeAndNil(frm);
  end;
end;

procedure TUsesClauseMgrExpert.InternalLoadSettings(_Settings: IExpertSettings);
begin
  inherited;
  FReplaceFileUseUnit := _Settings.ReadBool('ReplaceFileUseUnit', False);
  FReadMap := _Settings.ReadBool('ReadMap', True);
  FAvailTabIndex := _Settings.ReadInteger('AvailTabIndex', 0);
  FParseAll := _Settings.ReadBool('ParseAll', True);
  FDisableCache := _Settings.ReadBool('DisableCache', False);
  FSearchPathFavorites := _Settings.ReadBool('SearchPathFavorites', False);
  FFastAdd := _Settings.ReadBool('FastAdd', False);
end;

procedure TUsesClauseMgrExpert.InternalSaveSettings(_Settings: IExpertSettings);
begin
  inherited;
  _Settings.WriteBool('ReplaceFileUseUnit', FReplaceFileUseUnit);
  _Settings.WriteBool('ReadMap', FReadMap);
  _Settings.WriteInteger('AvailTabIndex', FAvailTabIndex);
  _Settings.WriteBool('ParseAll', FParseAll);
  _Settings.WriteBool('DisableCache', FDisableCache);
  _Settings.WriteBool('SearchPathFavorites', FSearchPathFavorites);
  _Settings.WriteBool('FastAdd', FFastAdd);
end;

{ TfmUsesManager }

constructor TfmUsesManager.Create(_Owner: TComponent; _UsesExpert: TUsesClauseMgrExpert);
var
  StopWatch: TStopwatch;
  il: TImageList;
begin
{$IFOPT D+}
  SendDebug('TfmUsesManager.Create Enter');
{$ENDIF D+}
  FUsesExpert := _UsesExpert;

  StopWatch := TStopwatch_StartNew;
  // To ensure we get the latest version of all units, start the thread again
  // it will run fast if everything is already cached.
  FUsesExpert.StartUnitParserThread;

  TStopWatch_Stop(StopWatch);
{$IFOPT D+}
  SendDebugFmt('Starting Unit Parser Thread took %d milliseconds', [TStopWatch_ElapsedMilliseconds(StopWatch)]);
{$ENDIF D+}

  inherited Create(_Owner);

  DoubleBuffered := True;
  pnlUnits.DoubleBuffered := True;
  pnlUses.DoubleBuffered := True;

  FIdentifierMatchGrp :=  TdzSpeedBitBtnGroup.Create;
  FIdentifierMatchGrp.AllowAllUp := False;
  FIdentifierMatchGrp.Add(b_IdentifierMatchStart, Ord(fieStartOnly));
  FIdentifierMatchGrp.Add(b_IdentifierMatchAnywhere, Ord(fieAnywhere));
  FIdentifierMatchGrp.Add(b_IdentifierMatchSort, Ord(fieStartFirst));
  // this only works if the order the buttons are added is the same as the order of the enums
  FIdentifierMatchGrp.SetDown(Ord(FUsesExpert.FFilterIdentifiers));
  FIdentifierMatchGrp.OnClick := sb_MatchWhereClick;

  chk_FastAdd.Checked := _UsesExpert.FFastAdd;

  FLeftRatio := pnlUses.Width / ClientWidth;

  FUsesExpert.SetFormIcon(Self);

  sg_Identifiers.Cells[0, 0] := 'Identifier';
  sg_Identifiers.Cells[1, 0] := 'Unit';

  TControl_SetMinConstraints(Self);
  pnlUses.Constraints.MinWidth := pnlUses.Width;

  // The image list assignments tend to get lost in the designer, so we assign them in code
  il := il_MenuIcons;
  AssignMenuIcons(il);

  InitDpiScaler;

  // call the code that previously was in the FormCreate event
  InitializeForm;

{$IFOPT D+}
  SendDebug('TfmUsesManager.Create Leave');
{$ENDIF D+}
end;

destructor TfmUsesManager.Destroy;
begin
  // call the code that previously was in the FormDestroy event
  FinalizeForm;

  inherited;
end;

procedure TfmUsesManager.AssignMenuIcons(_il: TImageList);
begin
  ActionList.Images := _il;
  pmuAvail.Images := _il;
  pmCopySaveProjectList.Images := _il;
  pmUCMStatusBar.Images := _il;
  pm_Favorite.Images := _il;
  pm_Intf.Images := _il;
  pm_Impl.Images := _il;
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
  FProjectUnits.BeginUpdate;
  try
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
  finally
    FProjectUnits.EndUpdate;
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

// this used to be the FormCreate event
procedure TfmUsesManager.InitializeForm;

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
  sl: TStringList;
begin
  FCaption_lblFilter := lblFilter.Caption; // for showing Filter results
  lblFilter.Tag := -1; // Initialize

  FProjectUnits := TStringList.Create;
  sg_Project.AssociatedList := FProjectUnits;

  FCommonUnits := TStringList.Create;
  sg_Common.AssociatedList := FCommonUnits;

  FFavoriteUnits := TStringList.Create;
  sg_Favorite.AssociatedList := FFavoriteUnits;

  FSearchPathUnits := TStringList.Create;
  sg_SearchPath.AssociatedList := FSearchPathUnits;

  FIdentifiers := TUnitExportlist.Create(0);
  FAliases := TStringList.Create;
  FOldToNewUnitNameMap := TStringList.Create;

  LoadFavorites;

  tim_Progress.Enabled := True;

  FSearchPathThread := TFileFindThread.Create;
  FSearchPathThread.FileMasks.Add('*.pas');
  FSearchPathThread.FileMasks.Add('*.dcu');
  sl := TStringList.Create;
  try
    FUsesExpert.GetSearchPathFavorites( sl);
    GxOtaGetEffectiveLibraryPath(FSearchPathThread.SearchDirs, nil, True, sl);
  finally
    FreeAndNil(sl);
  end;
  FSearchPathThread.OnFindComplete := SearchPathReady;
  FSearchPathThread.StartFind;
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

// this used to be the FormDestroy event
procedure TfmUsesManager.FinalizeForm;
begin
  tim_Progress.Enabled := False;

  if Assigned(FSearchPathThread) then begin
    FSearchPathThread.OnFindComplete := nil;
    FSearchPathThread.Terminate;
  end;

  FreeAndNil(FOldToNewUnitNameMap);
  FreeAndNil(FAliases);
  FreeAndNil(FSearchPathThread);
  FreeAndNil(FProjectUnits);
  FreeAndNil(FCommonUnits);
  FreeAndNil(FFavoriteUnits);

  FreePChars(FSearchPathUnits);
  FreeAndNil(FSearchPathUnits);

  FreeAndNil(FIdentifiers);
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
  SetSelectedFile('', -1);

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
  UpdateDefaultButton;
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
    FDefaultToInterfaceList :=
      UsesManager.IsPositionBeforeImplementation(GxOtaGetCurrentEditBufferPos);
    if FDefaultToInterfaceList then begin
      chk_FastAdd.Caption := 'Add fast (to Interface)';
      chk_FastAdd.Hint := 'Enter adds to Interface and closes dialog';
    end else begin
      chk_FastAdd.Caption := 'Add fast (to Implementation)';
      chk_FastAdd.Hint := 'Enter adds to Implementation and closes dialog';
    end;
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
  _State: TGridDrawState; _Focused: Boolean; _Tag: Integer; _StrikeThrough: Boolean);
var
  cnv: TCanvas;
begin
  cnv := _sg.Canvas;
  if _Text = '' then
    cnv.Brush.Color := _sg.Color
  else begin
    if _StrikeThrough then begin
        cnv.Font.Style := [fsStrikeOut];
    end;
    if gdSelected in _State then begin
      if not _Focused then begin
        cnv.Brush.Color := clDkGray;
        // I would have used clHighlightText but that becomes unreadable when theming is active
        cnv.Font.Color := clWhite;
      end;
    end else begin
      if _Tag <> 0 then begin
        cnv.Brush.Color := clYellow;
      end;
    end;
  end;
  TStringGrid_DrawCellFixed(_sg, _Text, _Rect, _State, _focused);
end;

procedure TfmUsesManager.sg_UsedDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect;
  State: TGridDrawState);
var
  sg: TStringGrid absolute Sender;
begin
  DrawStringGridCell(sg, sg.Cells[ACol, ARow], Rect, State, sg.Focused, Integer(sg.Objects[ACol, ARow]), False);
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
  HasSelectedItem := HaveSelectedItem(sg_Interface);
  actIntfMove.Enabled := HasSelectedItem;
  actIntfDelete.Enabled := HasSelectedItem;
  actIntfAddToFavorites.Enabled := HasSelectedItem;

  HasSelectedItem := HaveSelectedItem(sg_Implementation);
  actImplMove.Enabled := HasSelectedItem;
  actImplDelete.Enabled := HasSelectedItem;
  actImplAddToFavorites.Enabled := HasSelectedItem;

  AvailableSourceList := GetAvailableSourceList;
  ActiveLBHasSelection := HaveSelectedItem(AvailableSourceList);

  actAddToImplementationBtn.Enabled := ActiveLBHasSelection;
  actAddToImplementationMenu.Enabled := ActiveLBHasSelection;
  actAddToInterfaceBtn.Enabled := ActiveLBHasSelection;
  actAddToInterfaceMenu.Enabled := ActiveLBHasSelection;
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

{$IFDEF IDE_IS_HIDPI_AWARE}
procedure TfmUsesManager.ApplyDpi(_NewDpi: Integer; _NewBounds: PRect); 

  procedure AdjustTop(_ctrl: TControl);
  begin
    // todo: Is this still necessary?
    _Ctrl.Top := MulDiv(_Ctrl.Top, _NewDPI, FOldDPI);
  end;

  procedure AdjustHeight(_ctrl: TControl);
  begin
    // todo: Is this still necessary?
    _Ctrl.Height := MulDiv(_Ctrl.Height, _NewDPI, FOldDPI);
  end;

  procedure ArrangeButtonsInGroup(const _Buttons: array of TWinControl; _grp: TGroupBox);
  var
    i: Integer;
    Offset: Integer;
    btn: TWinControl;
  begin
    Assert(Length(_Buttons) > 1);
    btn := _Buttons[0];
    Offset := _grp.ClientHeight - btn.Top - btn.Height;
    for i := 0 to Length(_Buttons) - 1 do
      AdjustTop(_Buttons[i]);
    _grp.ClientHeight := btn.Top + btn.Height + Offset;
  end;

  procedure ArrangeAddControls(_btnAddIntf, _btnAddImpl: TButton; _grp: TGroupBox; _pnl: TPanel);
  begin
    ArrangeButtonsInGroup([_btnAddIntf, _btnAddImpl], _grp);
    _pnl.ClientHeight := _grp.Height;
  end;

var
  il: TImageList;
begin
  if FOldDPI = 0 then
    FOldDPI := TForm_CurrentPPI(Self);
  if Assigned(FScaler) then
    FScaler.ApplyDpi(_NewDpi, _NewBounds);

  if not Assigned(FImageScaler) then
    FImageScaler := TImageListScaler.Create(Self, il_MenuIcons);
  il := FImageScaler.GetScaledList(_NewDpi);
  AssignMenuIcons(il);

  TStringGrid_AdjustRowHeight(sg_Interface);
  TStringGrid_AdjustRowHeight(sg_Implementation);
  TStringGrid_AdjustRowHeight(sg_SearchPath);
  TStringGrid_AdjustRowHeight(sg_Project);
  TStringGrid_AdjustRowHeight(sg_Common);
  TStringGrid_AdjustRowHeight(sg_Favorite);
  TStringGrid_AdjustRowHeight(sg_Identifiers);

  lblFilter.Top := 0;
  pnlUnitsCaption.ClientHeight := lblFilter.Height+1;
  edtUnitFilter.Top := pnlUnitsCaption.Height;
  edtIdentifierFilter.Top := edtUnitFilter.Top;
  pnlAvailableHeader.ClientHeight := edtUnitFilter.Top + edtUnitFilter.Height;

  ArrangeAddControls(btnSearchPathAddToIntf, btnSearchPathAddToImpl, grp_SearchPathAdd, pnlSearchPathFooter);
  AdjustTop(btnAddSearchPathlToFavorites);

  ArrangeAddControls(btnProjectAddToInterface, btnProjectAddToImplementation, grp_ProjectAdd, pnlProjFooter);
  AdjustTop(btnAddProjectToFavorites);
  AdjustTop(btnCopySaveCurrentList);
  AdjustTop(btnCopySaveProjectListMenu);

  ArrangeAddControls(btnCommonAddToInterface, btnCommonAddToImplementation, grp_CommonAdd, pnlCommonFooter);
  AdjustTop(btnAddRtlToFavorites);

  ArrangeAddControls(btnFavoriteAddToInterface, btnFavoriteAddToImplementation, grp_FavoriteAdd, pnlFavFooter);
  AdjustTop(btnFavoriteAddToFavorites);
  AdjustTop(btnFavoriteDeleteFromFavorites);

  ArrangeAddControls(btnIdentifiersAddToIntf, btnIdentifiersAddToImpl, grp_IdentifiersAdd, pnlIdentifiersFooter);
//  AdjustTop(b_IdentifierMatchStart);
//  AdjustTop(b_IdentifierMatchAnywhere);
//  AdjustTop(b_IdentifierMatchSort);
  ArrangeButtonsInGroup([b_IdentifierMatchStart, b_IdentifierMatchAnywhere, b_IdentifierMatchSort], grp_IdentifiersMatch);

  // the status bar gets moved up above the lower panel for whatever reason
  // so we need to move it back where it belongs
  sbUCM.Top := Self.Height;

  FOldDpi := _NewDpi;
end;
{$ENDIF}

function TfmUsesManager.HaveSelectedItem(sg: TStringGrid): Boolean;
begin
  Result := False;
  if not Assigned(sg) then
    Exit; //==>

  Result := sg.Selection.Bottom - sg.Selection.Top >= 0;
  if Result then
    Result := (sg.Cells[0, sg.Selection.Top] <> '');
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

procedure TfmUsesManager.actAddToInterfaceAndCloseExecute(Sender: TObject);
begin
  if actAddToInterfaceBtn.Enabled then begin
    actAddToInterfaceBtn.Execute;
    actOK.Execute;
  end;
end;

procedure TfmUsesManager.actAddToImplemenationAndCloseExecute(Sender: TObject);
begin
  if actAddToImplementationBtn.Enabled then begin
    actAddToImplementationBtn.Execute;
    actOK.Execute;
  end;
end;

procedure TfmUsesManager.actAddToInterfaceExecute(Sender: TObject);
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
  if chk_FastAdd.Checked then
    actOK.Execute;
end;

procedure TfmUsesManager.actAddToImplementationExecute(Sender: TObject);
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
  if chk_FastAdd.Checked then
    actOK.Execute;
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
  IsAlreadyUsed: Boolean;
  CellText: string;
begin
  CellText := sg.Cells[ACol, ARow];
  if ACol = sg.ColCount - 1 then begin
    IsAlreadyUsed := SameText(CellText, 'System') or SameText(CellText, 'SysInit')
      or (IndexInStringGrid(sg_Interface, CellText) <> -1)
      or (not FDefaultToInterfaceList and (IndexInStringGrid(sg_Implementation, CellText) <> -1));
  end else
    IsAlreadyUsed := False;

  GridFocused := sg.Focused or edtUnitFilter.Focused or edtIdentifierFilter.Focused;

  DrawStringGridCell(sg, CellText, Rect, State, GridFocused, 0, IsAlreadyUsed);
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
  Obj: TObject;
  Item: TUnitExport;
begin
  Assert(Assigned(List));

  col := List.ColCount - 1;
  if List = sg_Identifiers then begin
    Obj := List.Objects[col, List.Row];
    if Assigned(Obj) then begin
      Item := Obj as TUnitExport;
      GxOtaGoToFileLine(Item.FileName, Item.LineNo + 1);
      ModalResult := mrCancel;
      Exit; //==>
    end
  end;

  UnitName := List.Cells[col, List.Row];
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
var
  Src: TStringGrid;
  col: Integer;
  UnitName: string;
begin
  Src := GetAvailableSourceList;
  Assert(Assigned(Src));
  col := Src.ColCount - 1;
  UnitName := Src.Cells[col, Src.row];
  if UnitName = '' then
    Exit;
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
    Idx: Integer;
  begin
    if IsDotNet then begin
      // do we really care about dotNET in Delphi any more?
      if IsDCU(FileName) then
        Exit; //==>
    end else if IsDCUIL(FileName) then
      Exit; //==>
    UnitName := ExtractPureFileName(FileName);
    if IsPas(FileName) then begin
      if not PathUnits.Find(UnitName, Idx) then
        PathUnits.AddObject(UnitName, Pointer(StrNew(PChar(FileName))));
    end else begin
      PathUnits.Add(UnitName);
    end;
  end;

begin
  if not Assigned(FSearchPathThread) then begin
    // If it is not assigned, something went wrong in the form's constructor
    // which freed the thread but a synchronise call was still waiting to be executed.
    Exit; //==>
  end;
{$IFOPT D+}
  SendDebug('SearchPath is ready');
{$ENDIF D+}
  IsDotNet := GxOtaCurrentProjectIsDotNet;
  PathFiles := nil;
  PathUnits := TStringList.Create;
  try
    PathFiles := TStringList.Create;
    PathUnits.Sorted := True;
    PathUnits.Duplicates := dupIgnore;
    FSearchPathThread.LockResults;
    try
      PathFiles.Assign(FSearchPathThread.Results);
    finally
      FSearchPathThread.ReleaseResults;
    end;
{$IFOPT D+}
  SendDebugFmt('Found %d files in SearchPath', [PathFiles.Count]);
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
    // todo: This unnecessarily sorts the alreadys sorted list
    //       It's probably not a real performance problem but ...
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

procedure TfmUsesManager.FilterVisibleUnits;

  procedure FilterStringGrid(Filter: TStrings; sg: TStringGrid);
  var
    FilterList: TStrings;
    List: TStrings;
  begin
    FilterList := TStringList.Create;
    try
      List := sg.AssociatedList;
      FilterStringListMatchAnywhere(List, FilterList, Filter, False);
      TStringGrid_AssignCol(sg, 0, FilterList, True);
      TGrid_Resize(sg, [roUseGridWidth, roUseAllRows]);
    finally
      FreeAndNil(FilterList);
    end;
  end;

var
  Filter: TStringList;
begin
  Filter := TStringList.Create;
  try
    Filter.Delimiter := ' ';
    Filter.DelimitedText := Trim(edtUnitFilter.Text);
    TStrings_RemoveEmptyLines(Filter);
    FilterStringGrid(Filter, sg_Favorite);
    FilterStringGrid(Filter, sg_Project);
    FilterStringGrid(Filter, sg_Common);
    FilterStringGrid(Filter, sg_SearchPath);
  finally
    FreeAndNil(Filter);
  end;
  SelectFirstItemInLists;
end;

procedure TfmUsesManager.FilterIdentifiers;
var
  i: Integer;
  Identifier: string;
  UnitName: string;
  Filter: string;
  FixedRows: Integer;
  FilterList: TList;
  FilterType: TFilterIdentifiersEnum;
  cnt: Integer;
  MultiFilter: TStringList;
  Idx: Integer;
  Item: TUnitExport;
begin
{$IFOPT D+}
  SendDebug('TfmUsesManager.FilterIdentifiers Enter');
{$ENDIF D+}
  TWinControl_Lock(pnlIdentifiers);
  Filter := Trim(edtIdentifierFilter.Text);

  if FIdentifierMatchGrp.TryGetSelected(Idx) then
    FilterType := TFilterIdentifiersEnum(Idx)
  else
    FilterType := FUsesExpert.FFilterIdentifiers;

  FilterList := TList.Create;
  try
    MultiFilter := TStringList.Create;
    try
      MultiFilter.Delimiter := ' ';
      MultiFilter.DelimitedText := Filter;
      TStrings_RemoveEmptyLines(MultiFilter);
      if MultiFilter.Count = 0 then
        FIdentifiers.AssignTo(FilterList)
      else begin
        case FilterType of
          fieAnywhere:
            FIdentifiers.SearchAnywhere(MultiFilter, FilterList);
          fieStartFirst:
            FIdentifiers.SearchStartFirst(MultiFilter, FilterList);
        else // fieStartOnly
          FIdentifiers.SearchStart(MultiFilter, FilterList);
        end;
      end;
    finally
      FreeAndNil(MultiFilter);
    end;
    cnt := FilterList.Count;
    if cnt = 0 then
      TStringGrid_Clear(sg_Identifiers)
    else begin
      FixedRows := sg_Identifiers.FixedRows;
      sg_Identifiers.RowCount := FixedRows + cnt;
      for i := 0 to cnt - 1 do begin
        Item := TObject(FilterList[i]) as TUnitExport;
        Identifier := String(Item.Identifier);
        sg_Identifiers.Cells[0, FixedRows + i] := Identifier;
        UnitName := Item.FileName;
        sg_Identifiers.Cells[1, FixedRows + i] := ExtractPureFileName(UnitName);
        sg_Identifiers.Objects[1, FixedRows + i] := Item;
      end;
    end;
    ShowIdentifiersFilterResult(cnt);
  finally
    FreeAndNil(FilterList);
  end;
  ShowSelectedUnitPathInStatusBar(1);
  ResizeIdentiferGrid;
{$IFOPT D+}
  SendDebug('TfmUsesManager.FilterIdentifiers Leave');
{$ENDIF D+}
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
  TGrid_Resize(sg_Identifiers, [roUseGridWidth, roUseFirstRows]);
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
{$IF Declared(SendDebug)}
  SendDebug('Loading favorites');
{$IFEND}
  FFavoriteUnits.Sorted := False;
  FFavoriteUnits.Clear;
  fn := ConfigInfo.ConfigPath + 'FavoriteUnits.txt'; // do not localize
  if FileExists(fn) then
    FFavoriteUnits.LoadFromFile(fn);
  FFavoriteUnits.Sorted := True;
{$IF Declared(SendDebug)}
  SendDebugFmt('Done loading %d favorites', [FFavoriteUnits.Count]);
{$IFEND}
end;

procedure TfmUsesManager.tim_ProgressTimer(Sender: TObject);
{$IFDEF DEBUG_NOT_FOUND_UNITS}
var
  NotFoundUnits: TStringList;
{$ENDIF}

{$IFDEF DEBUG_NOT_FOUND_UNITS}
  procedure SendDebugUnitNotFound(const _Identifier, _UnitName: string);
  var
    Idx: Integer;
  begin
    if not NotFoundUnits.Find(_UnitName, Idx) then begin
      SendDebugFmt('Unit for identifier %s (%s) not found in SearchPathUnits', [_Identifier, _UnitName]);
      NotFoundUnits.Add(_UnitName);
    end;
  end;
{$ENDIF}

var
  upt: TUnitExportParserThread;
{$IFDEF PREPROCESS_UNIT_PATHS}
  IdentIdx: Integer;
  UnitFile: string;
  PCharUnitFile: PChar;
  UnitName: string;
  Identifier: string;
  Idx: Integer;
  Item: TUnitExport;
{$ENDIF}
  StopWatch: TStopwatch;
begin
  upt := FUsesExpert.FUnitExportParserThread;
  if Assigned(upt) and not upt.HasFinished then begin
    lblUnitsFound.Caption := Format('Units found: %d', [upt.FoundUnitsCount]);
    lblUnitsParsed.Caption := Format('Units parsed: %d', [upt.ParsedUnitsCount]);
    lblUnitsLoaded.Caption := Format('Units loaded: %d', [upt.LoadedUnitsCount]);
    lblIdentifiers.Caption := Format('Identifiers found: %d', [upt.Identifiers.Count]);
    pnlIdentifiersProgress.Visible := True;
    Exit; //==>
  end;

  StopWatch := TStopwatch_StartNew;
  tim_Progress.Enabled := False;
  pnlIdentifiersProgress.Visible := False;
  TStopWatch_Stop(StopWatch);
{$IFOPT D+}
  SendDebugFmt('Hiding Panel and stopping timer took %d milliseconds', [TStopWatch_ElapsedMilliseconds(StopWatch)]);
{$ENDIF D+}

  if not Assigned(upt) then
    Exit; //==>

  StopWatch := TStopwatch_StartNew;
  TStringGrid_Clear(sg_Identifiers);
  TStopWatch_Stop(StopWatch);
{$IFOPT D+}
  SendDebugFmt('Clearing string grid took %d milliseconds', [TStopWatch_ElapsedMilliseconds(StopWatch)]);
{$ENDIF D+}

  FreeAndNil(FIdentifiers);
  FIdentifiers := upt.DetachIdentifiers;

{$IFDEF PREPROCESS_UNIT_PATHS}
  SendDebug('Preprocessing unit paths');
  StopWatch := TStopwatch_StartNew;
{$IFDEF DEBUG_NOT_FOUND_UNITS}
  NotFoundUnits := TStringList.Create;
  try
    NotFoundUnits.Sorted := True;
{$ENDIF DEBUG_NOT_FOUND_UNITS}
    for IdentIdx := 0 to FIdentifiers.Count - 1 do begin
      Item := FIdentifiers[IdentIdx];
      Identifier := Item.Identifier;
      UnitFile := Item.FileName;
      UnitName := ExtractPureFileName(UnitFile);
      if FSearchPathUnits.Find(UnitName, Idx) then begin
        UnitName := FSearchPathUnits[Idx];
        PCharUnitFile := PChar(FSearchPathUnits.Objects[Idx]);
        if PCharUnitFile = nil then begin
          PCharUnitFile := StrNew(PChar(UnitFile));
          FSearchPathUnits.Objects[Idx] := Pointer(PCharUnitFile);
        end;
{$IFDEF DEBUG_NOT_FOUND_UNITS}
      end else begin
        SendDebugUnitNotFound(Identifier, UnitName)
{$ENDIF DEBUG_NOT_FOUND_UNITS}
      end;
    end;
{$IFDEF DEBUG_NOT_FOUND_UNITS}
  finally
    FreeAndNil(NotFoundUnits);
  end;
{$ENDIF DEBUG_NOT_FOUND_UNITS}
  TStopWatch_Stop(StopWatch);
  SendDebugFmt('Preprocessing identifiers took %d milliseconds', [TStopWatch_ElapsedMilliseconds(StopWatch)]);
{$ENDIF PREPROCESS_UNIT_PATHS}

{$IFOPT D+}
  SendDebug('Filtering identifiers');
{$ENDIF D+}
  StopWatch := TStopwatch_StartNew;
  FilterIdentifiers;
  TStopWatch_Stop(StopWatch);
{$IFOPT D+}
  SendDebugFmt('Filtering identifiers took %d milliseconds', [TStopWatch_ElapsedMilliseconds(StopWatch)]);
{$ENDIF D+}

  TStopWatch_Stop(FUsesExpert.FIdentifierTabTimer);
{$IFOPT D+}
  SendDebugFmt('Total time from calling the expert until after filtering: %d milliseconds', [TStopWatch_ElapsedMilliseconds(FUsesExpert.FIdentifierTabTimer)]);
{$ENDIF D+}
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
        NewUnitName := Copy(NewUnitName, p + 1);
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

procedure TfmUsesManager.chk_FastAddClick(Sender: TObject);
begin
  UpdateDefaultButton;
end;

procedure TfmUsesManager.UpdateDefaultButton;
begin
  btnSearchPathAddToIntf.Default := False;
  btnSearchPathAddToIntf.Default := False;
  btnProjectAddToInterface.Default := False;
  btnProjectAddToImplementation.Default := False;
  btnCommonAddToInterface.Default := False;
  btnCommonAddToImplementation.Default := False;
  btnFavoriteAddToInterface.Default := False;
  btnFavoriteAddToImplementation.Default := False;
  btnIdentifiersAddToIntf.Default := False;
  btnIdentifiersAddToImpl.Default := False;
  btnOK.Default := False;

  if chk_FastAdd.Checked then begin
    if pcUnits.ActivePage = tabSearchPath then begin
      if FDefaultToInterfaceList then
        btnSearchPathAddToIntf.Default := FDefaultToInterfaceList
      else
        btnSearchPathAddToImpl.Default := True;
    end else if pcUnits.ActivePage = tabProject then begin
      if FDefaultToInterfaceList then
        btnProjectAddToInterface.Default := True
      else
        btnProjectAddToImplementation.Default := True;
    end else if pcUnits.ActivePage = tabCommon then begin
      if FDefaultToInterfaceList then
        btnCommonAddToInterface.Default := True
      else
        btnCommonAddToImplementation.Default := True;
    end else if pcUnits.ActivePage = tabFavorite then begin
      if FDefaultToInterfaceList then
        btnFavoriteAddToInterface.Default := True
      else
        btnFavoriteAddToImplementation.Default := True;
    end else if pcUnits.ActivePage = tabIdentifiers then begin
      if FDefaultToInterfaceList then
        btnIdentifiersAddToIntf.Default := True
      else
        btnIdentifiersAddToImpl.Default := True;
    end else
      btnOK.Default := True;
  end else
    btnOK.Default := True;
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

procedure TfmUsesManager.sb_MatchWhereClick(Sender: TObject);
begin
  TCursor_TemporaryChange();
  FilterIdentifiers;
  FForceFocusToIdentifierFilter := True;
  edtIdentifierFilter.SetFocus;
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
  if FSelectedUnitFn <> '' then
  begin
    // Todo: if IsCtrlDown then show and select the file in Windows Explorer
    CopyStatusBarTextToClipboard;
  end;
end;

procedure TfmUsesManager.CopyStatusBarTextToClipboard;
var
  s: string;
begin
  s := FSelectedUnitFn;
  if FSelectedUnitLineNo <> -1 then
    s := s + ':' + IntToStr(FSelectedUnitLineNo);
  Clipboard.AsText := s;
end;

procedure TfmUsesManager.mCopyThisFileToTheClipboardClick(Sender: TObject);
begin
  CopyFileToClipboard(FSelectedUnitFn);
end;

procedure TfmUsesManager.mCopyThisIdentifierToTheClipboardClick(Sender: TObject);
begin
  Clipboard.AsText := sg_Identifiers.Cells[0, sg_Identifiers.Row];
end;

procedure TfmUsesManager.mCopyThisTextToTheClipboardClick(Sender: TObject);
begin
  CopyStatusBarTextToClipboard;
end;

procedure TfmUsesManager.mSearchThisOnTheWebClick(Sender: TObject);
begin
  // google the identifier name together with the unit name:
  GXShellOpen('https://www.google.com/search?q=' +
      sg_Identifiers.Cells[0, sg_Identifiers.Row] + ' ' +
      sg_Identifiers.Cells[1, sg_Identifiers.Row], False);
end;

procedure TfmUsesManager.mShowThisFileInWindowsExplorerClick(Sender: TObject);
begin
  if FSelectedUnitFn <> '' then
    OpenExplorerAndSelectFile(FSelectedUnitFn);
end;

procedure TfmUsesManager.pmuAvailPopup(Sender: TObject);
// before popup of the units-list context menu
var
  ThisSrc: TStringGrid;
  IsIdentifiersTab: Boolean;
begin
  ThisSrc := GetAvailableSourceList; // sg_*
  IsIdentifiersTab := (ThisSrc = sg_Identifiers);
  mCopyThisIdentifierToTheClipboard.Visible := IsIdentifiersTab;
  mSearchThisOnTheWeb.Visible := IsIdentifiersTab;
end;

procedure TfmUsesManager.pmUCMStatusBarPopup(Sender: TObject);
var
  StatusBarFileExists: Boolean;
begin
  if FSelectedUnitFn = '' then
    Abort;
  StatusBarFileExists := FileExists(FSelectedUnitFn);
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

procedure TStatusBar_SetFilenamePanel(_sb: TStatusBar; _PanelIdx: Integer; const _Text: string);
var
  Borders: array[0..2] of Integer;
  PanelWidth: Integer;
  MaxWidth: Integer;
  txt: string;
  i: Integer;
begin
  // calculate a little indent on both sides of the text (credit @TLama)
  SendMessage(_sb.Handle, SB_GETBORDERS, 0, LPARAM(@Borders));

  _sb.Canvas.Font := _sb.Font;

  // Metermine the maximum allowed width for this panel, based on the width of the status bar
  // and the width of the other panels.
  MaxWidth := _sb.Width;
  for i := 0 to _sb.Panels.Count - 1 do begin
    if i <> _PanelIdx then begin
      MaxWidth := MaxWidth - 2 * Borders[1] - 2 - _sb.Panels[i].Width;
    end;
  end;

  txt := _Text;
  PanelWidth := _sb.Canvas.TextWidth(txt) + 2 * Borders[1] + 2;
  if PanelWidth > MaxWidth then begin
    // if the text is too long, reduce it
    txt := MinimizeName(TFileName(txt), _sb.Canvas, MaxWidth);
    // and recalculate the required width
    PanelWidth := _sb.Canvas.TextWidth(txt) + 2 * Borders[1] + 2;
  end;
  _sb.Panels[_PanelIdx].Text := txt;
  _sb.Panels[_PanelIdx].Width := PanelWidth;
end;

procedure TfmUsesManager.SetSelectedFile(const _fn: string; _LineNo: Integer);
begin
  FSelectedUnitLineNo := _LineNo;
  if _LineNo <> -1 then begin
    sbUCM.Panels[1].Text := IntToStr(_LineNo);
  end else begin
    sbUCM.Panels[1].Text := '';
  end;
  FSelectedUnitFn := _fn;
  if _fn <> '' then begin
    TStatusBar_SetFilenamePanel(sbUCM, 0, _fn);
    sbUCM.Hint := _fn + #13#10
      + 'Double click on this status bar to copy the text displayed on the status bar to the clipboard.'#13#10
      + 'Right click on the status bar to show a popup menu with more options.';
  end else begin
    sbUCM.Panels[0].Text := '';
    sbUCM.Hint := '';
  end;
end;

procedure TfmUsesManager.ShowSelectedUnitPathInStatusBar(const ARow: Integer);
var
  ThisSrc: TStringGrid;
  ThisUnitCol: Integer;
  ThisUnitName: string;
  ThisFullFileName: string;
  ThisUnitNamePtr: PChar;
  Obj: tobject;
  Item: TUnitExport;
  Idx: Integer;
begin
  ThisSrc := GetAvailableSourceList; // sg_*
  Assert(Assigned(ThisSrc));
  // the unit name is always in the rightmost column
  ThisUnitCol := ThisSrc.ColCount - 1;
  if ThisSrc = sg_Identifiers then begin
    Obj := ThisSrc.Objects[ThisUnitCol, ARow];
    if Assigned(Obj) then begin
      Item := obj as TUnitExport;
      SetSelectedFile(Item.Filename, Item.LineNo);
    end;
  end else begin
    ThisUnitName := ThisSrc.Cells[ThisUnitCol, ARow];
    if ThisUnitName = '' then begin
      SetSelectedFile('', -1);
    end else begin
      if FSearchPathUnits.Find(ThisUnitName, Idx) then
        ThisUnitNamePtr := PChar(FSearchPathUnits.Objects[Idx])
      else begin
        Idx := -1;
        ThisUnitNamePtr := nil;
      end;
      if Assigned(ThisUnitNamePtr) then begin
        ThisFullFileName := ThisUnitNamePtr;
        SetSelectedFile(ThisFullFileName, -1);
      end else if GxOtaTryFindPathToFile(ThisUnitName + '.pas', ThisFullFileName)
        or GxOtaTryFindPathToFile(ThisUnitName + '.dcu', ThisFullFileName) then begin
        SetSelectedFile(ThisFullFileName, -1);
        if Idx = -1 then begin
{$IFOPT D+}SendDebugFmt('Unit %s not found in SearchPathUnits', [ThisUnitName]);
{$ENDIF}
        end else begin
{$IFOPT D+}SendDebugFmt('Adding full filename for unit %s', [ThisUnitName]);
{$ENDIF}
          ThisUnitNamePtr := StrNew(PChar(ThisFullFileName));
          FSearchPathUnits.Objects[Idx] := Pointer(ThisUnitNamePtr);
        end;
      end else begin
        SetSelectedFile('', -1);
      end;
    end;
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
  RegisterGX_Expert(TUsesClauseMgrExpert);
end.

