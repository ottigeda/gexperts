unit GX_ClassBrowser;

// This unit is NOT fully compatible with C++Builder (we don't parse C++ code)
// Is there an easy way to add visual display of multiple interface inheritance?

{$I GX_CondDefine.inc}

{$DEFINE DoShowProgressForm}

interface

uses
  Windows,
  Classes, Controls, Buttons, StdCtrls, Forms, Dialogs, ActnList, ToolWin, ToolsAPI,
  Actions, Graphics, Menus, ExtCtrls, ComCtrls, ImgList, UITypes,
  GX_BaseForm, GX_ClassOptions, GX_ClassMgr, GX_ClassParsing, GX_Experts,
  GX_OtaUtils, GX_EnhancedEditor, GX_SharedImages;

type
  TInfoViewMode = (vmList, vmTree);

  TfmClassBrowser = class(TfmBaseForm)
    StatusBar: TStatusBar;
    Splitter1: TSplitter;
    pnlData: TPanel;
    pcMain: TPageControl;
    tshMembers: TTabSheet;
    tshInherit: TTabSheet;
    tshCode: TTabSheet;
    lvInfo: TListView;
    Splitter2: TSplitter;
    MainMenu: TMainMenu;
    mitFile: TMenuItem;
    mitFileAdd: TMenuItem;
    mitFileRemove: TMenuItem;
    mitFileSep1: TMenuItem;
    mitPrint: TMenuItem;
    mitFilePrintClassReport: TMenuItem;
    mitFilePrinterSetup: TMenuItem;
    mitFileSep2: TMenuItem;
    mitFileExit: TMenuItem;
    mitView: TMenuItem;
    mitViewList: TMenuItem;
    mitViewTree: TMenuItem;
    mitOptions: TMenuItem;
    mitOptionsOptions: TMenuItem;
    mitHelp: TMenuItem;
    mitHelpAbout: TMenuItem;
    pmBrowser: TPopupMenu;
    pmiBrowserRefresh: TMenuItem;
    pmiBrowserSep1: TMenuItem;
    pmiBrowserProperties: TMenuItem;
    scInherit: TScrollBox;
    dlgPrinterSetup: TPrinterSetupDialog;
    pmInfo: TPopupMenu;
    mitFilePrintClassHierarchy: TMenuItem;
    pmiBrowserGotoClass: TMenuItem;
    pmiDetailsGoto: TMenuItem;
    mitViewUnitNames: TMenuItem;
    mitHelpHelp: TMenuItem;
    mitHelpSep1: TMenuItem;
    mitHelpContents: TMenuItem;
    mitEdit: TMenuItem;
    mitEditCopy: TMenuItem;
    mitViewSep2: TMenuItem;
    mitViewDetails: TMenuItem;
    pnlMethod: TPanel;
    pnlBrowse: TPanel;
    tvBrowse: TTreeView;
    Actions: TActionList;
    actFileAdd: TAction;
    actFileRemove: TAction;
    actFilePrintClassReport: TAction;
    actFilePrintClassHierarchy: TAction;
    actFilePrinterSetup: TAction;
    actFileExit: TAction;
    actEditCopy: TAction;
    actViewList: TAction;
    actViewTree: TAction;
    actViewUnitNames: TAction;
    actViewClassProperties: TAction;
    mitViewSep1: TMenuItem;
    actOptionsOptions: TAction;
    actHelpHelp: TAction;
    actHelpContents: TAction;
    actHelpAbout: TAction;
    tbBrowse: TToolBar;
    tbnAdd: TToolButton;
    tbnRemove: TToolButton;
    tbnSeparator1: TToolButton;
    tbnRefresh: TToolButton;
    tbnFind: TToolButton;
    edtClassFilter: TEdit;
    mitFileRefresh: TMenuItem;
    mitEditFind: TMenuItem;
    actEditFind: TAction;
    actFileRefresh: TAction;
    tbInfo: TToolBar;
    tbnConstants: TToolButton;
    tbnMethods: TToolButton;
    tbnTypes: TToolButton;
    tbnVariables: TToolButton;
    tbnProperties: TToolButton;
    tbnSeparator2: TToolButton;
    tbnPrivate: TToolButton;
    tbnProtected: TToolButton;
    tbnPublic: TToolButton;
    tbnPublished: TToolButton;
    tbnSeparator3: TToolButton;
    edtMemberFilter: TEdit;
    actEditGotoMember: TAction;
    actEditGotoClass: TAction;
    actEditFindNext: TAction;
    mitEditFindNext: TMenuItem;
    actViewConstants: TAction;
    actViewMethods: TAction;
    actViewTypes: TAction;
    actViewVariables: TAction;
    actViewProperties: TAction;
    actViewPrivate: TAction;
    actViewProtected: TAction;
    actViewPublic: TAction;
    actViewPublished: TAction;
    mitViewSep3: TMenuItem;
    mitViewConstants: TMenuItem;
    mitViewMethods: TMenuItem;
    mitViewTypes: TMenuItem;
    mitViewVariables: TMenuItem;
    mitViewProperties: TMenuItem;
    mitViewSep4: TMenuItem;
    mitViewPrivate: TMenuItem;
    mitViewProtected: TMenuItem;
    mitViewPublic: TMenuItem;
    mitViewPublished: TMenuItem;
    timKeyDelay: TTimer;
    pmiBrowserPrint: TMenuItem;
    pmiBrowserSep2: TMenuItem;
    pmiBrowserPrintClassHierarchy: TMenuItem;
    pmiBrowserPrintClassReport: TMenuItem;
    procedure tvBrowseChange(Sender: TObject; Node: TTreeNode);
    procedure pnlDataResize(Sender: TObject);
    procedure lvInfoChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure pcMainChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure actFilePrintClassReportExecute(Sender: TObject);
    procedure actFilePrintClassHierarchyExecute(Sender: TObject);
    procedure actFileAddExecute(Sender: TObject);
    procedure actFileRemoveExecute(Sender: TObject);
    procedure actFileRefreshExecute(Sender: TObject);
    procedure actEditFindExecute(Sender: TObject);
    procedure actGenericViewNewFilterExecute(Sender: TObject);
    procedure actViewClassPropertiesExecute(Sender: TObject);
    procedure actHelpHelpExecute(Sender: TObject);
    procedure actHelpContentsExecute(Sender: TObject);
    procedure actHelpAboutExecute(Sender: TObject);
    procedure actOptionsOptionsExecute(Sender: TObject);
    procedure actFileExitExecute(Sender: TObject);
    procedure actFilePrinterSetupExecute(Sender: TObject);
    procedure actEditCopyExecute(Sender: TObject);
    procedure actViewListExecute(Sender: TObject);
    procedure actViewTreeExecute(Sender: TObject);
    procedure actViewUnitNamesExecute(Sender: TObject);
    procedure actEditGotoMemberExecute(Sender: TObject);
    procedure actEditGotoClassExecute(Sender: TObject);
    procedure ActionsUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure tvBrowseMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure actEditFindNextExecute(Sender: TObject);
    procedure tvBrowseDblClick(Sender: TObject);
    procedure lvInfoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure edtMemberFilterChange(Sender: TObject);
    procedure edtMemberFilterKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure edtClassFilterKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure edtClassFilterChange(Sender: TObject);
    procedure timKeyDelayTimer(Sender: TObject);
  private
    FProjectNotifier: TBaseIdeNotifier;
    FIsFirstInvocation: Boolean;
    FViewUnitNames: Boolean;
    FInfoViewMode: TInfoViewMode;
    FClassList: TClassList;
    fmProgress: TfmClassParsing;
    FPrimitiveTop: Boolean;
    FStayInPackage: Boolean;
    FParseRecursing: Boolean;
    FAutomaticallyHideBrowser: Boolean;
    FProjectNeedsLoading: Boolean;
    FStartingDir: string;
    FCurrentCodePaneFile: string;
    FClassHierarchyFontSize: Integer;
    FClassHierarchyBoxWidth: Integer;
    FClassHierarchyBoxSpace: Integer;
    FClassHierarchyFont: string;
    FFilters: TClassBrowswerFilters;
    FLastFind: string;
    FLastHitTestItemIdx: Integer;
    FLastHitTestSubItemIdx: Integer;
    FTreeFilter: string;

    procedure EndParse(Sender: TObject);
    procedure ParseFile(Sender: TObject; const FileName: string; FileIndex, FileCount: Integer);
    procedure LoadObjects(Item: TClassItem; ONode: TTreeNode);
    procedure LoadList(OInfo: TBrowseClassInfoCollection);
    procedure LoadClassList(Item: TClassItem; ONode: TTreeNode);
    procedure LoadClassTree(Item: TClassItem; ONode: TTreeNode);
    function GetMethodString(M: TBrowseMethodInfoItem): string;
    function CheckFilter(MInfo: TBrowseMethodInfoItem): Boolean;
    procedure LoadCode;
    procedure GetInheritedList(List: TStrings; const StartClassName: string);
    procedure DrawInheritance;
    procedure DrawResize;
    procedure LoadSettings;
    procedure SaveSettings;
    procedure LoadAllObjects;
    procedure PrintClassReportBuiltIn;
    procedure PrintClassBuiltIn(OInfo: TBrowseClassInfoCollection; ACanvas: TCanvas);
    procedure PrintClassHierarchyBuiltIn;
    procedure PrintClassDiagramBuiltIn(OInfo: TBrowseClassInfoCollection; ACanvas: TCanvas; BoxSize, VSpace: Integer);
    procedure RefreshNode;
    procedure AddProject;
    procedure RemoveProject;
    procedure ClickInheritancePanel(Sender: TObject);
    function FilterTab(const Source: string): string;
    procedure ViewBrowserDetails;
    procedure SetInfoViewMode(const Value: TInfoViewMode);
    procedure FindFromNode(const Text: string; Node: TTreeNode);
    procedure FiltersToActions;
    procedure FileNotification(NotifyCode: TOTAFileNotification; const FileName: string; var Cancel: Boolean);
  private
    FMethodText: TGxEnhancedEditor;
    FCodeText: TGxEnhancedEditor;
    FLastProject: string;
    procedure SetupEditorControls;
    function Images: TImageList;
    procedure UpdateListFilter;
    function TryGetClassInfo(out _Info: TBrowseClassInfoCollection): Boolean;
  protected
{$IFDEF GX_IDE_IS_HIDPI_AWARE}
    procedure ApplyDpi(_NewDpi: Integer; _NewBounds: PRect); override;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ClassList: TClassList read FClassList;
  end;

  TClassBrowserExpert = class(TGX_Expert)
  private
    fmClassBrowser: TfmClassBrowser;
  protected
    procedure SetActive(New: Boolean); override;
  public
    destructor Destroy; override;
    function GetActionCaption: string; override;
    class function GetName: string; override;
    procedure Execute(Sender: TObject); override;
    function HasConfigOptions: Boolean; override;
    function IsDefaultActive: Boolean; override;
  end;

implementation

{$R *.dfm}

uses
  SysUtils, Messages, Printers, CommCtrl, Math,
  u_dzVclUtils,
  GX_VerDepConst, GX_ClassIdentify, GX_ConfigurationInfo,
  GX_ClassProp, GX_GExperts,
  GX_GxUtils, GX_GenericUtils, GX_StringList, GX_IdeUtils;

type
  TClassProjectNotifier = class(TBaseIdeNotifier)
  private
    fmClassBrowser: TfmClassBrowser;
  public
    constructor Create(Form: TfmClassBrowser);
    procedure FileNotification(NotifyCode: TOTAFileNotification;
      const FileName: string; var Cancel: Boolean); override;
  end;

{ TClassProjectNotifier }

constructor TClassProjectNotifier.Create(Form: TfmClassBrowser);
begin
  inherited Create;
  fmClassBrowser := Form;
end;

procedure TClassProjectNotifier.FileNotification(NotifyCode: TOTAFileNotification;
  const FileName: string; var Cancel: Boolean);
begin
  fmClassBrowser.FileNotification(NotifyCode,FileName, Cancel);
end;

procedure ClearBitmap(Bitmap: Graphics.TBitmap);
begin
  if Assigned(Bitmap) then
  begin
    Bitmap.Height := 0;
    Bitmap.Width := 0;
  end;
end;

{ TfmClassBrowser }

procedure TfmClassBrowser.AddProject;
var
  Node: TTreeNode;
  Item: TClassItem;
  ProjectName: string;
begin
  ProjectName := GxOtaGetCurrentProjectFileName;
  if AnsiCompareText(FLastProject, ProjectName) = 0 then
    Exit;
  FLastProject := ProjectName;
  Node := tvBrowse.Items.Add(nil, ExtractFileName(ProjectName));
  Node.ImageIndex := ImageIndexClosedFolder;
  Node.SelectedIndex := ImageIndexOpenFolder;
  Item := ClassList.Add;
  Item.IsProject := True;
  Item.Directory := ExtractFilePath(ProjectName);
  Item.Name := ExtractPureFileName(ProjectName);
  if FileExists(Item.GenerateFilename(Item.Directory)) then
    Item.LoadFromFile(Item.Directory)
  else
    Item.Load;
  Node.Data := Item;
  LoadObjects(Item, Node);
end;

resourcestring
  SClassesParsed = '%d classes parsed in %g seconds';
  SSelectClassFirst = 'Please select a class in the tree first';

function GetClassesParsedText(ClassCount: Cardinal; Ticks: Cardinal): string;
begin
  Result := Format(SClassesParsed, [ClassCount, Ticks / 1000]);
end;

{ TfmClassBrowser }

procedure TfmClassBrowser.RemoveProject;
var
  ONode: TTreeNode;
  Node: TTreeNode;
  Item: TClassItem;
begin
  FLastProject := '';
  if tvBrowse.Items.Count = 0 then
    Exit;
  Node := tvBrowse.Items[0];
  if Node = nil then
    Exit;
  while Assigned(Node) do
  begin
    Item := TClassItem(Node.Data);
    if Item.IsProject then
    begin
      Item.SaveToFile(Item.Directory);
      FreeAndNil(Item);
      ONode := Node.GetNextSibling;
      FreeAndNil(Node);
      Node := ONode;
    end
    else
      Node := Node.GetNextSibling;
  end;
  tvBrowseChange(tvBrowse, nil);
end;

procedure TfmClassBrowser.LoadAllObjects;
var
  i: Integer;
  Node: TTreeNode;
  AClassItem: TClassItem;
begin
  TCursor_TempHourglass;
  tvBrowse.Items.BeginUpdate;
  try
    tvBrowse.Selected := nil;
    tvBrowse.Items.Clear;
    for i := 0 to ClassList.Count-1 do
    begin
      AClassItem := ClassList.Items[i];
      Node := tvBrowse.Items.AddObject(nil, AClassItem.Name, AClassItem);
      Node.ImageIndex := ImageIndexClosedFolder;
      Node.SelectedIndex := ImageIndexOpenFolder;
      LoadObjects(AClassItem, Node);
    end;
  finally
    tvBrowse.Items.EndUpdate;
  end;
end;

procedure TfmClassBrowser.LoadObjects(Item: TClassItem; ONode: TTreeNode);
resourcestring
  SSorting = 'Sorting...';
begin
  TCursor_TempHourglass;
  tvBrowse.Items.BeginUpdate;
  try
    tvBrowse.SortType := stNone;
    tvBrowse.Selected := nil;
    // while ONode.Count > 0 do
    //  ONode.Item[0].Free;
    ONode.DeleteChildren;
    case FInfoViewMode of
      vmList: LoadClassList(Item, ONode);
      vmTree: LoadClassTree(Item, ONode);
    else
      Assert(False, 'Unknown view mode!');
    end;
    ONode.Expand(False);
  finally
    StatusBar.SimpleText := SSorting;
    StatusBar.Repaint;
    tvBrowse.SortType := ComCtrls.stText;
    tvBrowse.Items.EndUpdate;
    StatusBar.SimpleText := '';
  end;
end;

procedure TfmClassBrowser.LoadClassTree(Item: TClassItem; ONode: TTreeNode);
var
  TempClassList: TStrings;
  Node: TTreeNode;
  i: Integer;

  procedure AddObjects(INode: TTreeNode);
  var
    j: Integer;
    ANode: TTreeNode;
    NodeText: string;
    AClassItem: TBrowseClassInfoCollection;
  begin
    j := TempClassList.Count-1;
    while j >= 0 do
    begin
      AClassItem := TBrowseClassInfoCollection(TempClassList.Objects[j]);

      if SameText(AClassItem.ObjectDerivedFrom, TBrowseClassInfoCollection(INode.Data).Name) then
      begin
        if FViewUnitNames then
        begin
          NodeText := AClassItem.SourceName + '.' + AClassItem.Name;
        end
        else
          NodeText := AClassItem.Name;

        ANode := tvBrowse.Items.AddChildObject(INode, NodeText, AClassItem);
        if AClassItem.ObjectType = cieClass then begin
          ANode.ImageIndex := ImageIndexGear;
          ANode.SelectedIndex := ImageIndexGear;
        end else begin
          ANode.ImageIndex := ImageIndexInterface;
          ANode.SelectedIndex := ImageIndexInterface;
        end;

        TempClassList.Delete(j);

        AddObjects(ANode);
        j := TempClassList.Count-1;
      end
      else
        Dec(j);
    end;
  end;

var
  NodeText: string;
  ClassInfoCollItem: TBrowseClassInfoCollection;
begin
  TempClassList := TStringList.Create;
  try
    for i := 0 to Item.ClassCount - 1 do
    begin
      ClassInfoCollItem := Item.ClassItem[i];
      TempClassList.AddObject(ClassInfoCollItem.Name, ClassInfoCollItem);
    end;

    // First add root items.
    i := TempClassList.Count-1;
    while i >= 0 do
    begin
      ClassInfoCollItem := TBrowseClassInfoCollection(TempClassList.Objects[i]);

      if Item.ObjectByName(ClassInfoCollItem.ObjectDerivedFrom) = nil then
      begin
        // This is a root item - ObjectDerivedFrom = nil --> no ancestor.
        if FViewUnitNames then
        begin
          NodeText := ClassInfoCollItem.SourceName + '.' + ClassInfoCollItem.Name;
        end
        else
          NodeText := ClassInfoCollItem.Name;

        Node := tvBrowse.Items.AddChildObject(ONode, NodeText, ClassInfoCollItem);
        if ClassInfoCollItem.ObjectType = cieClass then begin
          Node.ImageIndex := ImageIndexGear;
          Node.SelectedIndex := ImageIndexGear;
        end else begin
          Node.ImageIndex := ImageIndexInterface;
          Node.SelectedIndex := ImageIndexInterface;
        end;

        TempClassList.Delete(i);

        AddObjects(Node);
        i := TempClassList.Count-1;
      end
      else
        Dec(i);
    end;

    Assert(TempClassList.Count = 0, 'Bad algorithm building tree');

  finally
    FreeAndNil(TempClassList);
  end;
end;

procedure TfmClassBrowser.LoadClassList(Item: TClassItem; ONode: TTreeNode);
var
  INode: TTreeNode;
  i: Integer;
  NodeText: string;
  ClassInfo: TBrowseClassInfoCollection;
begin
  for i := 0 to Item.ClassCount-1 do
  begin
    ClassInfo := Item.ClassItem[i];
    if (FTreeFilter='') or StrContains(FTreeFilter, ClassInfo.Name, False) then begin
      if FViewUnitNames then
        NodeText := ClassInfo.SourceName + '.' + ClassInfo.Name
      else
        NodeText := ClassInfo.Name;
      INode := tvBrowse.Items.AddChildObject(ONode, NodeText, ClassInfo);
      if ClassInfo.ObjectType = cieClass then begin
        INode.ImageIndex := ImageIndexGear;
        INode.SelectedIndex := ImageIndexGear;
      end else begin
        INode.ImageIndex := ImageIndexInterface;
        INode.SelectedIndex := ImageIndexInterface;
      end;
    end;
  end;
end;

procedure TfmClassBrowser.ParseFile(Sender: TObject; const FileName: string;
  FileIndex, FileCount: Integer);
resourcestring
  SParsingProgress = 'Parsing classes in %s ...';
begin
{$IFDEF DoShowProgressForm}
  if fmProgress = nil then
    fmProgress := TfmClassParsing.CreateAndShow(Self, FileCount);
  if Assigned(fmProgress) then
    fmProgress.SetProgress(Format(SParsingProgress, [ExtractFileName(FileName)]), FileIndex);
  Application.ProcessMessages;
{$ENDIF DoShowProgressForm}
end;

procedure TfmClassBrowser.EndParse(Sender: TObject);
begin
  FreeAndNil(fmProgress);
end;

procedure TfmClassBrowser.LoadList(OInfo: TBrowseClassInfoCollection);

  procedure SetSubItemImage(const ListItem: TListItem; SubItem, Value: Integer);
  begin
    ListItem.SubItemImages[SubItem] := Value;
  end;

var
  AMethod: TBrowseMethodInfoItem;
  i: Integer;
  ListItem: TListItem;
begin
  lvInfo.Items.BeginUpdate;
  try
    lvInfo.Items.Clear;

    FMethodText.Clear;
    for i := 0 to OInfo.Count-1 do
    begin
      AMethod := OInfo.Items[i];
      if CheckFilter(AMethod) then
      begin
        listItem := lvInfo.Items.Add;
        ListItem.Caption := '';
        ListItem.ImageIndex := Ord(AMethod.MethodDeclare) + ImageIndexVisibility;
        ListItem.SubItems.Add('');
        SetSubItemImage(ListItem, 0, Ord(AMethod.MethodType) + ImageIndexMemberType);
        ListItem.SubItems.Add('');
        if AMethod.cVirtual then
          SetSubItemImage(ListItem, 1, ImageIndexVirtual)
        else if AMethod.cDynamic then
          SetSubItemImage(ListItem, 1, ImageIndexDynamic)
        else if AMethod.cMessage then
          SetSubItemImage(ListItem, 1, ImageIndexMessage)
        else if AMethod.cOverride then
          SetSubItemImage(ListItem, 1, ImageIndexOverride);
        ListItem.SubItems.Add('');
        if AMethod.cAbstract then
          SetSubItemImage(ListItem, 2, ImageIndexCheck);
        ListItem.SubItems.Add('');
        if AMethod.cOverload then
          SetSubItemImage(ListItem, 3, ImageIndexCheck);
        ListItem.SubItems.Add(CompressWhiteSpace(AMethod.DName));
        ListItem.Data := AMethod;
      end;
    end;
  finally
    lvInfo.Items.EndUpdate;
  end;
end;

procedure TfmClassBrowser.lvInfoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
resourcestring
  SHintVisibility = 'Visibility';
  SHintType = 'Type';
  SHintDirective = 'Directive';
  SHintVisPrivate = 'Private';
  SHintVisProtected = 'Protected';
  SHintVisPublic = 'Public';
  SHintVisPublished = 'Published';
  SHintTypeConstant = 'Constant';
  SHintTypeMethod = 'Method';
  SHintTypeType = 'Type';
  SHintTypeVariable = 'Variable';
  SHintTypeProperty = 'Property';
  SHintDirVirtual = 'Virtual';
  SHintDirDynamic = 'Dynamic';
  SHintDirMessage = 'Message';
  SHintDirOverride = 'Override';
  SHintDirAbstract = 'Abstract';
  SHintDirOverload = 'Overload';
  SHintDirReintroduce = 'Reintroduce';
var
  lvHitInfo: TLVHitTestInfo;
  Hint: string;
begin
  ZeroMemory(@lvHitInfo, SizeOf(lvHitInfo));
  lvHitInfo.pt.X := X;
  lvHitInfo.pt.Y := Y;
  if - 1 <> lvInfo.Perform(LVM_SUBITEMHITTEST, 0, LParam(@lvHitInfo)) then begin
    Hint := '';
    case lvHitInfo.iSubItem of
      0: begin
        Hint := SHintVisibility;
        case lvInfo.Items[lvHitInfo.iItem].ImageIndex - ImageIndexVisibility of
          Ord(cdPrivate): Hint := Hint + ': ' + SHintVisPrivate;
          Ord(cdProtected): Hint := Hint + ': ' + SHintVisProtected;
          Ord(cdPublic): Hint := Hint + ': ' + SHintVisPublic;
          Ord(cdPublished): Hint := Hint + ': ' + SHintVisPublished;
        end;
      end;
      1: begin
        Hint := SHintType;
        case lvInfo.Items[lvHitInfo.iItem].SubItemImages[lvHitInfo.iSubItem - 1] - ImageIndexMemberType of
          Ord(ctConstant): Hint := Hint + ': ' + SHintTypeConstant;
          Ord(ctMethod): Hint := Hint + ': ' + SHintTypeMethod;
          Ord(ctType): Hint := Hint + ': ' + SHintTypeType;
          Ord(ctVariable): Hint := Hint + ': ' + SHintTypeVariable;
          Ord(ctProperty): Hint := Hint + ': ' + SHintTypeProperty;
        end;
      end;
      2: begin
        Hint := SHintDirective;
        case lvInfo.Items[lvHitInfo.iItem].SubItemImages[lvHitInfo.iSubItem - 1] of
          ImageIndexVirtual: Hint := Hint + ': ' + SHintDirVirtual;
          ImageIndexDynamic: Hint := Hint + ': ' + SHintDirDynamic;
          ImageIndexMessage: Hint := Hint + ': ' + SHintDirMessage;
          ImageIndexOverride: Hint := Hint + ': ' + SHintDirOverride;
        end;
      end;
      3: Hint := SHintDirAbstract;
      4: Hint := SHintDirOverload;
    end;
    if (Hint <> '')
      and ((FLastHitTestItemIdx <> lvHitInfo.iItem) or (FLastHitTestSubItemIdx <> lvHitInfo.iSubItem)) then begin
      FLastHitTestItemIdx := lvHitInfo.iItem;
      FLastHitTestSubItemIdx := lvHitInfo.iSubItem;
      lvInfo.Hint := Hint;
      Application.ActivateHint(Mouse.CursorPos);
    end;
  end;
end;

function TfmClassBrowser.GetMethodString(M: TBrowseMethodInfoItem): string;
begin
  Result := IntToStr(Ord(M.MethodDeclare)) + #9 + IntToStr(Ord(M.MethodType)) + #9 +
            IntToStr(Ord(M.cVirtual)) + #9 + IntToStr(Ord(M.cAbstract)) + #9 +
            IntToStr(Ord(M.cOverride)) + #9 + FilterTab(M.DName);
end;

procedure TfmClassBrowser.timKeyDelayTimer(Sender: TObject);
begin
  timKeyDelay.Enabled := False;
  FTreeFilter := edtClassFilter.Text;
  LoadAllObjects;
end;

procedure TfmClassBrowser.tvBrowseChange(Sender: TObject; Node: TTreeNode);
resourcestring
  SSourceModule = 'Source module: %s  (%d ms)';
var
  OInfo: TBrowseClassInfoCollection;
  TimeSpent: DWORD;
begin
  lvInfo.Items.BeginUpdate;
  try
    lvInfo.Items.Clear;
    if Node = nil then
      actFileRemove.Enabled := False
    else
      actFileRemove.Enabled := (Node.Level = 0);
    if TryGetClassInfo(OInfo) then begin
      TCursor_TempHourglass;

      if not OInfo.IsLoaded then
        OInfo.LoadMethods;
      TimeSpent := GetTickCount;
      if pcMain.ActivePage = tshCode then
        LoadCode;
      if pcMain.ActivePage = tshInherit then
        DrawInheritance;
      TimeSpent := GetTickCount - TimeSpent;
      LoadList(OInfo);
      StatusBar.SimpleText := Format(SSourceModule, [OInfo.SourceName, TimeSpent]);
    end else begin
      StatusBar.SimpleText := '';
      FMethodText.Clear;
      FCodeText.Clear;
      FCurrentCodePaneFile := '';
      while scInherit.ControlCount > 0 do
        scInherit.Controls[0].Free;
    end;
  finally
    lvInfo.Items.EndUpdate;
  end;
  pnlDataResize(Self);
end;

function TfmClassBrowser.CheckFilter(MInfo: TBrowseMethodInfoItem): Boolean;
var
  Filter: string;
begin
  Result := True;
  case MInfo.MethodType of
    ctConstant:   Result := actViewConstants.Checked;
    ctMethod:     Result := actViewMethods.Checked;
    ctType:       Result := actViewTypes.Checked;
    ctVariable:   Result := actViewVariables.Checked;
    ctProperty:   Result := actViewProperties.Checked;
  end;
  if Result then begin
    case MInfo.MethodDeclare of
      cdPrivate:   Result := actViewPrivate.Checked;
      cdProtected: Result := actViewProtected.Checked;
      cdPublic:    Result := actViewPublic.Checked;
      cdPublished: Result := actViewPublished.Checked;
    end;
  end;
  if Result then begin
    Filter := edtMemberFilter.Text;
    if (Filter <> '') and not StrContains(Filter, MInfo.RName, False) then
      Result := False;
  end;
end;

procedure TfmClassBrowser.pnlDataResize(Sender: TObject);
begin
  if csDestroying in ComponentState then Exit;
    if lvInfo.ClientWidth > 50 then
    begin
      lvInfo.Columns[5].Width := Max(lvInfo.ClientWidth - lvInfo.Columns[0].Width - lvInfo.Columns[1].Width -
        lvInfo.Columns[2].Width - lvInfo.Columns[3].Width - lvInfo.Columns[4].Width, 0);
    end;
  tshMembers.Width := pcMain.ActivePage.Width;
  tshMembers.Height := pcMain.ActivePage.Height;
  DrawResize;
end;

procedure TfmClassBrowser.DrawResize;
var
  i: Integer;
  W: Integer;
  ShapeLeft: Integer;
  L: Integer;
  pnl: TPanel;
  shp: TShape;
begin
  // todo: This could use GX_dzVclUtils.TWinControl_Lock
  SendMessage(scInherit.Handle, WM_SETREDRAW, WPARAM(False), 0);
  try
    tshInherit.Width := pcMain.ActivePage.Width;
    tshInherit.Height := pcMain.ActivePage.Height;
    scInherit.Width := tshInherit.Width;
    scInherit.Height := tshInherit.Height;
    W := scInherit.Width - 20;
    if W > 300 then W := 300;
    L := (scInherit.Width - W) div 2;
    ShapeLeft := L + (W div 2) - 2;
    for i := 0 to scInherit.ControlCount - 1 do
      if scInherit.Controls[i] is TPanel then begin
        pnl := TPanel(scInherit.Controls[i]);
        pnl.SetBounds(L, pnl.Top, W, pnl.Height)
      end else if scInherit.Controls[i] is TShape then begin
        shp := TShape(scInherit.Controls[i]);
        shp.SetBounds(ShapeLeft, shp.Top, 2, 20);
      end;
  finally
    SendMessage(scInherit.Handle, WM_SETREDRAW, WPARAM(True), 0);
    for i := 0 to scInherit.ControlCount - 1 do
       scInherit.Controls[i].Invalidate;
    scInherit.Invalidate;
  end;
end;

procedure TfmClassBrowser.edtClassFilterChange(Sender: TObject);
begin
  timKeyDelay.Enabled := False;
  timKeyDelay.Enabled := True; // FI:W508 Variable is assigned twice successively
end;

procedure TfmClassBrowser.edtClassFilterKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key in [VK_DOWN, VK_UP, VK_NEXT, VK_PRIOR]) then begin
    if tvBrowse.Items.Count > 1 then
      tvBrowse.Perform(WM_KEYDOWN, Key, 0)
    else if tvBrowse.Items.Count = 1 then
      tvBrowse.Items[0].Selected := True;
    Key := 0;
  end;
end;

procedure TfmClassBrowser.edtMemberFilterChange(Sender: TObject);
begin
  tvBrowseChange(tvBrowse, tvBrowse.Selected);
end;

procedure TfmClassBrowser.edtMemberFilterKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key in [VK_DOWN, VK_UP, VK_NEXT, VK_PRIOR]) then begin
    if lvInfo.Items.Count > 1 then
      lvInfo.Perform(WM_KEYDOWN, Key, 0)
    else if lvInfo.Items.Count = 1 then
      lvInfo.Items[0].Selected := True;
    Key := 0;
  end;
end;

procedure TfmClassBrowser.lvInfoChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
var
  InfoItem: TBrowseMethodInfoItem;
begin
  FMethodText.Text := '';
  if Assigned(lvInfo.Selected) then
  begin
    InfoItem := TBrowseMethodInfoItem(lvInfo.Selected.Data);
    if Assigned(InfoItem) then
    begin
      FMethodText.Text := CompressWhiteSpace(FilterTab(InfoItem.DName));
      StatusBar.SimpleText := CompressWhiteSpace(FilterTab(FMethodText.Text));
    end;
  end;
end;

procedure TfmClassBrowser.LoadCode;

  procedure LoadCodePane(const F: string);
  var
    Lines: TGXUnicodeStringList;
  begin
    if FCurrentCodePaneFile = F then
    begin
      if FCodeText.LineCount > 2 then
        Exit;
    end
    else
      FCurrentCodePaneFile := F;

    Lines := TGXUnicodeStringList.Create;
    try
      GxOtaLoadFileToUnicodeStrings(F, Lines);
      FCodeText.Text := Lines.Text;
    finally
      FreeAndNil(Lines);
    end;
  end;

var
  LineNumber: Integer;
  SourceFileName: string;
  MInfo: TBrowseMethodInfoItem;
  OInfo: TBrowseClassInfoCollection;
begin
  TCursor_TempHourglass;

  FCodeText.BeginUpdate;
  try
    if not TryGetClassInfo(OInfo) then
      Exit; //==>
    SourceFileName := OInfo.FileName;
    LineNumber := -1;
    if lvInfo.Selected = nil then
      LineNumber := OInfo.RefreshLineNo
    else
    begin
      MInfo := TBrowseMethodInfoItem(lvInfo.Selected.Data);
      case MInfo.MethodType of
        ctConstant,
        ctType,
        ctVariable,
        ctProperty:   LineNumber := MInfo.GetInterfaceLine;
        ctMethod:
          begin
            if MInfo.cAbstract then
              LineNumber := MInfo.GetInterfaceLine
            else
              LineNumber := MInfo.GetImplementationLine;
          end;
      end;
    end;

    LoadCodePane(SourceFileName);

    if LineNumber >= 0 then
    begin
      FCodeText.TopLine := LineNumber + 1;
      FCodeText.CaretXY := Point(1, LineNumber);
    end;
  finally
    FCodeText.EndUpdate;
  end;
end;

procedure TfmClassBrowser.pcMainChange(Sender: TObject);
begin
  if pcMain.ActivePage = tshCode then
    LoadCode
  else
    if pcMain.ActivePage = tshInherit then
      DrawInheritance;
end;

procedure TfmClassBrowser.RefreshNode;
var
  Ticks: DWORD;
  Item: TClassItem;
begin
  TCursor_TempHourglass;

  Ticks := GetTickCount;
  Item := TClassItem(tvBrowse.Selected.Data);
  Item.Recurse := FParseRecursing;
  Item.Load;
  LoadObjects(Item, tvBrowse.Selected);
  StatusBar.SimpleText := GetClassesParsedText(Item.ClassCount, GetTickCount - Ticks);
end;

procedure TfmClassBrowser.GetInheritedList(List: TStrings; const StartClassName: string);
var
  LastVisitedLevel: Integer;
  FirstNode: TTreeNode;

  function GetInherited(const Comp: string): string;
  var
    OInfo: TBrowseClassInfoCollection;
    Node: TTreeNode;
  begin
    Result := '';
    Node := FirstNode;
    while Assigned(Node) do
    begin
      if Node.Level > LastVisitedLevel then
      begin
        if SameText(TBrowseClassInfoCollection(Node.Data).Name, Comp) then
        begin
          OInfo := TBrowseClassInfoCollection(Node.Data);
          Result := OInfo.DerivedFrom;
          if SameText(Result, Comp) then begin
            // prevent infinite loop for IInterface and TObject
            Result := '';
            Exit;
          end;
          // We could instead recurse the treeview backwards to get
          // this information if we are in "tree" mode.
          if FStayInPackage then
            LastVisitedLevel := Node.Level;
          Break;
        end;
      end;
      Node := Node.GetNext;
    end;
  end;

var
  InheritingFromClassName: string;
  CommaPosition: Integer;
begin
  FirstNode := tvBrowse.Items.GetFirstNode;
  LastVisitedLevel := FirstNode.Level;

  InheritingFromClassName := GetInherited(StartClassName);
  while InheritingFromClassName <> '' do
  begin
    List.Add(InheritingFromClassName);
    CommaPosition := Pos(',', InheritingFromClassName);
    // With multiple ancestors we want to search for the first one.
    if CommaPosition > 0 then
      InheritingFromClassName := Copy(InheritingFromClassName, 1, CommaPosition - 1);

    InheritingFromClassName := GetInherited(InheritingFromClassName);
  end;
end;

procedure TfmClassBrowser.ClickInheritancePanel(Sender: TObject);
//var
//  Panel: TPanel;
begin //FI:W519
  // Panel := Sender as TPanel;
  { TODO -oAnyone : Jump to the class declaration, or treeview node? }
end;

procedure TfmClassBrowser.DrawInheritance;
var
  ClassControlLeft: Integer;
  ClassControlWidth: Integer;

  procedure AddPanel(const PanelText: string; PanelPosition: Integer);
  var
    APanel: TPanel;
  begin
    APanel := TPanel.Create(scInherit);
    APanel.Visible := True;
    APanel.Parent := scInherit;
    APanel.OnClick := ClickInheritancePanel;
    SendMessage(APanel.Handle, WM_SETREDRAW, WPARAM(True), 0);
    APanel.BorderWidth := 1;
    APanel.FullRepaint := False;
    APanel.Caption := PanelText;
    APanel.BevelInner := bvLowered;
    APanel.SetBounds(ClassControlLeft, (PanelPosition * 50) + 10, ClassControlWidth, 30);

    // Assumes that form and panel have the same font
    if Self.Canvas.TextWidth(Caption) > APanel.ClientWidth - (2 * APanel.BorderWidth) then
    begin
      APanel.ShowHint := True;
      APanel.Hint := Caption;
    end;
  end;

  procedure AddShape(LeftPos: Integer; Index: Integer);
  var
    shp: TShape;
  begin
    shp := TShape.Create(scInherit);
    shp.Visible := True;
    shp.Parent := scInherit;
    shp.SetBounds(LeftPos, ((Index - 1) * 50) + 40, 2, 20);
  end;

var
  OInfo: TBrowseClassInfoCollection;
  InheritList: TStrings;
  ClassIndex: Integer;
  ShapeLeft: Integer;
  DrawClassName: string;
  i: Integer;
  EntryCount: Integer;
begin
  if not TryGetClassInfo(OInfo) then
    Exit; //==>

  TCursor_TempHourglass;

  // Get the list of ancestor classes.
  InheritList := TStringList.Create;
  try
    DrawClassName := OInfo.Name;
    InheritList.Add(DrawClassName);
    GetInheritedList(Inheritlist, DrawClassName);

    scInherit.DisableAutoRange;
    SendMessage(scInherit.Handle, WM_SETREDRAW, WPARAM(False), 0);
    try
      while scInherit.ControlCount > 0 do
        scInherit.Controls[0].Free;

      // Set up parameters for drawing the respective class items.
      ClassControlWidth := scInherit.Width - 20;
      if ClassControlWidth > 300 then ClassControlWidth := 300;

      ClassControlLeft := (scInherit.Width - ClassControlWidth) div 2;
      ShapeLeft := ClassControlLeft + (ClassControlWidth div 2) - 2;

      EntryCount := InheritList.Count-1;

      if FPrimitiveTop then
        ClassIndex := 0
      else
        ClassIndex := EntryCount;

      for i := EntryCount downto 0 do
      begin
        AddPanel(InheritList.Strings[ClassIndex], i);

        if i > 0 then
          AddShape(ShapeLeft, i);

        if FPrimitiveTop then
          Inc(ClassIndex)
        else
          Dec(ClassIndex);
      end;

    finally
      scInherit.EnableAutoRange;
      SendMessage(scInherit.Handle, WM_SETREDRAW, WPARAM(True), 0);

      for i := 0 to scInherit.ControlCount-1 do
        scInherit.Controls[i].Invalidate;
      scInherit.Invalidate;
    end;

  finally
    FreeAndNil(InheritList);
  end;
end;

procedure TfmClassBrowser.SaveSettings;
var
  Settings: IExpertSettings;
  i: Integer;
begin
  Settings := TClassBrowserExpert.GetSettings;
  // Do not localize any of the following lines.
  Settings.SaveForm('Window', Self);
  Settings.Subkey('Window').WriteInteger('Split', tvBrowse.Width);
  Settings.WriteInteger('ViewMode', Ord(FInfoViewMode));
  Settings.WriteBool('PrimitiveTop', FPrimitiveTop);
  Settings.WriteBool('StayInPackage', FStayInPackage);
  Settings.WriteBool('ParseRecursing', FParseRecursing);
  Settings.WriteBool('AutomaticallyHideBrowser', FAutomaticallyHideBrowser);
  Settings.WriteBool('UnitNames', FViewUnitNames);
  Settings.WriteInteger('ClassHierarchyFontSize', FClassHierarchyFontSize);
  Settings.WriteInteger('ClassHierarchyBoxWidth', FClassHierarchyBoxWidth);
  Settings.WriteInteger('ClassHierarchyBoxSpace', FClassHierarchyBoxSpace);
  Settings.WriteString('ClassHierarchyFont', 'Arial');
  for i := Low(FFilters) to High(FFilters) do
    Settings.WriteBool(Format('Filter%d', [i]), FFilters[i]);
  Settings.SaveFont('TreeFont', tvBrowse.Font);
  Settings.SaveFont('ListFont', lvInfo.Font);
  Settings.SaveFont('EditorFont', FCodeText.Font);
end;

procedure TfmClassBrowser.LoadSettings;
var
  Settings: IExpertSettings;
  i: Integer;
begin
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;

  Settings := TClassBrowserExpert.GetSettings;
  // Do not localize any of the following lines.
  Settings.LoadForm('Window', Self);
  tvBrowse.Width := Settings.Subkey('Window').ReadInteger('Split', tvBrowse.Width);
  if tvBrowse.Width = 0 then
    tvBrowse.Width := 100;
  FInfoViewMode := TInfoViewMode(Settings.ReadInteger('ViewMode', Ord(FInfoViewMode)));
  FViewUnitNames := Settings.ReadBool('UnitNames', False);
  FClassHierarchyFontSize := Settings.ReadInteger('ClassHierarchyFontSize', 8);
  FClassHierarchyBoxWidth := Settings.ReadInteger('ClassHierarchyBoxWidth', 25);
  FClassHierarchyBoxSpace := Settings.ReadInteger('ClassHierarchyBoxSpace', 10);
  FClassHierarchyFont := Settings.ReadString('ClassHierarchyFont', 'Arial');
  ClassList.StoragePath := AddSlash(ConfigInfo.ConfigPath + ClassBrowserStorageFolder);
  FPrimitiveTop := Settings.ReadBool('PrimitiveTop', FPrimitiveTop);
  FStayInPackage := Settings.ReadBool('StayInPackage', FStayInPackage);
  FParseRecursing := Settings.ReadBool('ParseRecursing', FParseRecursing);
  FAutomaticallyHideBrowser := Settings.ReadBool('AutomaticallyHideBrowser', FAutomaticallyHideBrowser);
  for i := Low(FFilters) to High(FFilters) do
    FFilters[i] := Settings.ReadBool(Format('Filter%d', [i]), True);

  Settings.LoadFont('TreeFont', tvBrowse.Font);
  Settings.LoadFont('ListFont', lvInfo.Font);
  Settings.LoadFont('EditorFont', FCodeText.Font);
  FMethodText.Font.Assign(FCodeText.Font);
  FiltersToActions;
end;

procedure TfmClassBrowser.FileNotification(NotifyCode: TOTAFileNotification; const FileName: string;
  var Cancel: Boolean);
begin
  case NotifyCode of
    ofnActiveProjectChanged: begin
        if not SameText(FileName, FLastProject) then begin
          RemoveProject;
          if Visible then
            AddProject
          else
            FProjectNeedsLoading := True;
        end;
      end;
    ofnFileClosing:
      if SameText(FileName, FLastProject) then
        RemoveProject;
  end;
end;

procedure TfmClassBrowser.FiltersToActions;
begin
  actViewConstants.Checked := FFilters[0];
  actViewMethods.Checked := FFilters[1];
  actViewTypes.Checked := FFilters[2];
  actViewVariables.Checked := FFilters[3];
  actViewProperties.Checked := FFilters[4];
  actViewPrivate.Checked:= FFilters[5];
  actViewProtected.Checked := FFilters[6];
  actViewPublic.Checked := FFilters[7];
  actViewPublished.Checked := FFilters[8];
  UpdateListFilter;
end;

procedure TfmClassBrowser.PrintClassReportBuiltIn;
resourcestring
  SClassReport = 'Class Report';
var
  OInfo: TBrowseClassInfoCollection;
begin
  if not TryGetClassInfo(OInfo) then
    Exit; //==>
  TCursor_TempHourglass;
  Printer.Title := SClassReport;
  Printer.BeginDoc;
  try
    PrintClassBuiltIn(OInfo, Printer.Canvas);
  finally
    Printer.EndDoc;
  end;
end;

procedure TfmClassBrowser.PrintClassBuiltIn(OInfo: TBrowseClassInfoCollection; ACanvas: TCanvas);
const
  PR_OffsetX = 30;
  PR_OffsetY = 20;

  procedure PrintHeader(ACanvas: TCanvas; var Row: Integer; ColumnWidth: Integer; FontHeight: Integer);
  resourcestring
    SName = 'Name';
  begin
    ACanvas.Font.Style := [fsBold];
    ACanvas.TextOut(PR_OffsetX, Row, 'Vi');
    ACanvas.TextOut(PR_OffsetX + ColumnWidth, Row, 'Ty');
    ACanvas.TextOut(PR_OffsetX + ColumnWidth * 2, Row, 'Di');
    ACanvas.TextOut(PR_OffsetX + ColumnWidth * 3, Row, 'Ab');
    ACanvas.TextOut(PR_OffsetX + ColumnWidth * 4, Row, 'Ol');
    ACanvas.TextOut(PR_OffsetX + ColumnWidth * 6, Row, SName);
    ACanvas.MoveTo(PR_OffsetX, Row + FontHeight + 1);
    ACanvas.LineTo(PR_OffsetX + Printer.PageWidth - (2 * PR_OffsetX), Row + FontHeight + 1);
    Row := Row + FontHeight + 4;
    ACanvas.Font.Style := [];
  end;

  procedure PrintBitmap(Canvas: TCanvas; DestRect: TRect; Bitmap: Graphics.TBitmap);
  var
    BitmapHeader: pBitmapInfo;
    BitmapImage: Pointer;
    HeaderSize: DWord;
    ImageSize: DWord;
  begin
    if Bitmap.Width = 0 then
      Exit;
    GetDIBSizes(Bitmap.Handle, HeaderSize, ImageSize);
    GetMem(BitmapHeader, HeaderSize);
    GetMem(BitmapImage, ImageSize);
    try
      GetDIB(Bitmap.Handle, Bitmap.Palette, BitmapHeader^, BitmapImage^);
      StretchDIBits(Canvas.Handle,
                    DestRect.Left, DestRect.Top,     // Destination Origin
                    DestRect.Right  - DestRect.Left, // Destination Width
                    DestRect.Bottom - DestRect.Top,  // Destination Height
                    0, 0,                            // Source Origin
                    Bitmap.Width, Bitmap.Height,     // Source Width & Height
                    BitmapImage,
                    TBitmapInfo(BitmapHeader^),
                    DIB_RGB_COLORS,
                    SRCCOPY);
    finally
      FreeMem(BitmapHeader);
      FreeMem(BitmapImage)
    end;
  end;

var
  i: Integer;
  Row: Integer;
  ColumnWidth: Integer;
  FontHeight: Integer;
  List: TStrings;
  MInfo: TBrowseMethodInfoItem;
  BitmapSize: Integer;
  Bitmap: Graphics.TBitmap;
resourcestring
  SClass = 'Class: ';
  SAncestor = 'Ancestor: ';
  SUnit = 'Unit: ';
begin
  Row := PR_OffSetY;
  ACanvas.Font.Name := 'Arial';
  ACanvas.Font.Size := 12;
  ACanvas.Font.Style := [fsBold];
  ACanvas.TextOut(PR_OffsetX, Row, SClass + OInfo.Name);
  Row := Row + ACanvas.TextHeight(SAllAlphaNumericChars) + 5;
  ACanvas.Font.Size := 10;
  FontHeight := ACanvas.TextHeight(SAllAlphaNumericChars);
  ColumnWidth := ACanvas.TextWidth('WW');
  BitmapSize := FontHeight - 1;
  ACanvas.TextOut(PR_OffsetX, Row, SAncestor + OInfo.DerivedFrom);
  Row := Row + FontHeight + 5;
  ACanvas.TextOut(PR_OffsetX, Row, SUnit + OInfo.SourceName);
  Row := Row + FontHeight + 10;
  PrintHeader(ACanvas, Row, ColumnWidth, FontHeight);
  ACanvas.Font.Style := [];
  List := TStringList.Create;
  try
    Bitmap := Graphics.TBitmap.Create;
    try
      for i := 0 to OInfo.Count - 1 do
        List.AddObject(GetMethodString(OInfo.Items[i]), OInfo.Items[i]);
      for i := 0 to List.Count - 1 do
      begin
        MInfo := TBrowseMethodInfoItem(List.Objects[i]);
        ClearBitmap(Bitmap);
        Images.GetBitmap(Ord(MInfo.MethodDeclare) + ImageIndexVisibility, Bitmap);
        PrintBitmap(ACanvas, Rect(PR_OffsetX, Row, PR_OffsetX + BitmapSize, Row + BitmapSize), Bitmap);
        ClearBitmap(Bitmap);
        Images.GetBitmap(Ord(MInfo.MethodType) + ImageIndexMemberType, Bitmap);
        PrintBitmap(ACanvas, Rect(PR_OffsetX + ColumnWidth, Row, PR_OffsetX + ColumnWidth + BitmapSize, Row + BitmapSize), Bitmap);

        if MInfo.cVirtual then
          Images.GetBitmap(ImageIndexVirtual, Bitmap)
        else if MInfo.cDynamic then
          Images.GetBitmap(ImageIndexDynamic, Bitmap)
        else if MInfo.cMessage then
          Images.GetBitmap(ImageIndexMessage, Bitmap)
        else if MInfo.cOverride then
          Images.GetBitmap(ImageIndexOverride, Bitmap)
        else
          ClearBitmap(Bitmap);
        PrintBitmap(ACanvas, Rect(PR_OffsetX + ColumnWidth * 2, Row, PR_OffsetX + ColumnWidth * 2 + BitmapSize, Row + BitmapSize), Bitmap);

        if MInfo.cAbstract then
        begin
          ClearBitmap(Bitmap);
          Images.GetBitmap(ImageIndexCheck, Bitmap);
          PrintBitmap(ACanvas, Rect(PR_OffsetX + ColumnWidth * 3, Row, PR_OffsetX + ColumnWidth * 3 + BitmapSize, Row + BitmapSize), Bitmap);
        end;
        if MInfo.cOverload then
        begin
          ClearBitmap(Bitmap);
          Images.GetBitmap(ImageIndexCheck, Bitmap);
          PrintBitmap(ACanvas, Rect(PR_OffsetX + ColumnWidth * 4, Row, PR_OffsetX + ColumnWidth * 4 + BitmapSize, Row + BitmapSize), Bitmap);
        end;

        ACanvas.TextOut(PR_OffsetX + ColumnWidth * 6, Row, FilterTab(MInfo.DName));
        Row := Row + FontHeight + 1;
        if Row + ((FontHeight + 1) * 3) > Printer.PageHeight then
        begin
          Printer.NewPage;
          Row := PR_OffsetY;
          PrintHeader(ACanvas, Row, ColumnWidth, FontHeight);
        end;
      end;
    finally
      FreeAndNil(Bitmap);
    end;
  finally
    FreeAndNil(List);
  end;
end;

procedure TfmClassBrowser.FormActivate(Sender: TObject);
resourcestring
  SLoadingClasses = 'Loading stored classes...';
  SLoadingProject = 'Loading current project...';
begin
  if FIsFirstInvocation then
  begin
    FIsFirstInvocation := False;
    Application.ProcessMessages;

    TCursor_TempHourglass;

    StatusBar.SimpleText := SLoadingClasses;
    StatusBar.Repaint;
    ClassList.LoadFromFile;
    LoadAllObjects;
  end;

  if FProjectNeedsLoading then begin
    StatusBar.SimpleText := SLoadingProject;
    StatusBar.Repaint;
    FProjectNeedsLoading := False;
    RemoveProject;
    Application.ProcessMessages;
    AddProject;
  end;
  StatusBar.SimpleText := '';
end;

procedure TfmClassBrowser.PrintClassDiagramBuiltIn(OInfo: TBrowseClassInfoCollection;
    ACanvas: TCanvas; BoxSize, VSpace: Integer);
const
  PR_OffsetX = 30;
  PR_OffsetY = 20;

var
  x, y: Integer;
  BoxW, BoxH: Integer;
  IndentX, IndentY: Integer;
  PageNum: Integer;

  procedure PrintClassSquare(Info: TBrowseClassInfoCollection; Level: Integer; var Py: Integer);
  var
    Rect: TRect;
    Node: TTreeNode;
    np: Integer;
    op: Integer;
    OldBrushColor: TColor;
  begin
    if y + BoxH + 20 > Printer.PageHeight then
    begin
      Printer.NewPage;
      y := PR_OffsetY;
      Py := 0;
      Inc(PageNum);
    end;
    x := PR_OffsetX + IndentX * Level;
    Rect := Classes.Rect(x, y, x + BoxW, y + BoxH);
    OldBrushColor := ACanvas.Brush.Color;
    ACanvas.Brush.Color := clBlack;
    ACanvas.FrameRect(Rect);
    ACanvas.Brush.Color := OldBrushColor;
    np := Rect.Bottom;
    op := PageNum;
    if Level > 0 then begin
      ACanvas.MoveTo(Rect.Left, Rect.Top + (BoxH div 2));
      ACanvas.LineTo(Rect.Left - (IndentX div 2), Rect.Top + (BoxH div 2));
      ACanvas.LineTo(Rect.Left - (IndentX div 2), Py);
    end;
    Inc(Rect.Left, 1);
    Inc(Rect.Top, 1);
    Dec(Rect.Bottom, 1);
    Dec(Rect.Right, 1);
    ACanvas.TextRect(Rect, x + 4, y + 4, Info.Name);
    y := y + BoxH + IndentY;
    Node := tvBrowse.Items[0];
    StatusBar.SimpleText := Info.Name;
    while Assigned(Node) do
    begin
      if op <> PageNum then
        np := 0;
      Application.ProcessMessages;
      if Node.Level > 0 then
        if SameText(TBrowseClassInfoCollection(Node.Data).ObjectDerivedFrom, Info.Name) then
          PrintClassSquare(TBrowseClassInfoCollection(Node.Data), Level + 1, np);
      Node := Node.GetNext;
    end;
  end;

var
  p, i: Integer;
  st: string;
begin
  PageNum := 1;
  x := PR_OffsetX;
  y := PR_OffsetY;
  st := '';
  for i := 1 to BoxSize do //FI:W528
    st := st + 'W';
  BoxW := ACanvas.TextWidth(st) + 8;
  BoxH := ACanvas.TextHeight(st) + 8;
  IndentX := (BoxW div 2);
  IndentY := VSpace;
  p := 0;
  PrintClassSquare(OInfo, 0, p);
end;

procedure TfmClassBrowser.PrintClassHierarchyBuiltIn;
var
  OInfo: TBrowseClassInfoCollection;
resourcestring
  SClassReport = 'Class Hierarchy Report';
begin
  if not TryGetClassInfo(OInfo) then
    raise Exception.Create(SSelectClassFirst);

  TCursor_TempHourglass;

  Printer.Title := SClassReport;
  Printer.BeginDoc;
  try
    Printer.Canvas.Font.Name := FClassHierarchyFont;
    Printer.Canvas.Font.Size := FClassHierarchyFontSize;
    Printer.Canvas.Font.Style := [];
    PrintClassDiagramBuiltIn(OInfo, Printer.Canvas, FClassHierarchyBoxWidth,
      FClassHierarchyBoxSpace);
  finally
    Printer.EndDoc;
  end;
end;

function TfmClassBrowser.FilterTab(const Source: string): string;
var
  i: Integer;
begin
  Result := Source;
  for i := 1 to Length(Source) do
    if Result[i] = #9 then
      Result[i] := #32;
end;

constructor TfmClassBrowser.Create(AOwner: TComponent);
begin
  inherited;

  TControl_SetMinConstraints(Self);

  SetNonModalFormPopupMode(Self);
  FStartingDir := ExtractFilePath(Application.ExeName);
  FIsFirstInvocation := True;
  FProjectNeedsLoading := True;
  FLastHitTestItemIdx := -1;

  SetupEditorControls;

  InitDpiScaler;

  FPrimitiveTop := False;
  FStayInPackage := False;
  FParseRecursing := False;
  FAutomaticallyHideBrowser := True;
  FInfoViewMode := vmTree;

  FClassList := TClassList.Create;
  FClassList.OnParseFile := ParseFile;
  FClassList.OnEndParse := EndParse;

  LoadSettings;

  FProjectNotifier := TClassProjectNotifier.Create(Self);
  FProjectNotifier.AddNotifierToIDE;
end;

destructor TfmClassBrowser.Destroy;
begin
  SaveSettings;

  FProjectNotifier.RemoveNotifierFromIDE;
  FProjectNotifier := nil;

  FreeAndNil(FClassList);

  inherited Destroy;
end;

{$IFDEF GX_IDE_IS_HIDPI_AWARE}
procedure TfmClassBrowser.ApplyDpi(_NewDpi: Integer; _NewBounds: PRect);
var
  il: TImageList;
begin
  inherited;
  il := GExpertsInst.GetScaledSharedDisabledImages(_NewDpi);
  tbBrowse.DisabledImages := il;
  tbInfo.DisabledImages := il;

  il := GExpertsInst.GetScaledSharedImages(_NewDpi);
  tbBrowse.Images := il;
  tbInfo.Images := il;
  Actions.Images := il;
  MainMenu.Images := il;
end;
{$ENDIF}

procedure TfmClassBrowser.FindFromNode(const Text: string; Node: TTreeNode);
begin
  if not Assigned(Node) then
    Exit;
  while Assigned(Node) do
  begin
    if (Node.Level > 0) and (CaseInsensitivePos(Text, TBrowseClassInfoCollection(Node.Data).Name) > 0) then
    begin
      tvBrowse.Selected := Node;
      Break;
    end;
    Node := Node.GetNext;
  end;
end;

function TfmClassBrowser.Images: TImageList;
begin
  Result := GetSharedImageList;
end;

{ TClassBrowserExpert }

procedure TClassBrowserExpert.SetActive(New: Boolean);
begin
  if New <> Active then
  begin
    inherited SetActive(New);
    if New then //FI:W505
      // Nothing to do here.
    else
    begin
      if Assigned(fmClassBrowser) then
      begin
        if fmClassBrowser.Visible then
          fmClassBrowser.Close;

        FreeAndNil(fmClassBrowser);
      end;
    end;
  end;
end;

destructor TClassBrowserExpert.Destroy;
begin
  FreeAndNil(fmClassBrowser);

  inherited Destroy;
end;

function TClassBrowserExpert.GetActionCaption: string;
resourcestring
  SClassBrowserMenuCaption = 'Class Bro&wser';
begin
  Result := SClassBrowserMenuCaption;
end;

class function TClassBrowserExpert.GetName: string;
begin
  Result := 'ClassBrowser'; // do not localize
end;

procedure TClassBrowserExpert.Execute(Sender: TObject);
begin
  if fmClassBrowser = nil then
  begin
    fmClassBrowser := TfmClassBrowser.Create(nil);
    SetFormIcon(fmClassBrowser);
  end;
  if fmClassBrowser.WindowState = wsMinimized then
    fmClassBrowser.WindowState := wsNormal;
  fmClassBrowser.Show;
  IncCallCount;
end;

function TClassBrowserExpert.HasConfigOptions: Boolean;
begin
  HasConfigOptions := False;
end;

function TClassBrowserExpert.IsDefaultActive: Boolean;
begin
  Result := not RunningCPPBuilder;
end;

procedure TfmClassBrowser.actFilePrintClassReportExecute(Sender: TObject);
begin
  PrintClassReportBuiltIn;
end;

procedure TfmClassBrowser.actFilePrintClassHierarchyExecute(
  Sender: TObject);
begin
  PrintClassHierarchyBuiltIn;
end;

procedure TfmClassBrowser.actFileAddExecute(Sender: TObject);
var
  New, SDir: string;
  Node: TTreeNode;
  Ticks: DWORD;
  Item: TClassItem;
begin
  SDir := FStartingDir;
  if not GetDirectory(SDir) then
    Exit;
  FStartingDir := SDir;
  New := ExtractFileName(SDir);
  if not TfmClassIdentify.Execute(Self, New, FParseRecursing) then
    Exit;
  begin
    Node := tvBrowse.Items.Add(nil, New);
    Node.ImageIndex := ImageIndexClosedFolder;
    Node.SelectedIndex := ImageIndexOpenFolder;
    Ticks := GetTickCount;
    Item := ClassList.Add;
    Item.IsProject := False;
    Item.Directory := SDir;
    Item.Name := New;
    Item.Recurse := FParseRecursing;
    Item.Load;
    ClassList.SaveToFile(False);
    Node.Data := Pointer(Item);
    LoadObjects(Item, Node);
    StatusBar.SimpleText := GetClassesParsedText(Item.ClassCount, GetTickCount - Ticks);
    StatusBar.Repaint;
  end;
end;

procedure TfmClassBrowser.actFileRemoveExecute(Sender: TObject);
var
  Selected: TTreeNode;
  Item: TClassItem;
begin
  Selected := tvBrowse.Selected;
  if Selected = nil then
  begin
    actFileRemove.Enabled := False;
    Exit;
  end;
  Item := TClassItem(Selected.Data);
  Selected.Free;
  FreeAndNil(Item);
  ClassList.SaveToFile(False);
  if tvBrowse.Selected = nil then
    actFileRemove.Enabled := False;
end;

procedure TfmClassBrowser.actFileRefreshExecute(Sender: TObject);
var
  Selected: TTreeNode;
begin
  Selected := tvBrowse.Selected;
  if Selected = nil then
    Exit; //==>

  if Selected.Level = 0 then
    RefreshNode
  else
  begin
    TBrowseClassInfoCollection(Selected.Data).LoadMethods;
    tvBrowseChange(tvBrowse, Selected);
  end;
  ClassList.SaveToFile(False);
end;

procedure TfmClassBrowser.actEditFindExecute(Sender: TObject);
resourcestring
  SFindClass = 'Find Class';
  SEnterClassName = 'Enter a full or partial class name to find';
var
  Find: string;
begin
  if tvBrowse.Items.Count = 0 then
    Exit; //==>

  Find := InputBox(SFindClass, SEnterClassName, FLastFind);
  if Find = '' then
    Exit; //==>

  FLastFind := Find;
  FindFromNode(FLastFind, tvBrowse.Items[0]);
end;

procedure TfmClassBrowser.actGenericViewNewFilterExecute(Sender: TObject);
var
  SendingAction: TCustomAction;
begin
  SendingAction := Sender as TCustomAction;
  Assert(Assigned(SendingAction));
  SendingAction.Checked := not SendingAction.Checked;

  UpdateListFilter;
end;

function TfmClassBrowser.TryGetClassInfo(out _Info: TBrowseClassInfoCollection): Boolean;
var
  Selected: TTreeNode;
begin
  Selected := tvBrowse.Selected;
  Result := (Selected <> nil) and (Selected.Level > 0) and (Selected.Data <> nil);
  if Result then
    _Info := TBrowseClassInfoCollection(Selected.Data);
end;

procedure TfmClassBrowser.UpdateListFilter;
var
  OInfo: TBrowseClassInfoCollection;
begin
  if not TryGetClassInfo(Oinfo) then
    Exit; //==>
  TCursor_TempHourglass;
  LoadList(OInfo);
  StatusBar.SimpleText := OInfo.SourceName + ': ' + IntToStr(OInfo.LineNo);
end;

procedure TfmClassBrowser.SetupEditorControls;
begin
  FMethodText := TGxEnhancedEditor.Create(Self);
  FMethodText.Name := 'edtMethodText';
  FMethodText.Parent := pnlMethod;
  FMethodText.Align := alClient;
  FMethodText.ReadOnly := True;
  FMethodText.Highlighter := gxpPas;

  FCodeText := TGxEnhancedEditor.Create(Self);
  FCodeText.Name := 'edtCodeText';
  FCodeText.Parent := tshCode;
  FCodeText.Align := alClient;
  FCodeText.ReadOnly := True;
  FCodeText.Highlighter := gxpPas;
  FCodeText.ActiveLineColor := TGxEnhancedEditor.DefaultActiveLineColor;
end;

procedure TfmClassBrowser.ViewBrowserDetails;
var
  OInfo: TBrowseClassInfoCollection;
begin
  if not TryGetClassInfo(Oinfo) then
    Exit; //==>
  TfmClassProp.Execute(Self, OInfo);
end;

procedure TfmClassBrowser.actViewClassPropertiesExecute(Sender: TObject);
begin
  if (ActiveControl = tvBrowse) then
    ViewBrowserDetails;
end;

procedure TfmClassBrowser.actHelpHelpExecute(Sender: TObject);
begin
  GxContextHelp(Self, 5);
end;

procedure TfmClassBrowser.actHelpContentsExecute(Sender: TObject);
begin
  GxContextHelpContents(Self);
end;

procedure TfmClassBrowser.actHelpAboutExecute(Sender: TObject);
begin
  ShowGXAboutForm;
end;

procedure TfmClassBrowser.actOptionsOptionsExecute(Sender: TObject);
begin
  if TfmClassOptions.Execute(Self, tvBrowse.Font, lvInfo.Font, FCodeText.Font, FAutomaticallyHideBrowser,
    FPrimitiveTop, FStayInPackage, FParseRecursing, FFilters,
    FClassHierarchyFontSize, FClassHierarchyBoxWidth,
    FClassHierarchyBoxSpace, FClassHierarchyFont) then begin
    SaveSettings;
    FiltersToActions;
  end;
end;

procedure TfmClassBrowser.actFileExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfmClassBrowser.actFilePrinterSetupExecute(Sender: TObject);
begin
  dlgPrinterSetup.Execute;
end;

procedure TfmClassBrowser.actEditCopyExecute(Sender: TObject);
begin
  if (ActiveControl is TGxEnhancedEditor) then
    TGxEnhancedEditor(ActiveControl).CopyToClipboard;
end;

procedure TfmClassBrowser.SetInfoViewMode(const Value: TInfoViewMode);
begin
  tvBrowse.Selected := nil;

  if Value = FInfoViewMode then
    Exit;

  FInfoViewMode := Value;
  LoadAllObjects;
end;

procedure TfmClassBrowser.actViewListExecute(Sender: TObject);
begin
  SetInfoViewMode(vmList);
end;

procedure TfmClassBrowser.actViewTreeExecute(Sender: TObject);
begin
  SetInfoViewMode(vmTree);
end;

procedure TfmClassBrowser.actViewUnitNamesExecute(Sender: TObject);
begin
  FViewUnitNames := not FViewUnitNames;
  LoadAllObjects;
end;

procedure TfmClassBrowser.actEditGotoMemberExecute(Sender: TObject);
var
  Line: Integer;
  SourceFile: string;
  MInfo: TBrowseMethodInfoItem;
begin
  if (tvBrowse.Selected = nil) or (lvInfo.Selected = nil) then
    Exit;
  Line := 0;

  MInfo := TBrowseMethodInfoItem(lvInfo.Selected.Data);
  case MInfo.MethodType of
    ctConstant,
    ctType,
    ctVariable,
    ctProperty:    Line := MInfo.GetInterfaceLine;
    ctMethod:
      begin
        if MInfo.cAbstract then
          Line := MInfo.GetInterfaceLine
        else
          Line := MInfo.GetImplementationLine;
      end;
  end; // case
  Inc(Line);
  SourceFile := TBrowseClassInfoCollection(tvBrowse.Selected.Data).FileName;

  GxOtaGoToFileLine(SourceFile, Line);

  if FAutomaticallyHideBrowser then
    Self.Hide;
end;

procedure TfmClassBrowser.actEditGotoClassExecute(Sender: TObject);
var
  ClassInfos: TBrowseClassInfoCollection;
begin
  if not TryGetClassInfo(ClassInfos) then
    Exit; //==>

  GxOtaGoToFileLine(ClassInfos.FileName, ClassInfos.RefreshLineNo + 1);
  if FAutomaticallyHideBrowser then
    Self.Hide;
end;

procedure TfmClassBrowser.ActionsUpdate(Action: TBasicAction; var Handled: Boolean);
begin
  actEditCopy.Enabled := (ActiveControl is TGxEnhancedEditor);
  actEditGotoMember.Enabled := Assigned(lvInfo.Selected);
  actEditGotoClass.Enabled := Assigned(tvBrowse.Selected) and (TObject(tvBrowse.Selected.Data) is TBrowseClassInfoCollection);
  actEditFindNext.Enabled := FLastFind <> '';
  actViewClassProperties.Enabled := (actEditGotoClass.Enabled and tvBrowse.Focused);
  actFileAdd.Enabled := (ClassList.StoragePath <> '');
  actFileRemove.Enabled := Assigned(tvBrowse.Selected) and (tvBrowse.Selected.Level = 0);
  actViewList.Checked := (FInfoViewMode = vmList);
  actViewTree.Checked := (FInfoViewMode = vmTree);
  actViewUnitNames.Checked := FViewUnitNames;
  actViewProperties.Enabled := Assigned(tvBrowse.Selected) or Assigned(lvInfo.Selected);
end;

procedure TfmClassBrowser.tvBrowseMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node: TTreeNode;
begin
  // RightClickSelect in Delphi 5/6 is totally useless, so this is a workaround
  if Button = mbRight then
  begin
    Node := tvBrowse.GetNodeAt(X, Y);
    if Node <> nil then
      tvBrowse.Selected := Node;
  end;
end;

procedure TfmClassBrowser.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #27 then
  begin
    Key := #0;
    actFileExit.Execute;
  end;
end;

procedure TfmClassBrowser.actEditFindNextExecute(Sender: TObject);
var
  FindNode: TTreeNode;
begin
  FindNode := tvBrowse.Selected;
  if Assigned(FindNode) then
    FindNode := FindNode.GetNext;
  if (not Assigned(FindNode)) and (tvBrowse.Items.Count > 0) then
    FindNode := tvBrowse.Items[0];
  if Assigned(FindNode) then
    FindFromNode(FLastFind, FindNode);
end;

procedure TfmClassBrowser.tvBrowseDblClick(Sender: TObject);
begin
  actEditGotoClass.Execute;
end;

initialization
  RegisterGX_Expert(TClassBrowserExpert);
end.

