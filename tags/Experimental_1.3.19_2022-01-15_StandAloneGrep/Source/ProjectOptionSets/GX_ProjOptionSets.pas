// Original Author: John Hansen <John_Hansen@tcsmgmt.com>
unit GX_ProjOptionSets;

{$I GX_CondDefine.inc}

interface

uses
  Windows, SysUtils, Types, ToolsAPI, TypInfo, Classes, Controls, Forms, StdCtrls, ExtCtrls,
  CheckLst, Menus, ComCtrls, ToolWin, ActnList, Actions, UITypes,
  OmniXml,
  GX_SharedImages, GX_IdeDock, GX_Experts, GX_ConfigurationInfo, GX_CheckListBoxWithHints;

type
  TOptionValueFunc = function(const AOption: string): string of object;

  TfmProjOptionSets = class(TfmIdeDockForm)
    pmuPrjOptions: TPopupMenu;
    mniPrjClearAll: TMenuItem;
    mniPrjCheckAll: TMenuItem;
    mniPrjAscending: TMenuItem;
    mniPrjDescending: TMenuItem;
    mniPrjSortCheckedFirst: TMenuItem;
    mnuPrjResort: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    mniModifyPrjOptionValues: TMenuItem;
    pmuEnvOptions: TPopupMenu;
    mniModifyEnvOptionValues: TMenuItem;
    pcSettings: TPageControl;
    tabSets: TTabSheet;
    tabProject: TTabSheet;
    lblProjectSettings: TLabel;
    pnlCheckListHost: TPanel;
    tabEnvironment: TTabSheet;
    pnlCurrentSet: TPanel;
    ToolBar: TToolBar;
    Actions: TActionList;
    actNewSet: TAction;
    actDeleteSet: TAction;
    actSaveSets: TAction;
    actApplySet: TAction;
    actHelp: TAction;
    tbnNewSet: TToolButton;
    tbnDeleteSet: TToolButton;
    tbnSaveSets: TToolButton;
    tbnApplyToProject: TToolButton;
    tbnSep1: TToolButton;
    tbnHelp: TToolButton;
    pmuSets: TPopupMenu;
    mitPopAdd: TMenuItem;
    mitPopApply: TMenuItem;
    mitPopDelete: TMenuItem;
    mitPopSave: TMenuItem;
    mitPopHelp: TMenuItem;
    mitPopSep: TMenuItem;
    actRenameSet: TAction;
    mitPopRename: TMenuItem;
    pnlSets: TPanel;
    lstSets: TListBox;
    pnlEnvironment: TPanel;
    pnlFilterComboHost: TPanel;
    cbFilter: TComboBox;
    procedure lstSetsClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure mniPrjClearAllClick(Sender: TObject);
    procedure mniPrjCheckAllClick(Sender: TObject);
    procedure mniPrjDescendingClick(Sender: TObject);
    procedure pmuPrjOptionsPopup(Sender: TObject);
    procedure mniPrjSortCheckedFirstClick(Sender: TObject);
    procedure mnuPrjResortClick(Sender: TObject);
    procedure cbFilterChange(Sender: TObject);
    procedure mniModifyEnvOptionValuesClick(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure actNewSetExecute(Sender: TObject);
    procedure ActionsUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure actApplySetExecute(Sender: TObject);
    procedure actSaveSetsExecute(Sender: TObject);
    procedure actDeleteSetExecute(Sender: TObject);
    procedure actHelpExecute(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure actRenameSetExecute(Sender: TObject);
    procedure mniModifyPrjOptionValuesClick(Sender: TObject);
    procedure pnlFilterComboHostResize(Sender: TObject);
    procedure lstSetsDblClick(Sender: TObject);
  private
    lstPrjOptions: TCheckListBoxWithHints;
    lstEnvOptions: TCheckListBoxWithHints;
    FPrjSetOptions: TStringList;
    FEnvSetOptions: TStringList;
    FSetChanged: Boolean;
    FLastLoadedSet: string;
    FDom: IXMLDocument;
    FProjItemIndex: Integer;
    FEnvItemIndex: Integer;
    procedure InitializeForm;
    procedure lstEnvironmentOptClickCheck(Sender: TObject);
    procedure lstProjectOptClickCheck(Sender: TObject);
    procedure lstEnvironmentOptMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure lstProjectOptMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ProcessListboxCheckClick(Listbox: TCheckListBoxWithHints; Options: TStringList);
    procedure HandleOnGetHintPrj(Sender: TObject; const CursorPos: TPoint; var HintStr: string);
    procedure HandleOnGetHintEnv(Sender: TObject; const CursorPos: TPoint; var HintStr: string);
    procedure SetupListOptionsControls;
    procedure FillFilterBox;
    procedure LoadPrjOptionList;
    procedure LoadEnvOptionList;
    procedure LoadOptionSetList;
    procedure LoadStorageIntoDOM;
    procedure SaveDOMToStorageFile;
    procedure LoadSetOptions;
    procedure SaveSetOptions;
    procedure ApplySetOptions;
    procedure DeleteSetFromStorage;
    function  FindSetByName(const Name: string): IXMLElement;
    function  AddSetToDOM(const Name: string): IXMLElement;
    function  GetNodeAttributeValue(Element: IXMLElement; const Name: string): string;
    function GetVariantValueAsString(AValue: Variant; AKind: TTypeKind): string;
    function GetPrjOptionValue(_PrjOptions: IOTAProjectOptions; const _Option: string): string;
    function GetEnvOptionValue(_EnvOptions: IOTAEnvironmentOptions; const _Option: string): string;
    procedure SetPrjOptionValue(_PrjOptions: IOTAProjectOptions; const _Option, _Value: string);
    function GetEnvironmentOptions: IOTAEnvironmentOptions;
    function GetCurrentSetName: string;
    procedure LoadSettings;
    procedure SaveSettings;
    procedure RefreshCheckmarks(_clb: TCheckListBoxWithHints; _Options: TStringList);
    procedure AddNewOptionSet(const SetName: string);
    function HaveSelectedSet: Boolean;
  protected
{$IFDEF IDE_IS_HIDPI_AWARE}
    procedure ApplyDpi(_NewDpi: Integer; _NewBounds: PRect); override;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

uses
  {$IFOPT D+} GX_DbugIntf, Clipbrd, {$ENDIF}
  Variants, Messages, Dialogs,
  GX_GxUtils, GX_GenericUtils, GX_IdeUtils, GX_OtaUtils,
  GX_VerDepConst, GX_ProjOptMap, GX_XmlUtils,
  u_dzVclUtils, u_dzClassUtils, GX_GExperts;

resourcestring
  SOptValue = '%s value';
  SOptSaved = 'Saved: ';
  SOptCurrent = 'Current: ';
  SValUnknown = 'unknown';
  SCurrentSet = 'Current Set: ';

const
  ROOT_NODE    = 'ProjectOptionSets';
  SET_NODE     = 'Set';
  ATTR_NODE    = 'Name';
  ENV_OPT_NODE = 'EnvironmentOptions';
  PRJ_OPT_NODE = 'ProjectOptions';
  OPT_NODE     = 'Option';

  {
    XML file structure:
    <ProjectOptionSets>
      <Set Name="SetName">
        <ProjectOptions>
          <Option Name="OptionName">OptionValue</Option>
          <Option Name="OptionName2">OptionValue2</Option>
        </ProjectOptions>
        <EnvironmentOptions>
          <Option Name="OptionName3">OptionValue3</Option>
          <Option Name="OptionName4">OptionValue4</Option>
        </EnvironmentOptions>
      </Set>
      <Set Name="SetName2">
      </Set>
    </ProjectOptionSets>
  }

type
  TProjOptionSetsExpert = class(TGX_Expert)
  private
    function GetStorageFile: string;
  protected
    procedure UpdateAction(Action: TCustomAction); override;
    procedure InternalLoadSettings(_Settings: IExpertSettings); override;
    procedure SetActive(New: Boolean); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetActionCaption: string; override;
    class function GetName: string; override;
    procedure Execute(Sender: TObject); override;
    property StorageFile: string read GetStorageFile;
    function HasConfigOptions: Boolean; override;
  end;

var
  fmProjOptionSets: TfmProjOptionSets;
  PrjOptSetsExpert: TProjOptionSetsExpert;

type
  TOptionDesc = class
  private
    FOptionKind: TTypeKind;
    FOptionIdx: integer;
  public
    constructor Create(_OptionKind: TTypeKind; _OptionIdx: Integer = -1);
    property OptionIdx: Integer read FOptionIdx;
    property OptionKind: TTypeKind read FOptionKind;
  end;

{ TOptionDesc }

constructor TOptionDesc.Create(_OptionKind: TTypeKind; _OptionIdx: Integer);
begin
  inherited Create;
  FOptionKind := _OptionKind;
  FOptionIdx := _OptionIdx;
end;

{ TProjOptionSetsExpert }

procedure TProjOptionSetsExpert.Execute(Sender: TObject);
begin
  if fmProjOptionSets = nil then
    fmProjOptionSets := TfmProjOptionSets.Create(nil);
  SetFormIcon(fmProjOptionSets);
  IdeDockManager.ShowForm(fmProjOptionSets);

  IncCallCount;
end;

constructor TProjOptionSetsExpert.Create;
begin
  inherited Create;

  FreeAndNil(PrjOptSetsExpert);
  PrjOptSetsExpert := Self;
end;

destructor TProjOptionSetsExpert.Destroy;
begin
  FreeAndNil(fmProjOptionSets);

  PrjOptSetsExpert := nil;

  inherited Destroy;
end;

function TProjOptionSetsExpert.GetActionCaption: string;
resourcestring
  SProjOptionsMenuCaption = 'Project &Option Sets';
begin
  Result := SProjOptionsMenuCaption;
end;

class function TProjOptionSetsExpert.GetName: string;
begin
  Result := 'ProjOptionSets'; // Do not localize.
end;

function TProjOptionSetsExpert.GetStorageFile: string;
begin
  Result := AddSlash(ConfigInfo.GetConfigPath) +
    'ProjectOptionSets-' + IdeProductName + MajorVersionNumberChar + '.xml';
end;

procedure TProjOptionSetsExpert.InternalLoadSettings(_Settings: IExpertSettings);
begin
  inherited InternalLoadSettings(_Settings);
  // This procedure is only called once so it is safe to register the form here.
  if Active then
    IdeDockManager.RegisterDockableForm(TfmProjOptionSets,
      fmProjOptionSets, 'fmProjOptionSets');
end;

procedure TProjOptionSetsExpert.SetActive(New: Boolean);
begin
  if New <> Active then
  begin
    inherited SetActive(New);
    if New then //FI:W505
      // Nothing to initialize here
    else
      FreeAndNil(fmProjOptionSets)
  end;
end;

procedure TProjOptionSetsExpert.UpdateAction(Action: TCustomAction);
begin
  Action.Enabled := GxOtaHaveCurrentProject;
end;

function TProjOptionSetsExpert.HasConfigOptions: Boolean;
begin
  Result := False;
end;

{ TfmProjOptionSets }

constructor TfmProjOptionSets.Create(AOwner: TComponent);
resourcestring
  SModifyOptionValuesMenuCaption = '&Modify Option Values...';
var
  MainForm: TCustomForm;
  ProjectOptions: TComponent;
begin
  inherited Create(AOwner);

  TControl_SetMinConstraints(Self);

  FLastLoadedSet := '';
  SetupListOptionsControls;
  FillFilterBox;

  lstPrjOptions.SortCheckedFirst := True;
  lstEnvOptions.SortCheckedFirst := True;

  InitializeForm;

  LoadPrjOptionList;
  LoadEnvOptionList;

  FDom := nil; // start with nil DOM
  FPrjSetOptions := TStringList.Create;
  FEnvSetOptions := TStringList.Create;

  LoadSettings;

  // Note: Optimally, we would go through IOTAOptions.EditOptions on our
  //   IOTAProjectOptions, but this fails when no project is loaded
  //   The TAction will work always, in particular for default project
  //   options, when there is no project loaded in the IDE.
  MainForm := GetIdeMainForm;
  if Assigned(MainForm) then
  begin
    ProjectOptions := MainForm.FindComponent('ProjectOptionsCommand');
    if (ProjectOptions <> nil) and (ProjectOptions is TBasicAction) then
    begin
      mniModifyPrjOptionValues.Action := ProjectOptions as TBasicAction;
      mniModifyPrjOptionValues.Caption := SModifyOptionValuesMenuCaption;
    end;
  end;
  if mniModifyPrjOptionValues.Action = nil then
    mniModifyPrjOptionValues.Enabled := False;

  // Open the XML storage file and load it into the DOM
  LoadStorageIntoDOM;
  // Load the saved list of project option sets
  LoadOptionSetList;
end;

destructor TfmProjOptionSets.Destroy;
begin
  SaveSettings;
  TStrings_FreeAllObjects(lstPrjOptions.Items).Clear;
  TStrings_FreeAllObjects(lstEnvOptions.Items).Clear;
  // Now free our objects
  FDom := nil;
  FreeAndNil(FPrjSetOptions);
  FreeAndNil(FEnvSetOptions);
  
  inherited Destroy;

  fmProjOptionSets := nil;
end;

{$IFDEF IDE_IS_HIDPI_AWARE}
procedure TfmProjOptionSets.ApplyDpi(_NewDpi: Integer; _NewBounds: PRect);
var
  il: TImageList;
begin
  inherited;
  il := GExpertsInst.GetScaledSharedDisabledImages(_NewDpi);
  ToolBar.DisabledImages := il;

  il := GExpertsInst.GetScaledSharedImages(_NewDpi);
  ToolBar.Images := il;
  Actions.Images := il;
  pmuPrjOptions.Images := il;
  pmuEnvOptions.Images := il;
  pmuSets.Images := il;
end;
{$ENDIF}

procedure TfmProjOptionSets.lstSetsClick(Sender: TObject);
begin
  if HaveSelectedSet then
  begin
    if GetCurrentSetName <> FLastLoadedSet then
    begin
      LoadSetOptions;
      FLastLoadedSet := GetCurrentSetName;
      FSetChanged := False;
    end;
  end;
end;

procedure TfmProjOptionSets.lstSetsDblClick(Sender: TObject);
begin
  actApplySet.Execute;
end;

procedure TfmProjOptionSets.AddNewOptionSet(const SetName: string);
var
  ItemPos: Integer;
begin
  ItemPos := lstSets.Items.IndexOf(SetName);
  if ItemPos = -1 then
  begin
    ItemPos := lstSets.Items.Add(SetName);
    // Update our set list
    AddSetToDOM(SetName);
  end;
  lstSets.ItemIndex := ItemPos;
  lstSetsClick(lstSets);
  FSetChanged := True;
end;

resourcestring
  SAllOptions = 'All options';

procedure TfmProjOptionSets.LoadPrjOptionList;
{$IFOPT D+}
{$DEFINE CheckProjectOptionMap}
{$ENDIF}
{$IFDEF CheckProjectOptionMap}
const
  MissingEntryFormat =
'    (' +sLineBreak+
'      Name: ''%s'';' +sLineBreak+
'      AssumedTypeKind: %s;' +sLineBreak+
'      Description: ''%s'';' +sLineBreak+
'      Categories: [ocUnknown];' +sLineBreak+
'      Translator: GxStringOptionTranslator;' +sLineBreak+
'    ),' +sLineBreak;
{$ENDIF CheckProjectOptionMap}
var
  j: Integer;
  OptionNames: TOTAOptionNameArray;
  tmpObj: TOptionDesc;
  NameOfOption: string;
  Items: TStringList;
  PrjOptions: IOTAProjectOptions;
{$IFDEF CheckProjectOptionMap}
  MissingEntries: string;
{$ENDIF CheckProjectOptionMap}
  OptionIdx: Integer;
begin
  TStrings_FreeAllObjects(lstPrjOptions.Items).Clear;
  PrjOptions := GxOtaGetActiveProjectOptions;
  if Assigned(PrjOptions) then begin
    OptionNames := nil;
    Items := nil;
    try
      Items := TStringList.Create;
      OptionNames := PrjOptions.GetOptionNames;
      for j := Low(OptionNames) to High(OptionNames) do begin
        NameOfOption := OptionNames[j].Name;
        OptionIdx := GetOptionNameIndex(NameOfOption);
      {$IFDEF CheckProjectOptionMap}
        if OptionIdx < 0 then
        begin
          // Add missing project option to list
          MissingEntries := MissingEntries + Format(MissingEntryFormat,
            [NameOfOption, GetEnumName(TypeInfo(TTypeKind), Integer(OptionNames[j].Kind)), NameOfOption]);
        end
        else
        begin
          // Sanity check: do we handle the IDE's option
          // with the right type ourselves?
          if GxOptionsMap[OptionIdx].AssumedTypeKind <> tkUnknown then
            Assert(GxOptionsMap[OptionIdx].AssumedTypeKind = OptionNames[j].Kind, 'Wrong type kind for option ' + GxOptionsMap[OptionIdx].Name);
        end;
      {$ENDIF CheckProjectOptionMap}

        // Load options honoring filter selection
        if ((cbFilter.Text = sAllOptions) or
            (CategoryTextToCategory(cbFilter.Text) in GetOptionCategories(NameOfOption))
           ) and
           OptionIsAppropriateForIde(GetOptionCategories(NameOfOption)) then
        begin
          tmpObj := TOptionDesc.Create(OptionNames[j].Kind, OptionIdx);
          Items.AddObject(NameOfOption, tmpObj);
        end;
        // Done loading option list
      end;
      lstPrjOptions.Items := Items;

    {$IFDEF CheckProjectOptionMap}
      if MissingEntries <> '' then
      begin
        Clipboard.AsText := MissingEntries;
        // Do not localize; this is just test code
        MessageDlg('There are missing project options in GX_ProjOptMap.pas:' +#13#10+ MissingEntries, mtWarning, [mbOK], 0);
      end;
    {$ENDIF CheckProjectOptionMap}
    finally
      FreeAndNil(Items);
      PrjOptions := nil;
    end;
  end;
end;

procedure TfmProjOptionSets.LoadEnvOptionList;
var
  OptionNames: TOTAOptionNameArray;
  Items: TStringList;

  {$IFOPT D+}
    procedure DumpAllOptionNamesToDebugWindow;
    var
      i: Integer;
    begin
      SendDebug('Dumping all environment option names from IDE list:');
      for i := Low(OptionNames) to High(OptionNames) do
        SendDebug(OptionNames[i].Name);
    end;
  {$ENDIF D+}

    procedure AddEnvOptionItem(const OptionName: string);
    var
      i: Integer;
      tmpObj: TOptionDesc;
      EnvOption: TOTAOptionName;
    begin
      i := High(OptionNames);
      while i >= Low(OptionNames) do
      begin
        if OptionNames[i].Name = OptionName then  // Case-sensitive comparison
        begin
          EnvOption := OptionNames[i];
          Break;
        end;
        Dec(i);
      end;

      //  Assert(i >= Low(OptionNames), 'Could not find environment option ' + OptionName);
      if i >= Low(OptionNames) then
      begin
        tmpObj := TOptionDesc.Create(EnvOption.Kind);
        try
          Items.AddObject(EnvOption.Name, tmpObj);
        except
          on E: Exception do
          begin
            FreeAndNil(tmpObj);
            raise;
          end;
        end;
      end;
    end;

var
  EnvOptions: IOTAEnvironmentOptions;
begin
  TStrings_FreeAllObjects(lstEnvOptions.Items).Clear;

  EnvOptions := GetEnvironmentOptions;
  if Assigned(EnvOptions) then begin
    try
      Items := TStringList.Create;
      try
        OptionNames := EnvOptions.GetOptionNames;
      except
        SetLength(OptionNames, 0); // Delphi 9 does not support these correctly, so ignore them
      end;
      if not Assigned(OptionNames) then
        Exit;

      {$IFOPT D+}
        // DumpAllOptionNamesToDebugWindow;
      {$ENDIF D+}

      // Note: we MANUALLY add to this list
      // as globally adding with all environment
      // options would pollute the list substantially
      // with irrelevant options and hence reduce
      // the usability of this feature.

      // The goal is to identify options that are
      // useful for individual project management.
      // Add when/where necessary

      AddEnvOptionItem('LibraryPath');
      AddEnvOptionItem('DotNetLibraryPath');
      AddEnvOptionItem('CppSearchPath');

      AddEnvOptionItem('BrowsingPath');
      AddEnvOptionItem('DotNetBrowsingPath');
      AddEnvOptionItem('CppBrowsingPath');

      AddEnvOptionItem('RepositoryDir');

      AddEnvOptionItem('PackageSearchPath');
      AddEnvOptionItem('PackageDPLOutput');
      AddEnvOptionItem('PackageDCPOutput');

      AddEnvOptionItem('DeclarationInformation');
      AddEnvOptionItem('ScopeSort');

      AddEnvOptionItem('WarnOnPackageRebuild');

      AddEnvOptionItem('StepProgramBlock');

      AddEnvOptionItem('DFMAsText');
      AddEnvOptionItem('AutoCreateForms');

      lstEnvOptions.Items := Items;
    finally
      FreeAndNil(Items);
      EnvOptions := nil;
    end;
  end;
end;

procedure TfmProjOptionSets.FormShow(Sender: TObject);
begin
  // Add a default, empty set if none exist
  if lstSets.Items.Count = 0 then
    AddNewOptionSet('Default');

  // If we (un)dock, sort order is lost and we need to resort
  lstPrjOptions.BeginUpdate;
  lstEnvOptions.BeginUpdate;

  // Load project options in case they have not
  // been available yet; this takes care of the
  // case where the form is shown when no project
  // is open. Without these two lines, after doing
  // the above, it would be impossible to use the
  // expert, as no options would ever be re-retrieved.
  LoadPrjOptionList;
  LoadEnvOptionList;

  RefreshCheckmarks(lstPrjOptions, FPrjSetOptions);
  RefreshCheckmarks(lstEnvOptions, FEnvSetOptions);
  // Set item index in lstSets
  if lstSets.Items.Count > 0 then
    lstSets.ItemIndex := 0;
  lstSetsClick(lstSets);
end;

procedure TfmProjOptionSets.LoadOptionSetList;
var
  Nodes: IXMLNodeList;
  SetName: string;
  i: Integer;
  Node: IXMLNode;
begin
  Assert(lstSets.Items.Count = 0, 'Set list already populated?');
  // Load list of sets from our DOM
  Nodes := FDom.documentElement.ChildNodes;
  for i := 0 to Nodes.Length - 1 do
  begin
    // Each node is a Set (maybe)
    Node := Nodes.Item[i];
    if Node.NodeType <> ELEMENT_NODE then
      Continue;
    SetName := GetNodeAttributeValue(Node as IXMLElement, ATTR_NODE);
    if SetName = '' then
      Continue;
    lstSets.Items.Add(SetName);
  end;
end;

procedure TfmProjOptionSets.ProcessListboxCheckClick(Listbox: TCheckListBoxWithHints; Options: TStringList);
var
  Index, TheItemIndex: Integer;
  ItemString: string;
begin
  TheItemIndex := Listbox.ItemIndex;
  //{$IFOPT D+} SendDebug('Current Options: ' + Options.Text);  {$ENDIF}
  FSetChanged := True;
  Assert(TheItemIndex > -1, 'Check event with no selected item');
  ItemString := Listbox.Items[TheItemIndex];
  Index := Options.IndexOfName(ItemString);
  if Listbox.Checked[TheItemIndex] then
  begin // Add a new checked item to the stored list
    Assert(Index = -1, 'Check event for item in Options');
    ItemString := ItemString + '=['+SValUnknown+']';
    Options.Add(ItemString);
  end
  else
  begin // Remove an item from the stored list
    Assert(Index > -1, 'Uncheck event for item not in Options');
    Options.Delete(Index);
  end;
end;

procedure TfmProjOptionSets.lstEnvironmentOptClickCheck(Sender: TObject);
begin
  if lstEnvOptions.ItemIndex <> FEnvItemIndex then
    lstEnvOptions.ItemIndex := FEnvItemIndex;
  ProcessListboxCheckClick(lstEnvOptions, FEnvSetOptions);
end;

procedure TfmProjOptionSets.lstProjectOptClickCheck(Sender: TObject);
begin
  if lstPrjOptions.ItemIndex <> FProjItemIndex then
    lstPrjOptions.ItemIndex := FProjItemIndex;
  ProcessListboxCheckClick(lstPrjOptions, FPrjSetOptions);
end;

procedure TfmProjOptionSets.LoadSetOptions;
var
  SetNode: IXMLElement;

  procedure LoadTheOptions(List: TStringList; const NodeName: string);
  var
    Nodes: IXMLNodeList;
    NumOptions, i: Integer;
    OptionNode: IXMLNode;
    Container: IXMLElement;
    Option: string;
  begin
    Nodes := SetNode.SelectNodes(NodeName);
    // It is possible that the container node (ProjectOptions & EnvironmentOptions)
    // do not yet exist.
    if Nodes.Length > 0 then
    begin
      Container := (Nodes.Item[0] as IXMLElement);
      Assert(Container.NodeType = ELEMENT_NODE, 'Invalid storage file structure');
      Nodes := Container.SelectNodes('Option');
      NumOptions := Nodes.Length;
      Assert((NumOptions >= 0) and (NumOptions <= Length(GxOptionsMap)), 'Corrupt options storage file: ' + IntToStr(NumOptions));
      for i := 0 to NumOptions - 1 do
      begin
        // Now build Option string
        OptionNode := Nodes.Item[i];
        if OptionNode.NodeType <> ELEMENT_NODE then
          Continue;
        Option := GetNodeAttributeValue(OptionNode as IXMLElement, ATTR_NODE);
        if Option = '' then Continue;
        Option := Option + '=' + OptionNode.Text;
        List.Add(Option);
      end;
    end;
  end;

begin
  // Load options from our DOM
  FPrjSetOptions.Clear;
  FEnvSetOptions.Clear;

  SetNode := FindSetByName(GetCurrentSetName);
  if SetNode <> nil then
  begin
    // After we've found our set, load the ProjectOptions and then the EnvironmentOptions
    LoadTheOptions(FPrjSetOptions, PRJ_OPT_NODE);
    LoadTheOptions(FEnvSetOptions, ENV_OPT_NODE);
  end;

  RefreshCheckmarks(lstPrjOptions, FPrjSetOptions);
  RefreshCheckmarks(lstEnvOptions, FEnvSetOptions);
  FSetChanged := False;
end;

procedure TfmProjOptionSets.ApplySetOptions;
var
  j: Integer;
  OptionName: string;
  PrjOptions: IOTAProjectOptions;
  EnvOptions: IOTAEnvironmentOptions;
begin
  // This method uses FPrjSetOptions and updates the ProjectOptions
  PrjOptions := GxOtaGetActiveProjectOptions;
  try
    if Assigned(PrjOptions) then
    begin
      for j := 0 to FPrjSetOptions.Count - 1 do
      begin
        OptionName := FPrjSetOptions.Names[j];
        Assert(Trim(OptionName) <> '', 'Empty option name in ApplySetOptions: ' + OptionName);
        SetPrjOptionValue(PrjOptions, OptionName, FPrjSetOptions.Values[OptionName]);
      end;
    end;
  finally
    PrjOptions := nil;
  end;

  // Now update the environment options also
  EnvOptions := GetEnvironmentOptions;
  try
    if Assigned(EnvOptions) then
    begin
      for j := 0 to FEnvSetOptions.Count - 1 do
      begin
        OptionName := FEnvSetOptions.Names[j];
        EnvOptions.Values[OptionName] := FEnvSetOptions.Values[OptionName];
      end;
    end;
  finally
    EnvOptions := nil;
  end;
end;

procedure TfmProjOptionSets.SaveSetOptions;
var
  Container: IXMLElement;

  function AddOption(const OptionName, OptionValue: string): IXMLElement;
  begin
    Result := FDom.CreateElement(OPT_NODE);
    Result.Text := OptionValue;
    Result.SetAttribute(ATTR_NODE, OptionName);
    Container.AppendChild(Result);
  end;

var
  SetNode: IXMLElement;
  i: Integer;
  PrjOptions: IOTAProjectOptions;
  EnvOptions: IOTAEnvironmentOptions;
  OptionName: string;
  OptionValue: string;
begin
  // Save the current set to our DOM
  SetNode := FindSetByName(GetCurrentSetName);
  if SetNode <> nil then
    SetNode.ChildNodes.Clear
  else begin
    // If the set node doesn't exist at all then we need to create it
    SetNode := AddSetToDOM(GetCurrentSetName);
  end;

  Container := FDom.CreateElement(PRJ_OPT_NODE);
  SetNode.AppendChild(Container);

  PrjOptions := GxOtaGetActiveProjectOptions;
  try
    if Assigned(PrjOptions) then
    begin
      // Iterate over FPrjSetOptions due to filtering
      for i := 0 to FPrjSetOptions.Count - 1 do begin
        OptionName := FPrjSetOptions.Names[i];
        OptionValue := GetPrjOptionValue(PrjOptions, OptionName);
        AddOption(OptionName, OptionValue);
      end;
    end;
  finally
    PrjOptions := nil;
  end;

  Container := FDom.CreateElement(ENV_OPT_NODE);
  SetNode.AppendChild(Container);

  EnvOptions := GetEnvironmentOptions;
  try
    if Assigned(EnvOptions) then
    begin
      for i := 0 to lstEnvOptions.Items.Count - 1 do
      begin
        if lstEnvOptions.Checked[i] then begin
          OptionName := lstEnvOptions.Items[i];
          OptionValue := GetEnvOptionValue(EnvOptions, OptionName);
          AddOption(OptionName, OptionValue);
        end;
      end;
    end;
  finally
    EnvOptions := nil;
  end;

  RefreshCheckmarks(lstPrjOptions, FPrjSetOptions);
  RefreshCheckmarks(lstEnvOptions, FEnvSetOptions);
  FSetChanged := False;
end;

procedure TfmProjOptionSets.DeleteSetFromStorage;
var
  SetNode: IXMLNode;
begin
  // Delete a set from our DOM (GetCurrentSetName)
  SetNode := FindSetByName(GetCurrentSetName);
  if SetNode <> nil then
    SetNode.ParentNode.RemoveChild(SetNode);
end;

function TfmProjOptionSets.AddSetToDOM(const Name: string): IXMLElement;
begin
  Assert(not Assigned(FindSetByName(Name)));
  Result := FDom.CreateElement(SET_NODE);
  FDom.documentElement.AppendChild(Result);
  Result.SetAttribute(ATTR_NODE, Name);
end;

function TfmProjOptionSets.GetVariantValueAsString(AValue: Variant; AKind: TTypeKind): string;
begin
  case AKind of
    tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
    tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString, tkVariant,
    tkArray, tkRecord, tkInterface, tkInt64, tkDynArray {$IFDEF GX_VER200_up}, tkUString {$ENDIF}:
      Result := VarAsType(AValue, varString);
  else
    {$IFOPT D+} SendDebug('Unknown value passed to GetValueAsString');  {$ENDIF}
    Result := SValUnknown;
  end;
end;

procedure TfmProjOptionSets.mniPrjClearAllClick(Sender: TObject);
var
  i: Integer;
  Index: Integer;
begin
  TCheckListBox_UncheckAll(lstPrjOptions);
  for i := 0 to lstPrjOptions.Items.Count - 1 do
  begin
    Index := FPrjSetOptions.IndexOfName(lstPrjOptions.Items[i]);
    if Index > -1 then
      FPrjSetOptions.Delete(Index);
  end;
  FSetChanged := True;
end;

procedure TfmProjOptionSets.mniPrjCheckAllClick(Sender: TObject);
var
  i: Integer;
  Index: Integer;
  s: string;
begin
  TCheckListBox_CheckAll(lstPrjOptions);
  for i := 0 to lstPrjOptions.Count - 1 do
  begin
    s := lstPrjOptions.Items[i];
    Assert(Trim(s) <> '', 'Empty option name in mniPrjCheckAllClick');
    Index := FPrjSetOptions.IndexOfName(s);
    if Index = -1 then
      FPrjSetOptions.Add(s + '=['+SValUnknown+']');
  end;
  FSetChanged := True;
end;

procedure TfmProjOptionSets.SetupListOptionsControls;
begin
  lstPrjOptions := TCheckListBoxWithHints.Create(Self);
  lstPrjOptions.Align := alClient;
  lstPrjOptions.Parent := pnlCheckListHost;
  lstPrjOptions.PopupMenu := pmuPrjOptions;
  lstPrjOptions.OnClickCheck := lstProjectOptClickCheck;
  lstPrjOptions.OnMouseDown  := lstProjectOptMouseDown;
  lstPrjOptions.ShowHint := True;
  lstPrjOptions.OnGetHint := HandleOnGetHintPrj;
  // Activate this to update the hint as the mouse moves, without
  // having to select the listbox item under the cursor
  //TCheckListBoxWithHints(lstPrjOptions).OnMouseMove := lstOptionsMouseMove;

  lstEnvOptions := TCheckListBoxWithHints.Create(Self);
  lstEnvOptions.Align := alClient;
  lstEnvOptions.Parent := pnlEnvironment;
  lstEnvOptions.PopupMenu := pmuEnvOptions;
  lstEnvOptions.OnClickCheck := lstEnvironmentOptClickCheck;
  lstEnvOptions.OnMouseDown  := lstEnvironmentOptMouseDown;
  lstEnvOptions.ShowHint := True;
  lstEnvOptions.OnGetHint := HandleOnGetHintEnv;
  // Activate this to update the hint as the mouse moves, without
  // having to select the listbox item under the cursor
  //TCheckListBoxWithHints(lstEnvOptions).OnMouseMove := lstOptionsMouseMove;
end;

procedure TfmProjOptionSets.FillFilterBox;
var
  i: TGxOptionCategory;
begin
  cbFilter.Items.Add(SAllOptions);
  cbFilter.ItemIndex := 0;

  for i := Low(TGxOptionCategory) to High(TGxOptionCategory) do
  begin
    if OptionCategoryIsAppropriateForIde(i) then
      cbFilter.Items.AddObject(GxOptionsCategoryText[i], Pointer(i));
  end;
end;

function TfmProjOptionSets.GetEnvOptionValue(_EnvOptions: IOTAEnvironmentOptions; const _Option: string): string;
var
  idx: Integer;
  tmpObj: TOptionDesc;
begin
  Result := '';
  if Assigned(_EnvOptions) then
  begin
    idx := lstEnvOptions.Items.IndexOf(_Option);
    if idx <> -1 then
    begin
      tmpObj := TOptionDesc(lstEnvOptions.Items.Objects[idx]);
      Result := GetVariantValueAsString(_EnvOptions.Values[_Option], tmpObj.OptionKind);
    end;
  end;
end;

function TfmProjOptionSets.GetEnvironmentOptions: IOTAEnvironmentOptions;
begin
  Result := GxOtaGetIDEServices.GetEnvironmentOptions;
end;

procedure ProcessTranslatedValueForLineBreaks(var Value: string);
const
  MaxCols = 30;
begin
  // If our value string does not contain any
  // line breaks, insert them manually.
  // Let's assume that we are using paths
  // and that the semicolon is our preferred
  // delimiter - so break at the semicolon.
  Value := WrapText(Value, sLineBreak, [PathSep], MaxCols);
end;

procedure TfmProjOptionSets.HandleOnGetHintPrj(Sender: TObject; const CursorPos: TPoint; var HintStr: string);
var
  idx: Integer;
  OptionName: string;
  TranslatedValueString: string;
  PrjOptions: IOTAProjectOptions;
  tmpObj: TOptionDesc;
  OptionIdx: Integer;
begin
  HintStr := '';
  // HintStr := 'ItemIndex = ' + IntToStr(lstPrjOptions.ItemIndex);

  // By default display hint for that
  // item over which the cursor hovers.
  idx := lstPrjOptions.ItemAtPos(CursorPos, True);
  if not (idx >= 0) then
    idx := lstPrjOptions.ItemIndex;

  if idx >= 0 then
  begin
    OptionName := lstPrjOptions.Items[idx];
    tmpObj := TOptionDesc(lstPrjOptions.Items.Objects[Idx]);
    if Assigned(tmpObj) then
      OptionIdx := tmpObj.OptionIdx
    else
      OptionIdx :=  -1;
    HintStr := GxOptionsMap[tmpObj.OptionIdx].Description;
    if HintStr <> '' then
      HintStr := HintStr + sLineBreak;
    HintStr := HintStr + Format(SOptValue, [OptionName]);
    if lstPrjOptions.Checked[idx] and (FPrjSetOptions.IndexOfName(OptionName) <> -1) then
    begin
      HintStr := HintStr + sLineBreak;
      // Get the saved value
      TranslatedValueString := TranslatedValue(OptionIdx, FPrjSetOptions.Values[OptionName]);
      ProcessTranslatedValueForLineBreaks(TranslatedValueString);
      HintStr := HintStr + SOptSaved + TranslatedValueString;
    end;
    HintStr := HintStr + sLineBreak;
    // Get current value
    PrjOptions := GxOtaGetActiveProjectOptions;
    try
      TranslatedValueString := TranslatedValue(OptionIdx, GetPrjOptionValue(PrjOptions, OptionName));
      ProcessTranslatedValueForLineBreaks(TranslatedValueString);
      HintStr := HintStr + SOptCurrent + TranslatedValueString;
    finally
      PrjOptions := nil;
    end;
  end;
end;

procedure TfmProjOptionSets.HandleOnGetHintEnv(Sender: TObject; const CursorPos: TPoint; var HintStr: string);
var
  idx: Integer;
  OptionName: string;
  TranslatedValueString: string;
  EnvOptions: IOTAEnvironmentOptions;
begin
  HintStr := '';
  // HintStr := 'ItemIndex = ' + IntToStr(lstEnvOptions.ItemIndex);

  // By default display hint for that
  // item over which the cursor hovers.
  idx := lstEnvOptions.ItemAtPos(CursorPos, True);
  if not (idx >= 0) then
    idx := lstEnvOptions.ItemIndex;

  if idx <> -1 then
  begin
    OptionName := lstEnvOptions.Items[idx];

    //HintStr := GetEnvOptionDescription(OptionName);
    HintStr := OptionName;

    if HintStr <> '' then
      HintStr := HintStr + sLineBreak;

    HintStr := HintStr + Format(SOptValue, [OptionName]);

    if lstEnvOptions.Checked[idx] and
       (FEnvSetOptions.IndexOfName(OptionName) <> -1) then
    begin
      HintStr := HintStr + sLineBreak;
      // Get the saved value.  Note that translation always fails, since
      // we don't have maps to translators for any environment options
      TranslatedValueString :=
        TranslatedValue(OptionName, FEnvSetOptions.Values[OptionName]);
      ProcessTranslatedValueForLineBreaks(TranslatedValueString);
      HintStr := HintStr + SOptSaved + TranslatedValueString;
    end;

    HintStr := HintStr + sLineBreak;

    // Get current value
    EnvOptions := GetEnvironmentOptions;
    try
      TranslatedValueString := TranslatedValue(OptionName, GetEnvOptionValue(EnvOptions, OptionName));
      ProcessTranslatedValueForLineBreaks(TranslatedValueString);
      HintStr := HintStr + SOptCurrent + TranslatedValueString;
    finally
      EnvOptions := nil;
    end;
  end;
end;

procedure TfmProjOptionSets.SaveSettings;
var
  Settings: IExpertSettings;
begin
  // Do not localize any of the below items
  Settings := TProjOptionSetsExpert.GetSettings;
  Settings.SaveForm('Window', Self, [fsSize]);
end;

procedure TfmProjOptionSets.LoadSettings;
var
  Settings: IExpertSettings;
begin
  // Do not localize any of the below items
  Settings := TProjOptionSetsExpert.GetSettings;
  Settings.LoadForm('Window', Self, [fsSize]);
end;

procedure TfmProjOptionSets.mniPrjSortCheckedFirstClick(Sender: TObject);
begin
  lstPrjOptions.SortCheckedFirst := TMenuItem(Sender).Checked;
  lstPrjOptions.Resort;
end;

procedure TfmProjOptionSets.mnuPrjResortClick(Sender: TObject);
begin
  lstPrjOptions.Resort;
end;

procedure TfmProjOptionSets.mniPrjDescendingClick(Sender: TObject);
begin
  lstPrjOptions.SortAscending := (Sender = mniPrjAscending);
  lstPrjOptions.Resort;
end;

procedure TfmProjOptionSets.pmuPrjOptionsPopup(Sender: TObject);
begin
  if lstPrjOptions.SortAscending then
    mniPrjAscending.Checked := True
  else
    mniPrjDescending.Checked := True;

  mniPrjSortCheckedFirst.Checked := lstPrjOptions.SortCheckedFirst
end;

procedure TfmProjOptionSets.cbFilterChange(Sender: TObject);
begin
  lstPrjOptions.BeginUpdate;

  LoadPrjOptionList;
  RefreshCheckmarks(lstPrjOptions, FPrjSetOptions);
  lstPrjOptions.Resort;
end;

procedure TfmProjOptionSets.RefreshCheckmarks(_clb: TCheckListBoxWithHints; _Options: TStringList);
var
  i, j: Integer;
  Items: TStrings;
begin
  if not Assigned(_Options) then
    Exit;
  // Now iterate over lstPrjOptions and check any items in FPrjSetOptions
  Items := _clb.Items;
  for i := 0 to Items.Count - 1 do
  begin
    j := _Options.IndexOfName(Items[i]);
    _clb.Checked[i] := (j > -1);
  end;
  if _clb.SortCheckedFirst then begin
    // if checked items should be treated separately, we need to resort
    _clb.NeedsSorting := True;
  end;
end;

procedure TfmProjOptionSets.mniModifyEnvOptionValuesClick(Sender: TObject);
var
  IEnvironmentOptions: IOTAEnvironmentOptions;
begin
  IEnvironmentOptions := GetEnvironmentOptions;
  if Assigned(IEnvironmentOptions) then
    IEnvironmentOptions.EditOptions;
end;

procedure TfmProjOptionSets.SetPrjOptionValue(_PrjOptions: IOTAProjectOptions; const _Option, _Value: string);
var
  VersionKeys: TStrings;
  i: integer;
  s: string;
  c: char;
begin
  if Assigned(_PrjOptions) then
  begin
    //{$IFOPT D+} SendDebugFmt('Setting %s to %s (currently %s)', [AOption, AValue, FPrjOptions.Values[AOption]]);  {$ENDIF}
    try
      // BCB 5.01 AVs here on the LibDir setting every time
      if _Option = 'Keys' then begin
        s := '';
        for i := 1 to Length(_Value) do begin
          c := _Value[i];
          if c = #$0A then
            s := s + #$0D;
          s := s + c;
        end;
        VersionKeys := TStringList.Create;
        try
          VersionKeys.Text := s;
          GxOtaSetVersionInfoKeysStrings(VersionKeys);
        finally
          FreeAndNil(VersionKeys);
        end;
      end else
        _PrjOptions.Values[_Option] := _Value;
    except on E: Exception do
      raise Exception.Create(Format('Error setting option %s to "%s" (%s).  IDE bug?', [_Option, _Value, E.Message]));
    end;
  end;
end;

procedure TfmProjOptionSets.InitializeForm;
begin
  SetToolbarGradient(ToolBar);
  pcSettings.ActivePage := tabSets;
end;

function TfmProjOptionSets.GetCurrentSetName: string;
begin
  Assert(HaveSelectedSet, 'No selected set in GetCurrentSetName');
  Result := lstSets.Items[lstSets.ItemIndex];
end;

function TfmProjOptionSets.HaveSelectedSet: Boolean;
begin
  Result := lstSets.ItemIndex <> -1;
end;

procedure TfmProjOptionSets.FormHide(Sender: TObject);
begin
  FLastLoadedSet := '';
end;

procedure TfmProjOptionSets.actNewSetExecute(Sender: TObject);
var
  NewSetName: string;
begin
  if InputQuery('Option Set Name', 'Option Set Name', NewSetName) then
  begin
    NewSetName := Trim(NewSetName);
    if NewSetName = '' then
      Exit;
    AddNewOptionSet(NewSetName);
  end;
end;

procedure TfmProjOptionSets.ActionsUpdate(Action: TBasicAction; var Handled: Boolean);
begin
  actDeleteSet.Enabled := HaveSelectedSet;
  actSaveSets.Enabled := HaveSelectedSet;
  actApplySet.Enabled := HaveSelectedSet and not FSetChanged;
  lstPrjOptions.Enabled := HaveSelectedSet;
  lstEnvOptions.Enabled := HaveSelectedSet;
  cbFilter.Enabled := HaveSelectedSet;
  actRenameSet.Enabled := HaveSelectedSet;
  if HaveSelectedSet then
    pnlCurrentSet.Caption := SCurrentSet + GetCurrentSetName
  else
    pnlCurrentSet.Caption := SCurrentSet + '<None>';
  Handled := True;
end;

procedure TfmProjOptionSets.actApplySetExecute(Sender: TObject);
begin
  if HaveSelectedSet then
  begin
    TCursor_TempHourglass;
    ApplySetOptions;
  end;
end;

procedure TfmProjOptionSets.actSaveSetsExecute(Sender: TObject);
begin
  if HaveSelectedSet then
  begin
    // Overwrite the current set in the DOM
    SaveSetOptions;
    // Now reload the set options
    LoadSetOptions;
  end;
  SaveDOMToStorageFile;
end;

procedure TfmProjOptionSets.actDeleteSetExecute(Sender: TObject);
resourcestring
  sDeleteMsg = 'Are you sure you want to delete this option set?';
begin
  if HaveSelectedSet then
  begin
    if MessageDlg(sDeleteMsg, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      // Delete a set from our DOM
      DeleteSetFromStorage;
      // Delete the item from our list
      lstSets.Items.Delete(lstSets.ItemIndex);
      FLastLoadedSet := '';
      // Clear checks from the option lists, since no set is selected
      TCheckListBox_UncheckAll(lstPrjOptions);
      FPrjSetOptions.Clear;
      TCheckListBox_UncheckAll(lstEnvOptions);
      FEnvSetOptions.Clear;
    end;
  end;
end;

procedure TfmProjOptionSets.actHelpExecute(Sender: TObject);
begin
  GxContextHelp(Self, 27);
end;

procedure TfmProjOptionSets.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    Key := 0;
    Close;
  end;
end;

procedure TfmProjOptionSets.lstEnvironmentOptMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FEnvItemIndex := lstEnvOptions.ItemAtPos(Point(X, Y), True);
end;

procedure TfmProjOptionSets.lstProjectOptMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FProjItemIndex := lstPrjOptions.ItemAtPos(Point(X, Y), True);
end;

procedure TfmProjOptionSets.LoadStorageIntoDOM;
var
  Root: IXMLElement;
  fn: string;
begin
  FDom := CreateXMLDoc;
  fn := PrjOptSetsExpert.StorageFile;
  if FileExists(fn) and (GetFileSize(fn) <> 0) then
    FDom.Load(fn)
  else begin
    AddXMLHeader(FDom);
    Root := FDom.CreateElement(ROOT_NODE);
    FDom.DocumentElement := Root;
  end;
end;

procedure TfmProjOptionSets.SaveDOMToStorageFile;
begin
  // We are calling SaveDOMToStorageFile from the destructor where
  // we may be in a forced clean-up due to an exception.
  if ExceptObject <> nil then
    Exit;

  FDom.Save(PrjOptSetsExpert.StorageFile, ofIndent);
end;

function TfmProjOptionSets.FindSetByName(const Name: string): IXMLElement;
var
  Nodes: IXMLNodeList;
  Node: IXMLNode;
  i: Integer;
  SetName: string;
begin
  Result := nil;
  Nodes := FDom.documentElement.SelectNodes(SET_NODE);
  for i := 0 to Nodes.Length - 1 do begin
    Node := Nodes.Item[i];
    SetName := GetNodeAttributeValue((Node as IXMLElement), ATTR_NODE);
    if (SetName = Name) and (Node.NodeType = ELEMENT_NODE) then
    begin
      Result := Node as IXmlElement;
      Break;
    end;
  end;
end;

function TfmProjOptionSets.GetNodeAttributeValue(Element: IXMLElement; const Name: string): string;
var
  List: IXMLNamedNodeMap;
  A: IXMLNode;
begin
  Result := '';
  List := Element.Attributes;
  if List = nil then
    Exit;
  A := List.GetNamedItem(Name);
  if A = nil then
    Exit;
  Result := A.NodeValue;
end;

procedure TfmProjOptionSets.actRenameSetExecute(Sender: TObject);
var
  NewSetName: string;
  SetNode: IXMLElement;
begin
  if not HaveSelectedSet then
    Exit;
  NewSetName := GetCurrentSetName;
  if InputQuery('Option Set Name', 'Option Set Name', NewSetName) then
  begin
    NewSetName := Trim(NewSetName);
    if NewSetName <> '' then
    begin
      SetNode := FindSetByName(GetCurrentSetName);
      Assert(Assigned(SetNode));
      SetNode.SetAttribute(ATTR_NODE, NewSetName);
      lstSets.Items[lstSets.ItemIndex] := NewSetName;
    end;
  end;
end;

function TfmProjOptionSets.GetPrjOptionValue(_PrjOptions: IOTAProjectOptions; const _Option: string): string;
var
  idx: Integer;
  tmpObj: TOptionDesc;
begin
  Result := '';
  if Assigned(_PrjOptions) then
  begin
    idx := lstPrjOptions.Items.IndexOf(_Option);
    if idx <> -1 then
    begin
      tmpObj := TOptionDesc(lstPrjOptions.Items.Objects[idx]);
      if (tmpObj.OptionKind = tkClass) and (_Option = 'Keys') then
        Result := GxOtaGetVersionInfoKeysString
      else
        Result := GetVariantValueAsString(_PrjOptions.Values[_Option], tmpObj.OptionKind);
    end;
  end;
end;

procedure TfmProjOptionSets.mniModifyPrjOptionValuesClick(Sender: TObject);
begin //FI:W519
  // This event is assignned at runtime to an internal IDE event handler
end;

procedure TfmProjOptionSets.pnlFilterComboHostResize(Sender: TObject);
begin
  cbFilter.Width := pnlFilterComboHost.ClientWidth - (2 * cbFilter.Left);
end;

initialization
  RegisterGX_Expert(TProjOptionSetsExpert);

end.

