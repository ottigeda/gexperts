unit GX_ConfigurationInfo;

{$I GX_CondDefine.inc}

interface

uses
  Registry,
  Graphics, Classes, TypInfo, Forms, ComCtrls, Types, IniFiles;

type
  IConfigInfo = interface(IUnknown) //FI:W523 - we don't need a GUID
    procedure SaveSettings;
    procedure SetAlphabetizeMenu(const Value: Boolean);
    procedure SetConfigPath(const Value: string);
    procedure SetGExpertsPath(const Value: string);
    procedure SetEditorExpertsEnabled(const Value: Boolean);
    procedure SetHelpFileLocation(const Value: string);
    procedure SetVclPath(const Value: string);
    procedure SetPlaceGxMainMenuInToolsMenu(const Value: Boolean);
    procedure SetHideWindowMenu(const Value: Boolean);
    procedure SetMoveComponentMenu(const Value: Boolean);
    function GetAlphabetizeMenu: Boolean;
    function GetConfigPath: string;
    function GetEditorExpertsEnabled: Boolean;
    function GetHelpFileLocation: string;
    function GetVclPath: string;
    function GetPlaceGxMainMenuInToolsMenu: Boolean;
    function GetHideWindowMenu: Boolean;
    function GetMoveComponentMenu: Boolean;
    function GetEnableKeyboardShortcuts: Boolean;
    function GetEnableCustomFont: Boolean;
    procedure SetEnableCustomFont(const Value: Boolean);
    procedure UpdateScreenForms;

    // Return the IDE's base registry key without a
    // trailing backslash, e.g.
    //    SOFTWARE\Borland\Delphi\6.0
    function IdeRootRegistryKey: string;
    // Return the GExperts base registry key within
    // the IDE without a trailing backslash, e.g.
    //    SOFTWARE\Borland\Delphi\6.0\GExperts
    function GExpertsIdeRootRegistryKey: string;
    // Return the location of the GExperts help file.
    property HelpFile: string read GetHelpFileLocation write SetHelpFileLocation;
    // Return the location of the VCL source code.
    // Path is guaranteed to have a trailing backslash.
    property VCLPath: string read GetVclPath write SetVclPath;
    // Return the path to the configuration files;
    // is guaranteed to have a trailing backslash.
    property ConfigPath: string read GetConfigPath write SetConfigPath;
    // Return whether the editor experts are enabled.
    property EditorExpertsEnabled: Boolean read GetEditorExpertsEnabled write SetEditorExpertsEnabled;
    // Determines whether the entries in the GExperts top level menu
    // are sorted alphabetically or historically.
    property AlphabetizeMenu: Boolean read GetAlphabetizeMenu write SetAlphabetizeMenu;
    // Determine whether the GExperts top-level Menu should be placed
    // as a menu item in the IDE's top-level Tools main menu, or whether
    // it should be a top-level menu of its own.
    property PlaceGxMainMenuInToolsMenu: Boolean read GetPlaceGxMainMenuInToolsMenu write SetPlaceGxMainMenuInToolsMenu;
    property HideWindowMenu: Boolean read GetHideWindowMenu write SetHideWindowMenu;
    property MoveComponentMenu: Boolean read GetMoveComponentMenu write SetMoveComponentMenu;
    function GExpertsPath: string;
    property EnableKeyboardShortcuts: Boolean read GetEnableKeyboardShortcuts;
    property EnableCustomFont: Boolean read GetEnableCustomFont write SetEnableCustomFont;
    function CustomFont: TFont;
  end;

  TGXFontFlag = (ffColor);
  TGXFontFlags = set of TGXFontFlag;

  TFormSaveFlag = (fsSize, fsPosition);
  TFormSaveFlags = set of TFormSaveFlag;

  TGExpertsBaseSettings = class(TCustomIniFile)
  private
    FIniFile: TCustomIniFile;
    FOwnsIniFile: Boolean;
  public
    constructor Create(_IniFile: TCustomIniFile; _OwnsIniFile: Boolean = False);
    destructor Destroy; override;
    function ReadDate(const Section, Name: string; Default: TDateTime): TDateTime; override;
    function ReadDateTime(const Section, Name: string; Default: TDateTime): TDateTime; override;
    function ReadInteger(const Section, Ident: string; Default: Longint): Longint; override;
    function ReadFloat(const Section, Name: string; Default: Double): Double; override;
    function ReadString(const Section, Ident, Default: string): string; override;
    function ReadTime(const Section, Name: string; Default: TDateTime): TDateTime; override;
    function ReadBinaryStream(const Section, Name: string; Value: TStream): Integer; override;
//    procedure ReadKeys(const Section: string; Sections: TStrings);
    procedure WriteDate(const Section, Name: string; Value: TDateTime); override;
    procedure WriteDateTime(const Section, Name: string; Value: TDateTime); override;
    procedure WriteFloat(const Section, Name: string; Value: Double); override;
    procedure WriteInteger(const Section, Ident: string; Value: Longint); override;
    procedure WriteString(const Section, Ident, Value: String); override;
    procedure WriteTime(const Section, Name: string; Value: TDateTime); override;
    procedure WriteBinaryStream(const Section, Name: string; Value: TStream); override;
    procedure ReadSection(const Section: string; Strings: TStrings); override;
    procedure ReadSections(Strings: TStrings); overload; override;
    procedure ReadSectionValues(const Section: string; Strings: TStrings); override;
    procedure EraseSection(const Section: string); override;
    procedure DeleteKey(const Section, Ident: String); override;
    procedure UpdateFile; override;
  end;

  TGExpertsSettings = class;

  ///<summary>
  /// handles the settings of a particular expert, stored under the section given in the constructor </summary>
  TExpertSettings = class
  private
    FGExpertsSettings: TGExpertsSettings;
    FSection: string;
  public
    constructor Create(GExpertsSettings: TGExpertsSettings; const Section: string);
    function ReadBool(const Ident: string; Default: Boolean): Boolean;
    procedure WriteBool(const Ident: string; Value: Boolean);
    function ReadBounds(const Default: TRect): TRect;
    procedure WriteBounds(const Value: TRect);
    function ReadInteger(const Ident: string; Default: Longint): Longint;
    procedure WriteInteger(const Ident: string; Value: Longint);
    function ReadString(const Ident: string; const Default: string): string;
    procedure WriteString(const Ident: string; const Value: string);
    function ReadAnsiChar(const Ident: string; Default: AnsiChar): AnsiChar;
    procedure WriteAnsiChar(const Ident: string; Value: AnsiChar);
    function ReadEnumerated(const Ident: string; TypeInfo: PTypeInfo; Default: Longint): Longint;
    procedure WriteEnumerated(const Ident: string; TypeInfo: PTypeInfo; Value: Longint);
    procedure EraseSection(const Section: string);
    procedure ReadSection(const Section: string; Strings: TStrings); 
    procedure LoadFont(const FontName: string; const Font: TFont; Flags: TGXFontFlags = []);
    procedure SaveFont(const FontName: string; const Font: TFont; Flags: TGXFontFlags = []);
    procedure LoadForm(const Section: string; Form: TCustomForm; FormSaveFlags: TFormSaveFlags = [fsSize, fsPosition]);
    procedure SaveForm(const Section: string; Form: TCustomForm; FormSaveFlags: TFormSaveFlags = [fsSize, fsPosition]);
    procedure ReadStrings(const ListName: string; List: TStrings; const Ident: string = 'Item');
    procedure WriteStrings(const ListName: string; List: TStrings; const Ident: string = 'Item');
    function ValueExists(const Ident: string): Boolean;
    ///<summary>
    /// SubSection can be empty </summary>
    function SectionExists(const SubSection: string): boolean;
    ///<summary>
    /// SubSection can be empty </summary>
    procedure ReadSectionValues(const SubSection: string; Strings: TStrings);
    procedure DeleteKey(const Ident: String);
    ///<summary>
    /// Create a new TExpertSettings instance that reads/writes to Section below
    /// the one used by this instance, that is FSection+'\'+Section. </summary>
    function CreateExpertSettings(const Section: string): TExpertSettings;
  end;

  TGExpertsSettings = class(TGExpertsBaseSettings)
  public
    // Save settings of Font to the registry under Section.
    procedure SaveFont(const Section: string; const Font: TFont; Flags: TGXFontFlags = []);
    // From Section, load settings into Font from the registry.
    procedure LoadFont(const Section: string; const Font: TFont; Flags: TGXFontFlags = []);
    // Write List to registry at Section, using Ident as the prefix for the string values
    procedure ReadStrings(const List: TStrings; const Section, Ident: string);
    // Read from registry at Section strings into List, using Ident as the prefix for the string values.
    procedure WriteStrings(const List: TStrings; const Section, Ident: string);
    function ReadEnumerated(const Section, Ident: string; TypeInfo: PTypeInfo; Default: Longint): Longint;
    procedure WriteEnumerated(const Section, Ident: string; TypeInfo: PTypeInfo; Value: Longint);
    procedure SaveForm(Form: TCustomForm; const Section: string = '';
      FormSaveFlags: TFormSaveFlags = [fsSize, fsPosition]);
    procedure LoadForm(Form: TCustomForm; const Section: string = '';
      FormSaveFlags: TFormSaveFlags = [fsSize, fsPosition]);
    constructor Create(const FileName: string = ''); overload;
    constructor Create(_IniFile: TCustomIniFile; _OwnsIniFile: Boolean = False); overload;
    function CreateExpertSettings(const Section: string): TExpertSettings;
  end;

function ConfigInfo: IConfigInfo;

//
// Registry persistence utility functions
//

// Write List to registry at Key, using SubKey as the prefix for the string values
procedure RegReadStrings(const Settings: TGExpertsSettings; const List: TStrings; const Key, SubKey: string);

// Read from registry at Key strings into List, using SubKey as the prefix for the string values.
procedure RegWriteStrings(const Settings: TGExpertsSettings; const List: TStrings; const Key, SubKey: string);

type
  TTreeSaveOption = (stsSelected, stsExpanded);
  TTreeSaveOptions = set of TTreeSaveOption;

const
  TreeSaveAll = [stsSelected, stsExpanded];

// Save/restore the selected and expanded state of a TTreeView
procedure SaveTreeSettings(Tree: TTreeView; Options: TTreeSaveOptions; SettingsKey: string);
procedure LoadTreeSettings(Tree: TTreeView; Options: TTreeSaveOptions; const SettingsKey: string);

implementation

uses
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  SysUtils,
  GX_EditorEnhancements, GX_MessageBox,
  GX_GenericUtils, GX_GenericClasses, GX_IdeUtils, GX_OtaUtils, GX_VerDepConst,
  Math, GX_BaseForm, GX_IdeDock;

type
  TConfigInfo = class(TSingletonInterfacedObject, IConfigInfo)
  private
    FVclPath: string;
    FGExpertsPath: string;
    FConfigPath: string;
    FHelpFileLocation: string;
    FIdeRootRegistryKey: string;
    FEditorExpertsEnabled: Boolean;
    FAlphabetizeMenu: Boolean;
    FPlaceGxMainMenuInToolsMenu: Boolean;
    FEnableKeyboardShortcuts: Boolean;
    FEnableCustomFont: Boolean;
    FCustomFont: TFont;
    FHideWindowMenu: Boolean;
    FMoveComponentMenu: Boolean;
    procedure LoadSettings;
    function DefaultConfigPath: string;
  protected
    // IConfigInfo
    procedure SaveSettings;
    procedure SetAlphabetizeMenu(const Value: Boolean);
    procedure SetConfigPath(const Value: string);
    procedure SetGExpertsPath(const Value: string);
    procedure SetEditorExpertsEnabled(const Value: Boolean);
    procedure SetHelpFileLocation(const Value: string);
    procedure SetVclPath(const Value: string);
    procedure SetPlaceGxMainMenuInToolsMenu(const Value: Boolean);
    procedure SetHideWindowMenu(const Value: Boolean);
    procedure SetMoveComponentMenu(const Value: Boolean);
    function GetAlphabetizeMenu: Boolean;
    function GetConfigPath: string;
    function GetEditorExpertsEnabled: Boolean;
    function GExpertsIdeRootRegistryKey: string;
    function GetHelpFileLocation: string;
    function IdeRootRegistryKey: string;
    function GetVclPath: string;
    function GExpertsPath: string;
    function GetPlaceGxMainMenuInToolsMenu: Boolean;
    function GetHideWindowMenu: Boolean;
    function GetMoveComponentMenu: Boolean;
    function ConfigurationKey: string;
    function GetEnableKeyboardShortcuts: Boolean;
    function GetEnableCustomFont: Boolean;
    procedure SetEnableCustomFont(const Value: Boolean);
    function CustomFont: TFont;
    procedure UpdateScreenForms;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TShowBadDirectoryMessage = class(TGxMsgBoxAdaptor)
  protected
    function GetMessage: string; override;
    function ShouldShow: Boolean; override;
  end;

var
  FPrivateConfigurationInfo: TConfigInfo = nil;

function ConfigInfo: IConfigInfo;
begin
  if not Assigned(FPrivateConfigurationInfo) then
    FPrivateConfigurationInfo := TConfigInfo.Create;

  Result := FPrivateConfigurationInfo;
end;

procedure RegSaveFont(const Settings: TGExpertsSettings; const Key: string; const Font: TFont);
begin
  Settings.SaveFont(Key, Font);
end;

procedure RegLoadFont(const Settings: TGExpertsSettings; const Key: string; const Font: TFont);
begin
  Settings.LoadFont(Key, Font);
end;

procedure RegReadStrings(const Settings: TGExpertsSettings; const List: TStrings; const Key, SubKey: string);
begin
  Settings.ReadStrings(List, Key, SubKey);
end;

procedure RegWriteStrings(const Settings: TGExpertsSettings; const List: TStrings; const Key, SubKey: string);
begin
  Settings.WriteStrings(List, Key, SubKey);
end;

// Return a text path to the node passed in
// Format:
//       Parent
//         Child1
//            Child2
// returns
//      Parent|Child1|Child2
//
// This is based on the nodes' text (!) property
// not on the node (component) names.
function GetNodePath(Node: TTreeNode): string;
begin
  Assert(Node <> nil);
  Result := Node.Text;
  while Node.Parent <> nil do
  begin
    Node := Node.Parent;
    Result := Node.Text + '|' + Result;
  end;
end;

procedure SaveTreeSettings(Tree: TTreeView; Options: TTreeSaveOptions; SettingsKey: string);
var
  Settings: TGExpertsSettings;
  CurrentNode: TTreeNode;
  i: Cardinal;
begin
  if Tree = nil then Exit;

  if SettingsKey = '' then Exit;

  if (stsSelected in Options) and (Tree.Selected <> nil) then
  begin
    Settings := TGExpertsSettings.Create(SettingsKey);
    try
      {$IFOPT D+} SendDebug('FileMgr: Saving selection ' + GetNodePath(Tree.Selected)); {$ENDIF D-}
      Settings.WriteString(Tree.Name, 'Selected', GetNodePath(Tree.Selected)); // Do not localize.
    finally
      FreeAndNil(Settings);
    end;
  end;

  if (stsExpanded in Options) and (Tree.Items.Count > 0) then
  begin
    SettingsKey := AddSlash(SettingsKey) + Tree.Name;
    Settings := TGExpertsSettings.Create(SettingsKey); // Do not localize.
    try
      Settings.EraseSection('Expanded');

      CurrentNode := Tree.Items.GetFirstNode;
      i := 0;
      while CurrentNode <> nil do
      begin
        if CurrentNode.Expanded then
        begin
          {$IFOPT D+} SendDebug('FileMgr: Writing expanded ' + GetNodePath(CurrentNode)); {$ENDIF}
          Settings.WriteString('Expanded', Format('Value%d', [i]), GetNodePath(CurrentNode));
          Inc(i);
        end;
        CurrentNode := CurrentNode.GetNext;
      end;
    finally
      FreeAndNil(Settings);
    end;
  end;
  {$IFOPT D+} SendDebug('FileMgr: Done writing tree state'); {$ENDIF}
end;

procedure LoadTreeSettings(Tree: TTreeView; Options: TTreeSaveOptions; const SettingsKey: string);

  function FindAtParentLevel(ParentNode: TTreeNode; const NodeName: string): TTreeNode;
  var
    CurrentNode: TTreeNode;
  begin
    Result := nil;

    if ParentNode = nil then
      CurrentNode := Tree.Items.GetFirstNode
    else
      CurrentNode := ParentNode.GetFirstChild;

    while CurrentNode <> nil do
    begin
      if SameText(CurrentNode.Text, NodeName) then
      begin
        Result := CurrentNode;
        Break;
      end;
      CurrentNode := CurrentNode.GetNextSibling;
    end;
  end;

  function GetNode(const NodePath: string): TTreeNode;
  var
    PathList: TStrings;
    CurrentNode: TTreeNode;
    j: Integer;
  begin
    Result := nil;
    PathList := TStringList.Create;
    try
      AnsiStrTok(NodePath, '|', PathList);
      if PathList.Count = 0 then
        Exit;

      CurrentNode := nil;
      for j := 0 to PathList.Count - 2 do
      begin
        CurrentNode := FindAtParentLevel(CurrentNode, PathList[j]);
        if CurrentNode = nil then
          Exit;
      end;
      Result := FindAtParentLevel(CurrentNode, PathList[PathList.Count - 1]);
    finally
      FreeAndNil(PathList);
    end;
  end;

var
  Path: string;
  Settings: TGExpertsSettings;
  FoundNode: TTreeNode;
  i: Integer;
  Expand: TStrings;
begin
  if (Tree = nil) or (Tree.Items.Count <= 0) then Exit;

  if SettingsKey = '' then Exit;

  if stsExpanded in Options then
  begin
    Settings := TGExpertsSettings.Create(AddSlash(SettingsKey) + Tree.Name);
    try
      Expand := TStringList.Create;
      try
        Settings.ReadSectionValues('Expanded', Expand);
        for i := 0 to Expand.Count - 1 do
        begin
          {$IFOPT D+} SendDebug('FileMgr: Getting for expansion ' + Expand[i]); {$ENDIF}
          FoundNode := GetNode(Expand[i]);
          if FoundNode <> nil then
            FoundNode.Expand(False);
        end;
      finally
        FreeAndNil(Expand);
      end;
    finally
      FreeAndNil(Settings);
    end;
  end;

  if stsSelected in Options then
  begin
    Settings := TGExpertsSettings.Create(SettingsKey);
    try
      Path := Settings.ReadString(Tree.Name, 'Selected', ''); // Do not localize.
      {$IFOPT D+} SendDebug('FileMgr: Restoring selection ' + Path); {$ENDIF}
      FoundNode := GetNode(Path);
      if FoundNode <> nil then
        Tree.Selected := FoundNode;
    finally
      FreeAndNil(Settings);
    end;
  end;
end;

{ TConfigInfo }

constructor TConfigInfo.Create;
begin
  // Don't do anything significant here, because this gets recreated many times
  {$IFOPT D+} SendDebug('Creating configuration info'); {$ENDIF D+}
  inherited Create;
  FPrivateConfigurationInfo := Self;
  FCustomFont := TFont.Create;

  FIdeRootRegistryKey := GxOtaGetIdeBaseRegistryKey;
  FVclPath := AddSlash(GetIdeRootDirectory) +
              AddSlash('Source') +
              {$IFNDEF GX_VER230_up} {$IFDEF GX_VER170_up} AddSlash('Win32') + {$ENDIF} {$ENDIF}
              AddSlash('VCL'); // Do not localize.

  FGExpertsPath := AddSlash(ExtractFilePath(ThisDllName));
  FConfigPath := DefaultConfigPath;

  EditorEnhancements.Enabled := False;

  LoadSettings;
  ShowGxMessageBox(TShowBadDirectoryMessage, FConfigPath);
end;

function TConfigInfo.DefaultConfigPath: string;
begin
  Result := AddSlash(GetUserApplicationDataFolder) + AddSlash('GExperts') + IDEEnglishName;
end;

destructor TConfigInfo.Destroy;
begin
  {$IFOPT D+} SendDebug('TConfigInfo.Destroy'); {$ENDIF D+}
  //SaveSettings; // Call this below to prevent re-creating TConfigInfo
  FreeEditorEnhancements;
  FreeAndNil(FCustomFont);

  inherited Destroy;
end;

procedure TConfigInfo.LoadSettings;
var
  Settings: TGExpertsSettings;
  Setting: Boolean;
begin
  {$IFOPT D+} SendDebug('Loading configuration info settings'); {$ENDIF D+}

  // Do not localize any of the following strings.
  Settings := TGExpertsSettings.Create;
  try
    FVclPath := AddSlash(Settings.ReadString(ConfigurationKey, 'VCLPath', FVclPath));
    FConfigPath := AddSlash(Settings.ReadString(ConfigurationKey, 'ConfigPath', FConfigPath));
    FHelpFileLocation := Settings.ReadString(ConfigurationKey, 'HelpFile', FGExpertsPath + 'GExperts.chm');
    if SameText(ExtractFileExt(FHelpFileLocation), '.hlp') then
      FHelpFileLocation := ChangeFileExt(FHelpFileLocation, '.chm');
    FAlphabetizeMenu := Settings.ReadBool(ConfigurationKey, 'AlphabetizeMenu', True);
    FEditorExpertsEnabled := Settings.ReadBool(ConfigurationKey, 'EditorExpertsEnabled', True);
    FPlaceGxMainMenuInToolsMenu := Settings.ReadBool(ConfigurationKey, 'PlaceGxMainMenuInToolsMenu', False);
    FHideWindowMenu := Settings.ReadBool(ConfigurationKey, 'HideWindowMenu', False);
    FMoveComponentMenu := Settings.ReadBool(ConfigurationKey, 'MoveComponentMenu', False);
    FEnableKeyboardShortcuts := Settings.ReadBool(ConfigurationKey, 'EnableKeyboardShortcuts', True);
    FEnableCustomFont := Settings.ReadBool(ConfigurationKey, 'EnableCustomFont', False);
    Settings.LoadFont(AddSlash(ConfigurationKey) + 'CustomFont', FCustomFont);

    Setting := Settings.ReadBool(ConfigurationKey, 'EditorEnhancementsEnabled', False);
    EditorEnhancements.Enabled := Setting and not IsStandAlone;
  finally
    FreeAndNil(Settings);
  end;
end;

procedure TConfigInfo.SaveSettings;
var
  Settings: TGExpertsSettings;
begin
  if IsStandAlone then
    Exit;

  // Do not localize any of the following strings.
  Settings := TGExpertsSettings.Create;
  try
    Settings.WriteString(ConfigurationKey, 'VCLPath', FVclPath);
    Settings.WriteString(ConfigurationKey, 'ConfigPath', FConfigPath);
    Settings.WriteString(ConfigurationKey, 'HelpFile', FHelpFileLocation);
    Settings.WriteBool(ConfigurationKey, 'AlphabetizeMenu', FAlphabetizeMenu);
    Settings.WriteBool(ConfigurationKey, 'EditorExpertsEnabled', FEditorExpertsEnabled);
    Settings.WriteBool(ConfigurationKey, 'PlaceGxMainMenuInToolsMenu', FPlaceGxMainMenuInToolsMenu);
    Settings.WriteBool(ConfigurationKey, 'HideWindowMenu', FHideWindowMenu);
    Settings.WriteBool(ConfigurationKey, 'MoveComponentMenu', FMoveComponentMenu);
    Settings.WriteBool(ConfigurationKey, 'EditorEnhancementsEnabled', EditorEnhancements.Enabled);
    Settings.WriteBool(ConfigurationKey, 'EnableCustomFont', FEnableCustomFont);
    Settings.SaveFont(AddSlash(ConfigurationKey) + 'CustomFont', FCustomFont);
  finally
    FreeAndNil(Settings);
  end;
end;

procedure TConfigInfo.SetVclPath(const Value: string);
begin
  FVclPath := AddSlash(Value);
end;

function TConfigInfo.GExpertsIdeRootRegistryKey: string;
const
  SGExpertsString = 'GExperts-1.3';
begin
  Result := AddSlash(FIdeRootRegistryKey) + SGExpertsString;
end;

procedure TConfigInfo.SetConfigPath(const Value: string);
begin
  FConfigPath := AddSlash(Value);
end;

function TConfigInfo.GetAlphabetizeMenu: Boolean;
begin
  Assert(not IsStandAlone);

  Result := FAlphabetizeMenu;
end;

function TConfigInfo.GetConfigPath: string;
begin
  Result := FConfigPath;
end;

function TConfigInfo.GetEditorExpertsEnabled: Boolean;
begin
  Assert(not IsStandAlone);

  Result := FEditorExpertsEnabled;
end;

function TConfigInfo.GetHelpFileLocation: string;
begin
  Result := FHelpFileLocation;
end;

function TConfigInfo.IdeRootRegistryKey: string;
begin
  Result := FIdeRootRegistryKey;
end;

function TConfigInfo.GetMoveComponentMenu: Boolean;
begin
  Result := FMoveComponentMenu;
end;

procedure TConfigInfo.SetMoveComponentMenu(const Value: Boolean);
begin
  FMoveComponentMenu := Value;
end;

function TConfigInfo.GetVclPath: string;
begin
  Result := FVclPath;
end;

procedure TConfigInfo.SetAlphabetizeMenu(const Value: Boolean);
begin
  Assert(not IsStandAlone);

  FAlphabetizeMenu := Value;
end;

procedure TConfigInfo.SetEditorExpertsEnabled(const Value: Boolean);
begin
  Assert(not IsStandAlone);

  FEditorExpertsEnabled := Value;
end;

procedure TConfigInfo.SetHelpFileLocation(const Value: string);
begin
  FHelpFileLocation := Value;
end;

function TConfigInfo.GetPlaceGxMainMenuInToolsMenu: Boolean;
begin
  Result := FPlaceGxMainMenuInToolsMenu;
end;

procedure TConfigInfo.SetPlaceGxMainMenuInToolsMenu(const Value: Boolean);
begin
  FPlaceGxMainMenuInToolsMenu := Value;
end;

function TConfigInfo.GetHideWindowMenu: Boolean;
begin
  Result := FHideWindowMenu;
end;

procedure TConfigInfo.SetHideWindowMenu(const Value: Boolean);
begin
  FHideWindowMenu := Value;
end;

function TConfigInfo.GExpertsPath: string;
begin
  Result := ExtractFilePath(FGExpertsPath);
end;

procedure TConfigInfo.SetGExpertsPath(const Value: string);
begin
  FGExpertsPath := AddSlash(Value);
end;

function TConfigInfo.ConfigurationKey: string;
begin
  Result := 'Misc';
end;

function TConfigInfo.GetEnableKeyboardShortcuts: Boolean;
begin
  Result := FEnableKeyboardShortcuts;
end;

function TConfigInfo.CustomFont: TFont;
begin
  Result := FCustomFont;
end;

function TConfigInfo.GetEnableCustomFont: Boolean;
begin
  Result := FEnableCustomFont;
end;

procedure TConfigInfo.UpdateScreenForms;
var
  i: Integer;
  Form: TCustomForm;
begin
  for i := 0 to Screen.FormCount - 1 do
  begin
    Form := Screen.Forms[i];
    if (Form is TfmBaseForm) or (Form is TfmIdeDockForm) then
      Form.Font.Assign(FCustomFont);
  end;
end;

procedure TConfigInfo.SetEnableCustomFont(const Value: Boolean);
begin
  FEnableCustomFont := Value;
end;

{ TShowBadDirectoryMessage }

function TShowBadDirectoryMessage.GetMessage: string;
resourcestring
  SBadConfigPath =
    'The storage directory defined in the GExperts configuration dialog does ' +
    'not exist or is read-only. Under Windows Vista or later, this may be due ' +
    'to being a limited user and not having write access to this directory. ' +
    'Please correct the problem or you may find that some GExperts data files ' +
    'are not being saved.' + sLineBreak +
    'Current Directory: %s' + sLineBreak +
    'Suggested Directory: %s';
begin
  Assert(Assigned(FPrivateConfigurationInfo));
  Result := Format(SBadConfigPath, [FData, FPrivateConfigurationInfo.DefaultConfigPath]);
end;

var
  ShownOnce: Boolean = False;

function TShowBadDirectoryMessage.ShouldShow: Boolean;
begin
  Result := not ShownOnce;
  // The directory must exist and be writeable to be valid.
  if Result then
    Result := not PrepareDirectoryForWriting(ConfigInfo.ConfigPath);
  ShownOnce := True;
end;

const
  FontNameIdent = 'Name'; // Do not localize.
  FontSizeIdent = 'Size'; // Do not localize.
  FontColorIdent = 'Color'; // Do not localize.
  FontStyleBoldIdent = 'Bold'; // Do not localize.
  FontStyleItalicIdent = 'Italic'; // Do not localize.
  FontStyleUnderlineIdent = 'Underline'; // Do not localize.
  CountIdent = 'Count'; // Do not localize.

{ TGExpertsSettings }

constructor TGExpertsSettings.Create(const FileName: string);
begin
  if FileName = '' then
    inherited Create(TRegistryIniFile.Create(ConfigInfo.GExpertsIdeRootRegistryKey), True)
  else
    inherited Create(TRegistryIniFile.Create(FileName), True);
end;

constructor TGExpertsSettings.Create(_IniFile: TCustomIniFile; _OwnsIniFile: Boolean);
begin
  inherited Create(_IniFile, _OwnsIniFile);
end;

procedure TGExpertsSettings.SaveFont(const Section: string; const Font: TFont; Flags: TGXFontFlags);
begin
  Assert(Assigned(Font), 'nil font in TGExpertsSettings.SaveFont');
  WriteString(Section, FontNameIdent, Font.Name);
  WriteInteger(Section, FontSizeIdent, Font.Size);
  WriteBool(Section, FontStyleBoldIdent, (fsBold in Font.Style));
  WriteBool(Section, FontStyleItalicIdent, (fsItalic in Font.Style));
  WriteBool(Section, FontStyleUnderlineIdent, (fsUnderline in Font.Style));
  if ffColor in Flags then
    WriteInteger(Section, FontColorIdent, Font.Color);
end;

procedure TGExpertsSettings.LoadFont(const Section: string; const Font: TFont; Flags: TGXFontFlags);
begin
  Assert(Assigned(Font),  'nil font in TGExpertsSettings.LoadFont');
  Font.Name := ReadString(Section, FontNameIdent, Font.Name);
  Font.Size := ReadInteger(Section, FontSizeIdent, Font.Size);
  if ReadBool(Section, FontStyleBoldIdent, (fsBold in Font.Style)) then
    Font.Style := Font.Style + [fsBold];
  if ReadBool(Section, FontStyleItalicIdent, (fsItalic in Font.Style)) then
    Font.Style := Font.Style + [fsItalic];
  if ReadBool(Section, FontStyleUnderlineIdent, (fsUnderline in Font.Style)) then
    Font.Style := Font.Style + [fsUnderline];
  if ffColor in Flags then
    Font.Color := ReadInteger(Section, FontColorIdent, Font.Color);
end;

procedure TGExpertsSettings.ReadStrings(const List: TStrings; const Section, Ident: string);
var
  i: Integer;
  RegistryValueCount: Integer;
  ListString: string;
  Identifier: string;
begin
  Assert(Assigned(List));
  RegistryValueCount := Max(0, ReadInteger(Section, CountIdent, 0));
  for i := 0 to RegistryValueCount - 1 do
  begin
    Identifier := Ident + IntToStr(i);
    if ValueExists(Section, Identifier) then
    begin
      ListString := ReadString(Section, Identifier, '');
      List.Add(ListString);
    end
    {$IFOPT D+}
    else SendDebugError('The Count value for ' +Section+ ':' +Ident+ ' is invalid');
    {$ENDIF}
  end;
end;

procedure TGExpertsSettings.WriteStrings(const List: TStrings; const Section, Ident: string);
var
  i: Integer;
begin
  Assert(Assigned(List), 'nil string list in TGExpertsSettings.WriteStrings');
  EraseSection(Section);
  // Assume brutally that a write will fail.
  WriteInteger(Section, CountIdent, 0);
  for i := 0 to List.Count - 1 do
  begin
    // We do never run into a conflict with the "Count" value,
    // as the Ident always gets a number appended.
    WriteString(Section, Format('%s%d', [Ident, i]), List.Strings[i]);
  end;
  // Record the amount of values written.
  WriteInteger(Section, CountIdent, List.Count);
end;

function TGExpertsSettings.ReadEnumerated(const Section, Ident: string; TypeInfo: PTypeInfo; Default: Longint): Longint;
var
  TypeData: PTypeData;
begin
  Assert(TypeInfo^.Kind = tkEnumeration, 'Non-enumerated type passed to ReadEnumerated');
  TypeData := GetTypeData(TypeInfo);
  Assert(Default in [TypeData^.MinValue..TypeData^.MaxValue], 'Bad default value in ReadEnumerated');
  Result := ReadInteger(Section, Ident, Default);
  if not (Result in [TypeData^.MinValue..TypeData^.MaxValue]) then
  begin
    {$IFOPT D+} SendDebugError('Read in an invalid value for ' + Section + Ident); {$ENDIF D+}
    Result := Default;
  end;
end;

procedure TGExpertsSettings.WriteEnumerated(const Section, Ident: string; TypeInfo: PTypeInfo; Value: Longint);
begin
  Assert(TypeInfo^.Kind = tkEnumeration, 'Non-enumerated type passed to WriteEnumerated');
  Assert(Value in [GetTypeData(TypeInfo)^.MinValue..GetTypeData(TypeInfo)^.MaxValue], 'Bad default value in WriteEnumerated');
  WriteInteger(Section, Ident, Value);
end;

procedure TGExpertsSettings.LoadForm(Form: TCustomForm; const Section: string; FormSaveFlags: TFormSaveFlags);
var
  StorageSection: string;
begin
  if Section = '' then
    StorageSection := Form.ClassName
  else
    StorageSection := Section;

  if fsSize in FormSaveFlags then
  begin
    Form.Width := ReadInteger(StorageSection, 'Width', Form.Width);
    Form.Height := ReadInteger(StorageSection, 'Height', Form.Height);
  end;
  if fsPosition in FormSaveFlags then
  begin
    if ValueExists(StorageSection, 'Left') then begin
      Form.Left := ReadInteger(StorageSection, 'Left', Form.Left);
      Form.Top := ReadInteger(StorageSection, 'Top', Form.Top);
    end else
      CenterForm(Form)
  end else begin
    CenterForm(Form);
  end;
end;

procedure TGExpertsSettings.SaveForm(Form: TCustomForm; const Section: string;
  FormSaveFlags: TFormSaveFlags);
var
  StorageSection: string;
begin
  if Section = '' then
    StorageSection := Form.ClassName
  else
    StorageSection := Section;
  if fsPosition in FormSaveFlags then
  begin
    WriteInteger(StorageSection, 'Left', Form.Left);
    WriteInteger(StorageSection, 'Top', Form.Top);
  end;
  if fsSize in FormSaveFlags then
  begin
    WriteInteger(StorageSection, 'Width', Form.Width);
    WriteInteger(StorageSection, 'Height', Form.Height);
  end;
end;

procedure FinalizeConfigurationInfo;
begin
  if Assigned(FPrivateConfigurationInfo) then
    FPrivateConfigurationInfo.SaveSettings;
  FreeAndNil(FPrivateConfigurationInfo);
end;

function TGExpertsSettings.CreateExpertSettings(const Section: string): TExpertSettings;
begin
  Result := TExpertSettings.Create(Self, Section);
end;

{ TExpertSettings }

constructor TExpertSettings.Create(GExpertsSettings: TGExpertsSettings;
  const Section: string);
begin
  inherited Create;
  FGexpertsSettings := GExpertsSettings;
  FSection := Section;
end;

function TExpertSettings.CreateExpertSettings(const Section: string): TExpertSettings;
begin
  Result := FGExpertsSettings.CreateExpertSettings(FSection + '\' + Section);
end;

procedure TExpertSettings.DeleteKey(const Ident: String);
begin
  FGExpertsSettings.DeleteKey(FSection, Ident);
end;

procedure TExpertSettings.EraseSection(const Section: string);
begin
  FGExpertsSettings.EraseSection(FSection + '\' + Section);
end;

procedure TExpertSettings.LoadFont(const FontName: string; const Font: TFont;
  Flags: TGXFontFlags);
begin
  FGExpertsSettings.LoadFont(AddSlash(FSection) + FontName, Font, Flags);
end;

procedure TExpertSettings.LoadForm(const Section: string; Form: TCustomForm;
  FormSaveFlags: TFormSaveFlags);
begin
  FGExpertsSettings.LoadForm(Form, AddSlash(FSection) + Section, FormSaveFlags);
end;

function TExpertSettings.ReadBool(const Ident: string; Default: Boolean): Boolean;
begin
  Result := FGExpertsSettings.ReadBool(FSection, Ident, Default);
end;

function TExpertSettings.ReadBounds(const Default: TRect): TRect;
var
  Width: integer;
  Height: integer;
  Left: Integer;
  Top: Integer;
begin
  Left := ReadInteger('Left', Default.Left);
  Top := ReadInteger('Top', Default.Top);
  Width := Default.Right - Default.Left;
  Height := Default.Bottom - Default.Top;
  Width := ReadInteger('Width', Width);
  Height := ReadInteger('Height', Height);
  Result := Bounds(Left, Top, Width, Height);
end;

function TExpertSettings.ReadEnumerated(const Ident: string; TypeInfo: PTypeInfo;
  Default: Integer): Longint;
begin
  Result := FGExpertsSettings.ReadEnumerated(FSection, Ident, TypeInfo, Default);
end;

function TExpertSettings.ReadAnsiChar(const Ident: string; Default: AnsiChar): AnsiChar;
var
  Value: Integer;
begin
  Value := ReadInteger(Ident, Ord(Default));
  if Value > 255 then
    Value := 0;
  Result := AnsiChar(Value);
end;

function TExpertSettings.ReadInteger(const Ident: string; Default: Integer): Longint;
begin
  Result := FGExpertsSettings.ReadInteger(FSection, Ident, Default);
end;

procedure TExpertSettings.ReadSection(const Section: string; Strings: TStrings);
begin
  FGExpertsSettings.ReadSection(FSection + '\' + Section, Strings);
end;

procedure TExpertSettings.ReadSectionValues(const SubSection: string; Strings: TStrings);
var
  s: string;
begin
  s := FSection;
  if SubSection <> '' then
    s := AddSlash(s) + SubSection;
  FGExpertsSettings.ReadSectionValues(s, Strings);
end;

function TExpertSettings.ReadString(const Ident: string; const Default: string): string;
begin
  Result := FGExpertsSettings.ReadString(FSection, Ident, Default);
end;

procedure TExpertSettings.ReadStrings(const ListName: string; List: TStrings; const Ident: string = 'Item');
var
  s: string;
begin
  s := FSection;
  if ListName <> '' then
    s := AddSlash(s) + ListName;
  FGExpertsSettings.ReadStrings(List, s, Ident);
end;

procedure TExpertSettings.SaveFont(const FontName: string; const Font: TFont;
  Flags: TGXFontFlags);
begin
  FGExpertsSettings.SaveFont(AddSlash(FSection) + FontName, Font, Flags);
end;

procedure TExpertSettings.SaveForm(const Section: string; Form: TCustomForm;
  FormSaveFlags: TFormSaveFlags = [fsSize, fsPosition]);
begin
  FGExpertsSettings.SaveForm(Form, AddSlash(FSection) + Section, FormSaveFlags);
end;

function TExpertSettings.SectionExists(const SubSection: string): boolean;
var
  s: string;
begin
  s := FSection;
  if SubSection <> '' then
    s := AddSlash(s) + SubSection;
  Result := FGExpertsSettings.SectionExists(s);
end;

function TExpertSettings.ValueExists(const Ident: string): Boolean;
begin
  Result := FGExpertsSettings.ValueExists(FSection, Ident);
end;

procedure TExpertSettings.WriteBool(const Ident: string; Value: Boolean);
begin
  FGExpertsSettings.WriteBool(FSection, Ident, Value);
end;

procedure TExpertSettings.WriteBounds(const Value: TRect);
var
  Width: integer;
  Height: integer;
begin
  Width := Value.Right - Value.Left;
  Height := Value.Bottom - Value.Top;
  WriteInteger('Left', Value.Left);
  WriteInteger('Top', Value.Top);
  WriteInteger('Width', Width);
  WriteInteger('Height', Height);
end;

procedure TExpertSettings.WriteEnumerated(const Ident: string; TypeInfo: PTypeInfo;
  Value: Integer);
begin
  FGExpertsSettings.WriteEnumerated(FSection, Ident, TypeInfo, Value);
end;

procedure TExpertSettings.WriteAnsiChar(const Ident: string; Value: AnsiChar);
begin
  WriteInteger(Ident, Ord(Value));
end;

procedure TExpertSettings.WriteInteger(const Ident: string; Value: Integer);
begin
  FGExpertsSettings.WriteInteger(FSection, Ident, Value);
end;

procedure TExpertSettings.WriteString(const Ident: string; const Value: string);
begin
  FGExpertsSettings.WriteString(FSection, Ident, Value);
end;

procedure TExpertSettings.WriteStrings(const ListName: string; List: TStrings; const Ident: string = 'Item');
var
  s: string;
begin
  s := FSection;
  if ListName <> '' then
    s := AddSlash(s) + ListName;
  FGExpertsSettings.WriteStrings(List, s, Ident);
end;

{ TGExpertsBaseSettings }

constructor TGExpertsBaseSettings.Create(_IniFile: TCustomIniFile; _OwnsIniFile: Boolean = False);
begin
  inherited Create(_IniFile.FileName);
  FIniFile := _IniFile;
  FOwnsIniFile:= _OwnsIniFile;
end;

destructor TGExpertsBaseSettings.Destroy;
begin
  if FOwnsIniFile then
    FreeAndNil(FIniFile);
  inherited;
end;

procedure TGExpertsBaseSettings.DeleteKey(const Section, Ident: String);
begin
  FIniFile.DeleteKey(Section, Ident);
end;

procedure TGExpertsBaseSettings.EraseSection(const Section: string);
begin
  FIniFile.EraseSection(Section);
end;

function TGExpertsBaseSettings.ReadBinaryStream(const Section, Name: string; Value: TStream): Integer;
begin
  Result := FIniFile.ReadBinaryStream(Section, Name, Value);
end;

function TGExpertsBaseSettings.ReadDate(const Section, Name: string; Default: TDateTime): TDateTime;
begin
  Result := FIniFile.ReadDate(Section, Name, Default);
end;

function TGExpertsBaseSettings.ReadDateTime(const Section, Name: string; Default: TDateTime): TDateTime;
begin
  Result := FIniFile.ReadDateTime(Section, Name, Default);
end;

function TGExpertsBaseSettings.ReadFloat(const Section, Name: string; Default: Double): Double;
begin
  Result := FIniFile.ReadFloat(section, Name, Default);
end;

function TGExpertsBaseSettings.ReadInteger(const Section, Ident: string; Default: Integer): Longint;
begin
  Result := FIniFile.ReadInteger(section, ident, Default);
end;

procedure TGExpertsBaseSettings.ReadSection(const Section: string; Strings: TStrings);
begin
  FIniFile.ReadSection(Section, Strings);
end;

procedure TGExpertsBaseSettings.ReadSections(Strings: TStrings);
begin
  FIniFile.ReadSections(Strings);
end;

procedure TGExpertsBaseSettings.ReadSectionValues(const Section: string; Strings: TStrings);
begin
  FIniFile.ReadSectionValues(Section, Strings);
end;

function TGExpertsBaseSettings.ReadString(const Section, Ident, Default: string): string;
begin
  Result := FIniFile.ReadString(Section, Ident, default);
end;

function TGExpertsBaseSettings.ReadTime(const Section, Name: string; Default: TDateTime): TDateTime;
begin
  Result := FIniFile.ReadTime(Section,Name, Default);
end;

procedure TGExpertsBaseSettings.UpdateFile;
begin
  FIniFile.UpdateFile;
end;

procedure TGExpertsBaseSettings.WriteBinaryStream(const Section, Name: string; Value: TStream);
begin
  FIniFile.WriteBinaryStream(Section, Name, Value);
end;

procedure TGExpertsBaseSettings.WriteDate(const Section, Name: string; Value: TDateTime);
begin
  FIniFile.WriteDate(Section, Name, Value);
end;

procedure TGExpertsBaseSettings.WriteDateTime(const Section, Name: string; Value: TDateTime);
begin
  FIniFile.WriteDateTime(Section, Name, Value);
end;

procedure TGExpertsBaseSettings.WriteFloat(const Section, Name: string; Value: Double);
begin
  FIniFile.WriteFloat(Section, Name, Value);
end;

procedure TGExpertsBaseSettings.WriteInteger(const Section, Ident: string; Value: Integer);
begin
  FIniFile.WriteInteger(Section, Ident, Value);
end;

procedure TGExpertsBaseSettings.WriteString(const Section, Ident, Value: String);
begin
  FIniFile.WriteString(Section, Ident, Value);
end;

procedure TGExpertsBaseSettings.WriteTime(const Section, Name: string; Value: TDateTime);
begin
  FIniFile.WriteTime(Section, Name, Value);
end;

initialization
  FPrivateConfigurationInfo := nil;

finalization
  FinalizeConfigurationInfo;

end.

