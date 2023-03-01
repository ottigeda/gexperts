unit GX_EditPath;

{$I GX_CondDefine.inc}

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  Contnrs,
  SynMemo,
  GX_BaseForm;

type
  TConfigEntry = class
  private
    FConfigName: string;
    FPlatform: string;
    FPath: string;
    function GetConfigCaption: string;
    function GetPlatformCaption: string;
  public
    constructor Create(const _ConfigName, _Platform, _Path: string);
    function TextCaption: string;
    function ListCaption: string;
  end;

type
  Tf_EditPath = class(TfmBaseForm)
    p_Top: TPanel;
    l_Target: TLabel;
    cmb_Target: TComboBox;
    p_Bottom: TPanel;
    b_Cancel: TButton;
    b_Ok: TButton;
    p_Main: TPanel;
    p_Memo: TPanel;
    lb_Iherited: TListBox;
    spl_Vertical: TSplitter;
    procedure cmb_TargetDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
      State: TOwnerDrawState);
    procedure cmb_TargetChange(Sender: TObject);
    procedure b_OKClick(Sender: TObject);
  private
    FConfigs: TObjectList;
    FMemo: TSynMemo;
    FActiveItem: TConfigEntry;
    procedure SetData(const _Path, _InheritedPath: string);
    procedure GetData(out _Path: string);
    procedure HandleDropFiles(_Sender: TObject; _Files: TStrings);
  public
    constructor Create(_Owner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

uses
  ToolsAPI,
  u_dzTypes,
  u_dzVclUtils,
  u_dzStringUtils,
  u_dzStringArrayUtils,
  u_dzClassUtils,
  SynUnicode,
  SynEdit,
  GX_OtaUtils,
  GX_Experts,
  GX_EnhancedEditor;

{ Tf_EditPath }

{$IFDEF GX_DELPHI2009_UP}
const
  DCC_UnitSearchPath = 'DCC_UnitSearchPath';
{$ENDIF}

procedure Tf_EditPath.GetData(out _Path: string);
var
  i: Integer;
  Lines: TUnicodeStrings;
  s: string;
begin
  Lines := FMemo.Lines;
  _Path := '';
  for i := 0 to Lines.Count - 1 do begin
    s := Trim(Lines[i]);
    if s <> '' then
      _Path := _Path + ';' + s;
  end;
  _Path := Copy(_Path, 2);
end;

procedure Tf_EditPath.HandleDropFiles(_Sender: TObject; _Files: TStrings);
var
  i: Integer;
  Lines: TUnicodeStrings;
  fn: string;
begin
  Lines := FMemo.Lines;
  for i := 0 to _Files.Count - 1 do begin
    fn := _Files[i];
    if not DirectoryExists(fn) then
      fn := ExtractFileDir(fn);
    if DirectoryExists(fn) then
      Lines.Add(fn);
  end;
end;

procedure Tf_EditPath.SetData(const _Path, _InheritedPath: string);
var
  Dirs: TStringArray;
  i: Integer;
  Lines: TUnicodeStrings;
begin
  Dirs := SplitString(_Path, [';']);
  Lines := FMemo.Lines;
  Lines.Clear;
  for i := Low(Dirs) to High(Dirs) do
    if Trim(Dirs[i]) <> '' then
      Lines.Add(Dirs[i]);

{$IFDEF GX_DELPHI2009_UP}
  Dirs := SplitString(_InheritedPath, [';']);
  Lines := lb_Iherited.Items;
  Lines.Clear;
  for i := Low(Dirs) to High(Dirs) do
    if Trim(Dirs[i]) <> '' then
      Lines.Add(Dirs[i]);
{$ENDIF}
end;

//function GetImplementedInterfaces(AClass: TClass): TStringArray;
//var
//  i: Integer;
//  InterfaceTable: PInterfaceTable;
//  InterfaceEntry: PInterfaceEntry;
//begin
//  while Assigned(AClass) do begin
//    InterfaceTable := AClass.GetInterfaceTable;
//    if Assigned(InterfaceTable) then begin
//      SetLength(Result, InterfaceTable.EntryCount);
//      for i := 0 to InterfaceTable.EntryCount - 1 do begin
//        InterfaceEntry := @InterfaceTable.Entries[i];
//        Result[i] := GUIDToString(InterfaceEntry.IID);
//      end;
//    end;
//    AClass := AClass.ClassParent;
//  end;
//end;

//function GetImplementingObject(const i: IInterface): TObject;
//const
//  AddByte = $04244483;
//  AddLong = $04244481;
//type
//  PAdjustSelfThunk = ^TAdjustSelfThunk;
//  TAdjustSelfThunk = packed record
//    case AddInstruction: Longint of
//      AddByte: (AdjustmentByte: shortint);
//      AddLong: (AdjustmentLong: Longint);
//  end;
//  PInterfaceMT = ^TInterfaceMT;
//  TInterfaceMT = packed record
//    QueryInterfaceThunk: PAdjustSelfThunk;
//  end;
//  TInterfaceRef = ^PInterfaceMT;
//var
//  QueryInterfaceThunk: PAdjustSelfThunk;
//begin
//  Result := Pointer(i);
//  if Assigned(Result) then
//    try
//      QueryInterfaceThunk := TInterfaceRef(i)^.QueryInterfaceThunk;
//      case QueryInterfaceThunk.AddInstruction of
//        AddByte: Inc(PAnsiChar(Result), QueryInterfaceThunk.AdjustmentByte);
//        AddLong: Inc(PAnsiChar(Result), QueryInterfaceThunk.AdjustmentLong);
//      else
//        Result := nil;
//      end;
//    except
//      Result := nil;
//    end;
//end;

{$IFDEF GX_DELPHI2009_UP}
procedure Tf_EditPath.cmb_TargetDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  cmb: TComboBox absolute Control;
  Item: TConfigEntry;
begin
  Item := TConfigEntry(cmb.Items.Objects[Index]);
  if odComboBoxEdit in State then begin
    cmb.Canvas.TextRect(Rect, Rect.Left, Rect.Top, Item.TextCaption);
  end else begin
    if Item.FPlatform = '' then
      cmb.Canvas.Font.Style := cmb.Canvas.Font.Style + [fsBold];
    cmb.Canvas.TextRect(Rect, Rect.Left, Rect.Top, Item.ListCaption);
  end;
end;

procedure Tf_EditPath.cmb_TargetChange(Sender: TObject);
var
  cmb: TComboBox absolute Sender;
  Item: TConfigEntry;
  Path: string;
  i: Integer;
  cfg: TConfigEntry;
  InheritedPath: string;
begin
  if not TComboBox_GetSelectedObject(cmb, Pointer(Item)) then
    Exit; //==>
  if Item = FActiveItem then
    Exit; //==>

  if Assigned(FActiveItem) then begin
    GetData(Path);
    FActiveItem.FPath := Path;
  end;
  FActiveItem := Item;
  InheritedPath := '';
  for i := 0 to FConfigs.Count - 1 do begin
    cfg := FConfigs[i] as TConfigEntry;
    if cfg = FActiveItem then
      Break; //==v
    if ((cfg.FConfigName = 'Base') or (cfg.FConfigName = FActiveItem.FConfigName))
      and ((cfg.FPlatform = FActiveItem.FPlatform) or (cfg.FPlatform = '')) then
      InheritedPath := InheritedPath + ';' + cfg.FPath;
  end;
  InheritedPath := Copy(InheritedPath, 2);
  SetData(FActiveItem.FPath, InheritedPath);
end;

{$ELSE}

procedure Tf_EditPath.cmb_TargetDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
begin
  // do nothing
end;

procedure Tf_EditPath.cmb_TargetChange(Sender: TObject);
begin
  // do nothing
end;
{$ENDIF}

procedure Tf_EditPath.b_OKClick(Sender: TObject);
var
  Project: IOTAProject;
  ProjectOptions: IOTAProjectOptions;
  Path: string;
{$IFDEF GX_DELPHI2009_UP}
  i: Integer;
  cfg: TConfigEntry;
  CfgIdx: Integer;
  ProjectOptionsConfigurations: IOTAProjectOptionsConfigurations;
  BuildCfg: IOTABuildConfiguration;
{$ENDIF}
{$IFDEF GX_DELPHIXE2_UP}
//  PrjPlatforms: TArray<string>;
  PlatformCfg: IOTABuildConfiguration;
{$ENDIF}
begin
  inherited;
  if not GxOtaTryGetCurrentProject(Project) then
    Exit; //==>

  if Assigned(FActiveItem) then
    GetData(FActiveItem.FPath);

  ProjectOptions := Project.ProjectOptions;
{$IFDEF GX_DELPHI2009_UP}
  if Supports(ProjectOptions, IOTAProjectOptionsConfigurations, ProjectOptionsConfigurations)
    and (FConfigs.Count > 0) then begin
    for i := 0 to FConfigs.Count - 1 do begin
      cfg := FConfigs[i] as TConfigEntry;
      for CfgIdx := 0 to ProjectOptionsConfigurations.ConfigurationCount - 1 do begin
        BuildCfg := ProjectOptionsConfigurations.Configurations[CfgIdx];
        if BuildCfg.Name = cfg.FConfigName then begin
{$IFDEF GX_DELPHIXE2_UP}
          if (cfg.FPlatform <> '') then begin
            PlatformCfg := BuildCfg.PlatformConfiguration[cfg.FPlatform];
            PlatformCfg.Value[DCC_UnitSearchPath] := cfg.FPath;
          end else
{$ENDIF}begin
            BuildCfg.Value[DCC_UnitSearchPath] := cfg.FPath;
          end;
        end;
      end;
    end;
  end else
{$ENDIF}begin
    GetData(Path);
    // neither of these work correctly for Delphi 2007. WTF?
    ProjectOptions.Values[OPTION_NAME_UNIT_SEARCH_PATH] := Path;
    ProjectOptions.Values['DCC_UnitSearchPath'] := Path;
  end;
end;

constructor Tf_EditPath.Create(_Owner: TComponent);
var
  Project: IOTAProject;
  ProjectOptions: IOTAProjectOptions;
{$IFDEF GX_DELPHI2009_UP}
  ActiveConfig: string;
{$IFDEF GX_DELPHIXE2_UP}
  ActivePlatform: string;
{$ENDIF}
  ActiveIndex: Integer;
  ProjectOptionsConfigurations: IOTAProjectOptionsConfigurations;
  BaseConfig: IOTABuildConfiguration;
  ActiveCfg: IOTABuildConfiguration;

  procedure HandleConfiguration(_BuildConfig: IOTABuildConfiguration);
  {$IFDEF GX_DELPHIXE2_UP}
  var
    PrjPlatforms: TArray<string>;
    PlatformCfg: IOTABuildConfiguration;
  {$ENDIF}
  var
    i: Integer;
    Child: IOTABuildConfiguration;
    ThisIndex: Integer;
    cfg: TConfigEntry;
    Path: string;
  begin
    Path := _BuildConfig.GetValue(DCC_UnitSearchPath, False);
    cfg := TConfigEntry.Create(_BuildConfig.Name, '', Path);
    FConfigs.Add(cfg);
    ThisIndex := cmb_Target.Items.AddObject(cfg.ListCaption, cfg);
    if _BuildConfig.Name = ActiveConfig then
      ActiveIndex := ThisIndex;
{$IFDEF GX_DELPHIXE2_UP}
    PrjPlatforms := _BuildConfig.Platforms;
    for i := Low(PrjPlatforms) to High(PrjPlatforms) do begin
      PlatformCfg := _BuildConfig.PlatformConfiguration[PrjPlatforms[i]];
      Path := PlatformCfg.GetValue(DCC_UnitSearchPath, False);
      cfg := TConfigEntry.Create(_BuildConfig.Name, PrjPlatforms[i], Path);
      FConfigs.Add(cfg);
      ThisIndex := cmb_Target.Items.AddObject(cfg.ListCaption, cfg);
      if PrjPlatforms[i] = ActivePlatform then
        ActiveIndex := ThisIndex;
    end;
{$ENDIF}
    for i := 0 to _BuildConfig.ChildCount - 1 do begin
      Child := _BuildConfig.Children[i];
      HandleConfiguration(Child);
    end;
  end;
{$ENDIF}

begin
  inherited;
  FConfigs := TObjectList.Create;
  TPanel_BevelNone([p_Top, p_Memo, p_Bottom]);
  FMemo := TSynMemo.Create(Self);
  FMemo.Parent := p_Memo;
  FMemo.Align := alClient;
  FMemo.HideSelection := False;
  FMemo.WordWrap := False;
  FMemo.ActiveLineColor := TGxEnhancedEditor.DefaultActiveLineColor;
  FMemo.RightEdge := 0;
  FMemo.Gutter.Width := 0;
  FMemo.Options := FMemo.Options - [eoScrollPastEol, eoTabsToSpaces] + [eoHideShowScrollbars];
  GxOtaGetEditorFont(FMemo.Font, -1); // Editor font, size reduced by 1 pt.
  TWinControl_SetFocus(FMemo);
  TWinControl_ActivateDropFiles(FMemo, HandleDropFiles);

  if GxOtaTryGetCurrentProject(Project) then begin
    ProjectOptions := Project.ProjectOptions;
{$IFDEF GX_DELPHI2009_UP}
  // IOTAProjectOptionsConfigurations is only declared for Delphi 2009 and up even though build
  // configurations were already introduced in Delphi 2007. I tried to simply copy the interface
  // declarations from Delphi 2009 but it didn't work, Supports returned false.
    if Supports(ProjectOptions, IOTAProjectOptionsConfigurations, ProjectOptionsConfigurations) then begin
      ActiveIndex := 0;
      ActiveCfg := ProjectOptionsConfigurations.ActiveConfiguration;
      if Assigned(ActiveCfg) then
        ActiveConfig := ActiveCfg.Name
      else
        ActiveConfig := '';
{$IFDEF GX_DELPHIXE2_UP}
      ActivePlatform := ProjectOptionsConfigurations.ActivePlatformName;
{$ENDIF}
      BaseConfig := ProjectOptionsConfigurations.BaseConfiguration;
      HandleConfiguration(BaseConfig);
      cmb_Target.DropDownCount := cmb_Target.Items.Count;
      cmb_Target.ItemIndex := ActiveIndex;
      cmb_Target.OnChange(cmb_Target);
    end else
{$ENDIF}begin
      p_Top.Visible := False;
      lb_Iherited.Visible := False;
      spl_Vertical.Visible := False;
      SetData(ProjectOptions.Values[OPTION_NAME_UNIT_SEARCH_PATH], '');
    end;
  end else
    raise Exception.Create('no project available');
end;

destructor Tf_EditPath.Destroy;
begin
  FreeAndNil(FConfigs);
  inherited;
end;

type
  TEditPathExpert = class(TGX_Expert)
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetActionCaption: string; override;
    // optional, but recommended
    function GetHelpString: string; override;
    // optional, defaults to true
    function HasConfigOptions: Boolean; override;
//    // optional if HasConfigOptions returns false
//    procedure Configure; override;
//    // Override to load any configuration settings
//    procedure InternalLoadSettings(_Settings: IExpertSettings); override;
//    // Override to save any configuration settings
//    procedure InternalSaveSettings(_Settings: IExpertSettings); override;
    procedure Execute(Sender: TObject); override;
  end;

{ TEditPathExpert }

constructor TEditPathExpert.Create;
begin
  inherited;

end;

destructor TEditPathExpert.Destroy;
begin

  inherited;
end;

procedure TEditPathExpert.Execute(Sender: TObject);
begin
  Tf_EditPath.Execute(Application);
end;

function TEditPathExpert.GetActionCaption: string;
begin
  Result := 'Edit &Path ...';
end;

function TEditPathExpert.GetHelpString: string;
begin
  Result := 'Needs a help string';
end;

function TEditPathExpert.HasConfigOptions: Boolean;
begin
  Result := False;
end;

constructor TConfigEntry.Create(const _ConfigName, _Platform, _Path: string);
begin
  inherited Create;
  FConfigName := _ConfigName;
  FPlatform := _Platform;
  FPath := _Path;
end;

function TConfigEntry.GetConfigCaption: string;
begin
  if FConfigName = 'Base' then
    Result := sAllConfigurations
  else
    Result := FConfigName + sConfiguration;
end;

function TConfigEntry.GetPlatformCaption: string;
begin
  Result := GxOtaGetPlatformCaption(FPlatform);
end;

function TConfigEntry.TextCaption: string;
begin
{$IFDEF GX_DELPHIXE2_UP}
  Result := GetConfigCaption + ' - ' + GetPlatformCaption;
{$ELSE}
  Result := GetConfigCaption;
{$ENDIF}
end;

function TConfigEntry.ListCaption: string;
begin
  if FPlatform <> '' then
    Result := '    ' + GetPlatformCaption
  else
    Result := GetConfigCaption;
end;

initialization
  RegisterGX_Expert(TEditPathExpert);
end.
