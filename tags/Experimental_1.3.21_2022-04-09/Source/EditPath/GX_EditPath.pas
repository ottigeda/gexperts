unit GX_EditPath;

{$I GX_CondDefine.inc}

interface

{.$DEFINE GX_DELPHI2009_UP}

{$IFDEF GX_DELPHI2009_UP}
// Only Delphi 2009 and up provide the necessary ToolsAPI funtionality
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
  Buttons,
  Actions, // if you get a compile error here, add Actions=ActnList to unit aliases
  ActnList,
  Menus,
  GX_BaseForm,
  GX_SharedImages;

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
  TLineProcessMethod = function(const _s: string): string of object;

type
  TListBox = class(StdCtrls.TListBox)
    ///<summary>
    /// Overrides the default behaviour of the TListbox for mouse wheel scrolling:
    /// Rather than scroll the view without affecting the selected item, select the
    /// next/previous item as if the user pressed the up/down arrow key. </summary>
    function DoMouseWheel(_Shift: TShiftState; _WheelDelta: Integer; _MousePos: TPoint): Boolean; override;
  end;

type
  Tf_EditPath = class(TfmBaseForm)
    p_Bottom: TPanel;
    b_Cancel: TButton;
    b_Ok: TButton;
    p_Main: TPanel;
    p_Memo: TPanel;
    spl_Vertical: TSplitter;
    p_Left: TPanel;
    lb_Target: TListBox;
    p_Top: TPanel;
    l_Target: TLabel;
    spl_Horizontal: TSplitter;
    p_Inherited: TPanel;
    lb_Inherited: TListBox;
    p_RightBottomCaption: TPanel;
    l_Inherited: TLabel;
    p_Right: TPanel;
    sb_MoveUp: TSpeedButton;
    sb_MoveDown: TSpeedButton;
    TheActionList: TActionList;
    act_MoveUp: TAction;
    act_MoveDown: TAction;
    act_PrevTarget: TAction;
    act_NextTarget: TAction;
    act_OK: TAction;
    act_Cancel: TAction;
    act_MakeRelative: TAction;
    act_MakeAbsolute: TAction;
    act_PrependDots: TAction;
    act_RemoveDots: TAction;
    p_RightCaption: TPanel;
    l_UniitSearchPath: TLabel;
    b_MakeRelative: TButton;
    b_MakeAbsolute: TButton;
    b_PrependDots: TButton;
    b_RemoveDots: TButton;
    b_Favorites: TButton;
    pm_Favorites: TPopupMenu;
    pm_Memo: TPopupMenu;
    mi_MemoMoveUp: TMenuItem;
    mi_MemoMoveDown: TMenuItem;
    mi_MemoPrevoiusTarget: TMenuItem;
    mi_MemoNextTarget: TMenuItem;
    N2: TMenuItem;
    act_MoveToBase: TAction;
    mi_MemoMovetobasetarget: TMenuItem;
    N3: TMenuItem;
    mi_MemoPrependDots: TMenuItem;
    mi_MemoRemoveDots: TMenuItem;
    mi_MemoMakeRelative: TMenuItem;
    mi_MemoMakeAbsolute: TMenuItem;
    procedure cmb_TargetChange(Sender: TObject);
    procedure lb_TargetClick(Sender: TObject);
    procedure act_MoveUpExecute(Sender: TObject);
    procedure act_MoveDownExecute(Sender: TObject);
    procedure act_PrevTargetExecute(Sender: TObject);
    procedure act_NextTargetExecute(Sender: TObject);
    procedure act_OKExecute(Sender: TObject);
    procedure act_CancelExecute(Sender: TObject);
    procedure act_MakeRelativeExecute(Sender: TObject);
    procedure act_MakeAbsoluteExecute(Sender: TObject);
    procedure act_PrependDotsExecute(Sender: TObject);
    procedure act_RemoveDotsExecute(Sender: TObject);
    procedure act_MoveToBaseExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure b_FavoritesClick(Sender: TObject);
  private
    FConfigs: TObjectList;
    FMemo: TSynMemo;
    FActiveItem: TConfigEntry;
    FProjectDir: string;
    FFavorites: TStringList;
    procedure SetData(const _Path, _InheritedPath: string);
    procedure GetData(out _Path: string);
    procedure HandleDropFiles(_Sender: TObject; _Files: TStrings);
    procedure HandleTargetChange(_NewCfg: TConfigEntry);
    procedure MemoKeyDown(_Sender: TObject; var _Key: Word; _Shift: TShiftState);
    function doMakeRelative(const _s: string): string;
    procedure ProcessSelectedMemoLines(_ProcessMethod: TLineProcessMethod);
    procedure ProcessAllMemoLines(_ProcessMethod: TLineProcessMethod);
    function doMakeAbsolute(const _s: string): string;
    function doAddDots(const _s: string): string;
    function doDelDots(const _s: string): string;
    procedure InitFavoritesMenu;
    procedure FavoritesPmConfigureClick(_Sender: TObject);
    procedure FavoritesPmHandleFavoriteClick(_Sender: TObject);
    procedure FavoritesEditEntry(_Sender: TWinControl; var _Name, _Value: string; var _OK: Boolean);
    procedure GetSelectedLines(out _StartIdx, _EndIdx: Integer);
  protected
{$IFDEF IDE_IS_HIDPI_AWARE}
    procedure ApplyDpi(_NewDpi: Integer; _NewBounds: PRect); override;
{$ENDIF}
  public
    constructor Create(_Owner: TComponent); override;
    destructor Destroy; override;
  end;
{$ENDIF GX_DELPHI2009_UP}

implementation

{$IFDEF GX_DELPHI2009_UP}

{$R *.dfm}

uses
  Math,
  StrUtils,
  ToolsAPI,
  u_dzTypes,
  u_dzVclUtils,
  u_dzStringUtils,
  u_dzStringArrayUtils,
  u_dzClassUtils,
  u_dzFileUtils,
  SynEditTypes,
  SynUnicode,
  SynEdit,
  GX_OtaUtils,
  GX_Experts,
  GX_EnhancedEditor,
  GX_GExperts,
  GX_GenericUtils,
  GX_ConfigurationInfo,
  GX_IdeSearchPathEnhancer,
  GX_IdeFavoritesList,
  GX_IdeSearchPathFavoriteEdit;

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

{ Tf_EditPath }

const
  DCC_UnitSearchPath = 'DCC_UnitSearchPath';

{$IFDEF IDE_IS_HIDPI_AWARE}
procedure Tf_EditPath.ApplyDpi(_NewDpi: Integer; _NewBounds: PRect);
var
  il: TImageList;
begin
  inherited;

  il := GExpertsInst.GetScaledSharedImages(_NewDpi);
  TheActionList.Images := il;
end;
{$ENDIF}

procedure Tf_EditPath.GetData(out _Path: string);
var
  i: Integer;
  Lines: TStrings;
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
  Lines: TStrings;
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
  Lines: TStrings;
begin
  Dirs := SplitString(_Path, [';']);
  Lines := FMemo.Lines;
  Lines.Clear;
  for i := Low(Dirs) to High(Dirs) do
    if Trim(Dirs[i]) <> '' then
      Lines.Add(Dirs[i]);

  Dirs := SplitString(_InheritedPath, [';']);
  Lines := lb_Inherited.Items;
  Lines.Clear;
  for i := Low(Dirs) to High(Dirs) do
    if Trim(Dirs[i]) <> '' then
      Lines.Add(Dirs[i]);
end;

procedure Tf_EditPath.lb_TargetClick(Sender: TObject);
var
  lb: TListBox absolute Sender;
  cfg: TConfigEntry;
begin
  Assert(lb is TListBox);
  if not TListBox_GetSelectedObject(lb, Pointer(cfg)) then
    Exit; //==>
  HandleTargetChange(cfg);
end;

procedure Tf_EditPath.MemoKeyDown(_Sender: TObject; var _Key: Word; _Shift: TShiftState);
begin
//
end;

procedure Tf_EditPath.cmb_TargetChange(Sender: TObject);
var
  cmb: TComboBox absolute Sender;
  cfg: TConfigEntry;
begin
  Assert(cmb is TComboBox);
  if not TComboBox_GetSelectedObject(cmb, Pointer(cfg)) then
    Exit; //==>
  HandleTargetChange(cfg);
end;

procedure Tf_EditPath.HandleTargetChange(_NewCfg: TConfigEntry);
var
  Path: string;
  i: Integer;
  cfg: TConfigEntry;
  InheritedPath: string;
begin
  if _NewCfg = FActiveItem then
    Exit; //==>

  if Assigned(FActiveItem) then begin
    GetData(Path);
    FActiveItem.FPath := Path;
  end;
  FActiveItem := _NewCfg;
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

procedure Tf_EditPath.act_CancelExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure Tf_EditPath.act_MoveDownExecute(Sender: TObject);
var
  YPos: Integer;
  LineCnt: Integer;
begin
  FMemo.BeginUpdate;
  try
    YPos := FMemo.CaretY;
    LineCnt := FMemo.Lines.Count;
    if YPos < LineCnt then begin
      if YPos = LineCnt - 1 then begin
        // the last line might be empty, if it is, do not swap it with the non empty previous line
        if Trim(FMemo.Lines[YPos]) = '' then
          Exit; //==>
      end;

      FMemo.Lines.Exchange(YPos - 1, YPos);
      FMemo.CaretY := YPos + 1;
    end;
  finally
    FMemo.EndUpdate;
  end;
  TWinControl_SetFocus(FMemo);
end;

procedure Tf_EditPath.act_MoveToBaseExecute(Sender: TObject);
var
  StartIdx: Integer;
  EndIdx: Integer;
  i: Integer;
  sl: TStringList;
  BaseCfg: TConfigEntry;
begin
  inherited;

  if lb_Target.Items.Count = 0 then
    Exit; //==>
  BaseCfg := TConfigEntry(lb_Target.Items.Objects[0]);
  if not Assigned(BaseCfg) then
    Exit; //==>

  GetSelectedLines(StartIdx, EndIdx);
  sl := TStringList.Create;
  try
    sl.StrictDelimiter := True;
    sl.Delimiter := ';';
    sl.DelimitedText := BaseCfg.FPath;
    for i := StartIdx to EndIdx do begin
      sl.Add(FMemo.Lines[i]);
    end;
    BaseCfg.FPath := sl.DelimitedText;
  finally
    FreeAndNil(sl);
  end;
  for i := EndIdx downto StartIdx do begin
    FMemo.Lines.Delete(i);
  end;
end;

procedure Tf_EditPath.act_MoveUpExecute(Sender: TObject);
var
  YPos: Integer;
  LineIdx: Integer;
begin
  FMemo.BeginUpdate;
  try
    YPos := FMemo.CaretY;
    LineIdx := YPos - 1;
    if LineIdx > 0 then begin
      if LineIdx = FMemo.Lines.Count - 1 then begin
        // the last line might be empty, if it is, do not swap it with the non empty previous line
        if Trim(FMemo.Lines[LineIdx]) = '' then
          Exit; //==>
      end;
      FMemo.Lines.Exchange(LineIdx - 1, LineIdx);
      FMemo.CaretY := YPos - 1;
    end;
  finally
    FMemo.EndUpdate;
  end;
  TWinControl_SetFocus(FMemo);
end;

procedure Tf_EditPath.act_NextTargetExecute(Sender: TObject);
begin
  TListBox_SetItemIndex(lb_Target, lb_Target.ItemIndex + 1, True);
end;

procedure Tf_EditPath.act_PrevTargetExecute(Sender: TObject);
begin
  TListBox_SetItemIndex(lb_Target, lb_Target.ItemIndex - 1, True);
end;

procedure Tf_EditPath.ProcessAllMemoLines(_ProcessMethod: TLineProcessMethod);
var
  i: Integer;
begin
  FMemo.BeginUpdate;
  try
    for i := 0 to FMemo.Lines.Count - 1 do begin
      FMemo.Lines[i] := _ProcessMethod(FMemo.Lines[i]);
    end;
  finally
    FMemo.EndUpdate;
  end;

end;

function Tf_EditPath.doMakeAbsolute(const _s: string): string;
begin
  if (_s <> '') and not StartsText('$(BDS)\', _s) then begin
    Result := TFileSystem.ExpandFileNameRelBaseDir(_s, FProjectDir);
  end else
    Result := _s;
end;

procedure Tf_EditPath.b_FavoritesClick(Sender: TObject);
var
  pnt: TPoint;
begin
  pnt := b_Favorites.ClientToScreen(Point(0, b_Favorites.Height));
  pm_Favorites.Popup(pnt.X, pnt.Y);
end;

procedure Tf_EditPath.InitFavoritesMenu;
var
  i: Integer;
  mi: TMenuItem;
  FavName: string;
begin
  pm_Favorites.Items.Clear;
  for i := 0 to FFavorites.Count - 1 do begin
    FavName := FFavorites.Names[i];
    mi := TPopupMenu_AppendMenuItem(pm_Favorites, FavName, FavoritesPmHandleFavoriteClick);
    mi.Tag := i + 1;
  end;
  TPopupMenu_AppendMenuItem(pm_Favorites, 'Configure ...', FavoritesPmConfigureClick);
end;

procedure Tf_EditPath.act_MakeAbsoluteExecute(Sender: TObject);
var
  ProjectFile: string;
begin
  ProjectFile := GxOtaGetCurrentProjectFileName(True);
  if ProjectFile = '' then
    Exit; //==>

  FProjectDir := ExtractFilePath(ProjectFile);
  ProcessAllMemoLines(doMakeAbsolute);
end;

function Tf_EditPath.doMakeRelative(const _s: string): string;
begin
  if (_s <> '') and not StartsText('$(BDS)\', _s) then begin
    Result := ExtractRelativePath(AddSlash(FProjectDir), _s);
  end else
    Result := _s;
end;

procedure Tf_EditPath.act_MakeRelativeExecute(Sender: TObject);
var
  ProjectFile: string;
begin
  ProjectFile := GxOtaGetCurrentProjectFileName(True);
  if ProjectFile = '' then
    Exit; //==>

  FProjectDir := ExtractFilePath(ProjectFile);
  ProcessAllMemoLines(doMakeRelative);
end;

procedure Tf_EditPath.GetSelectedLines(out _StartIdx, _EndIdx: Integer);
begin
  _StartIdx := FMemo.BlockBegin.Line - 1;
  _EndIdx := FMemo.BlockEnd.Line - 1;
  if FMemo.BlockEnd.Char = 1 then
    Dec(_EndIdx);
  if _EndIdx < _StartIdx then
    _EndIdx := _StartIdx;
end;

procedure Tf_EditPath.ProcessSelectedMemoLines(_ProcessMethod: TLineProcessMethod);
var
  i: Integer;
  sl: TStringList;
  StartIdx: Integer;
  EndIdx: Integer;
  SelStart: Integer;
  Line: string;
  SelEnd: Integer;
begin
  if FMemo.Lines.Count = 0 then
    Exit; //==>

  GetSelectedLines(StartIdx, EndIdx);
  sl := TStringList.Create;
  try
    sl.Assign(FMemo.Lines);
    for i := StartIdx to EndIdx do begin
      sl[i] := _ProcessMethod(sl[i]);
    end;
    FMemo.Text := sl.Text;

    SelStart := FMemo.RowColToCharIndex(BufferCoord(1, StartIdx + 1));
    if EndIdx + 1 = FMemo.Lines.Count then begin
      Line := FMemo.Lines[EndIdx];
      SelEnd := FMemo.RowColToCharIndex(BufferCoord(Length(Line) + 1, EndIdx + 1));
    end else
      SelEnd := FMemo.RowColToCharIndex(BufferCoord(1, EndIdx + 2));
    FMemo.SelStart := SelStart;
    FMemo.SelEnd := SelEnd;
  finally
    FreeAndNil(sl);
  end;
end;

function Tf_EditPath.doAddDots(const _s: string): string;
begin
  Result := '..\' + _s;
end;

procedure Tf_EditPath.act_PrependDotsExecute(Sender: TObject);
begin
  ProcessSelectedMemoLines(doAddDots);
end;

function Tf_EditPath.doDelDots(const _s: string): string;
begin
  if LeftStr(_s, 3) = '..\' then begin
    Result := Copy(_s, 4);
  end else
    Result := _s;
end;

procedure Tf_EditPath.act_RemoveDotsExecute(Sender: TObject);
begin
  ProcessSelectedMemoLines(doDelDots);
end;

procedure Tf_EditPath.act_OKExecute(Sender: TObject);
var
  Project: IOTAProject;
  ProjectOptions: IOTAProjectOptions;
  Path: string;
  i: Integer;
  cfg: TConfigEntry;
  CfgIdx: Integer;
  ProjectOptionsConfigurations: IOTAProjectOptionsConfigurations;
  BuildCfg: IOTABuildConfiguration;
{$IFDEF GX_DELPHIXE2_UP}
  PlatformCfg: IOTABuildConfiguration;
{$ENDIF}
begin
  ModalResult := mrOk;
  if not GxOtaTryGetCurrentProject(Project) then
    Exit; //==>

  if Assigned(FActiveItem) then
    GetData(FActiveItem.FPath);

  ProjectOptions := Project.ProjectOptions;
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
  end else begin
    GetData(Path);
    // neither of these work correctly for Delphi 2007. WTF?
    ProjectOptions.Values[OPTION_NAME_UNIT_SEARCH_PATH] := Path;
    ProjectOptions.Values[DCC_UnitSearchPath] := Path;
  end;
end;

procedure Tf_EditPath.FavoritesEditEntry(_Sender: TWinControl; var _Name, _Value: string;
  var _OK: Boolean);
begin
  _OK := Tf_IdeSearchPathFavoriteEdit.Execute(_Sender, _Name, _Value)
end;

procedure Tf_EditPath.FavoritesPmConfigureClick(_Sender: TObject);
resourcestring
  SFavSearchPaths = 'Favorite Search Paths';
begin
  Tf_GxIdeFavoritesList.Execute(Self, SFavSearchPaths, FavoritesEditEntry, FFavorites);
  TGxIdeSearchPathEnhancer.SetSearchPathFavorites(FFavorites);
  InitFavoritesMenu;
end;

procedure Tf_EditPath.FavoritesPmHandleFavoriteClick(_Sender: TObject);
var
  mi: TMenuItem;
  sl: TStringList;
  FavName: string;
begin
  mi := _Sender as TMenuItem;
  sl := TStringList.Create;
  try
    FavName := FFavorites.Names[mi.Tag - 1];
    sl.Delimiter := ';';
    sl.DelimitedText := FFavorites.Values[FavName];
    FMemo.Lines.AddStrings(sl);
  finally
    FreeAndNil(sl);
  end;
end;

procedure Tf_EditPath.FormClose(Sender: TObject; var Action: TCloseAction);
var
  Settings: IExpertSettings;
begin
  Settings := TEditPathExpert.GetSettings;
  Settings.SaveForm('Window', Self);
end;

constructor Tf_EditPath.Create(_Owner: TComponent);
var
  Project: IOTAProject;
  ProjectOptions: IOTAProjectOptions;
  ActiveConfig: string;
{$IFDEF GX_DELPHIXE2_UP}
  ActivePlatform: string;
{$ENDIF}
  ActiveIndex: Integer;
  ProjectOptionsConfigurations: IOTAProjectOptionsConfigurations;
  BaseConfig: IOTABuildConfiguration;
  ActiveCfg: IOTABuildConfiguration;
  Settings: IExpertSettings;

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
    ThisIndex := lb_Target.Items.AddObject(cfg.ListCaption, cfg);
    if _BuildConfig.Name = ActiveConfig then
      ActiveIndex := ThisIndex;
{$IFDEF GX_DELPHIXE2_UP}
    PrjPlatforms := _BuildConfig.Platforms;
    for i := Low(PrjPlatforms) to High(PrjPlatforms) do begin
      PlatformCfg := _BuildConfig.PlatformConfiguration[PrjPlatforms[i]];
      Path := PlatformCfg.GetValue(DCC_UnitSearchPath, False);
      cfg := TConfigEntry.Create(_BuildConfig.Name, PrjPlatforms[i], Path);
      FConfigs.Add(cfg);
      ThisIndex := lb_Target.Items.AddObject(cfg.ListCaption, cfg);
      if PrjPlatforms[i] = ActivePlatform then
        ActiveIndex := ThisIndex;
    end;
{$ENDIF}
    for i := 0 to _BuildConfig.ChildCount - 1 do begin
      Child := _BuildConfig.Children[i];
      HandleConfiguration(Child);
    end;
  end;

begin
  inherited;

  FConfigs := TObjectList.Create;

  TControl_SetMinConstraints(Self);

  TPanel_BevelNone([p_Top, p_Memo, p_Bottom, p_Left, p_Right, p_RightBottomCaption, p_RightCaption]);

  FMemo := TSynMemo.Create(Self);
  FMemo.Parent := p_Memo;
  FMemo.Align := alClient;
  FMemo.HideSelection := False;
  FMemo.WordWrap := False;
  FMemo.ActiveLineColor := TGxEnhancedEditor.DefaultActiveLineColor;
  FMemo.RightEdge := 0;
  FMemo.Gutter.Width := 0;
  FMemo.Options := FMemo.Options - [eoScrollPastEol, eoTabsToSpaces] + [eoHideShowScrollbars];
  FMemo.OnKeyDown := MemoKeyDown;
  FMemo.PopupMenu := pm_Memo;

  l_UniitSearchPath.FocusControl := FMemo;

  GxOtaGetEditorFont(FMemo.Font, -1); // Editor font, size reduced by 1 pt.
  TWinControl_SetFocus(FMemo);

  TWinControl_ActivateDropFiles(FMemo, HandleDropFiles);

  if GxOtaTryGetCurrentProject(Project) then begin
    ProjectOptions := Project.ProjectOptions;
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
      lb_Target.ItemIndex := ActiveIndex;
      lb_TargetClick(lb_Target);
    end else begin
      p_Top.Visible := False;
      lb_Inherited.Visible := False;
      spl_Vertical.Visible := False;
      SetData(ProjectOptions.Values[OPTION_NAME_UNIT_SEARCH_PATH], '');
    end;
  end else
    raise Exception.Create('no project available');

  FFavorites := TStringList.Create;
  TGxIdeSearchPathEnhancer.GetSearchPathFavorites(FFavorites);
  InitFavoritesMenu;

  Settings := TEditPathExpert.GetSettings;
  CenterForm(Self);
  Settings.LoadForm('Window', Self);
end;

destructor Tf_EditPath.Destroy;
begin
  FreeAndNil(FFavorites);
  FreeAndNil(FConfigs);
  inherited;
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
var
  frm: Tf_EditPath;
begin
  frm := Tf_EditPath.Create(Application);
  try
    SetFormIcon(frm);
    frm.ShowModal;
  finally
    frm.Free;
  end;
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

{ TListBox }

function TListBox.DoMouseWheel(_Shift: TShiftState; _WheelDelta: Integer;
  _MousePos: TPoint): Boolean;
var
  Idx: Integer;
begin
  // calculate the index of the item to select
  Idx := ItemIndex - Sign(_WheelDelta);
  if Idx >= Items.Count then
    Idx := Items.Count
  else if Idx < 0 then
    Idx := 0;
  // select it
  ItemIndex := Idx;
  // and simulate a mouse click on it so the selected contents
  Self.Click;

  // tell the caller that the event has been handled
  Result := True;
end;

initialization
  RegisterGX_Expert(TEditPathExpert);
{$ENDIF GX_DELPHI2009_UP}
end.

