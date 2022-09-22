unit GX_ExpertManager;

{$I GX_CondDefine.inc}

interface

uses
  Windows, SysUtils, Classes, ImgList, ImageList, Controls, Forms, Dialogs,
  Menus, ComCtrls, Actions, ActnList, ToolWin,
{$IFDEF IDE_IS_HIDPI_AWARE}
  u_dzDpiScaleUtils,
{$ENDIF}
  DropTarget, DropSource,
  GX_Experts, GX_BaseForm;

type
  TExpertManagerExpert = class;

  TAddExpertToRegistryResult = (aerOK, aerDuplicate, aerNoExpert);

  TfmExpertManager = class(TfmBaseForm)
    lvExperts: TListView;
    pmItems: TPopupMenu;
    mitEnableExpert: TMenuItem;
    mitDisableExpert: TMenuItem;
    mitPopSep1: TMenuItem;
    mitRemoveExpert: TMenuItem;
    mi_PopSep2: TMenuItem;
    mi_MoveUpExpert: TMenuItem;
    mi_MoveDownExpert: TMenuItem;
    StatusBar: TStatusBar;
    Actions: TActionList;
    ToolBar: TToolBar;
    tbnSave: TToolButton;
    tbnRevert: TToolButton;
    tbnSep1: TToolButton;
    tbnEnable: TToolButton;
    tbnDisable: TToolButton;
    tbnSep2: TToolButton;
    tbnAdd: TToolButton;
    tbnRemove: TToolButton;
    tbnSep3: TToolButton;
    tb_MoveUp: TToolButton;
    tb_MoveDown: TToolButton;
    tbnSep4: TToolButton;
    tbnHelp: TToolButton;
    actExpertEnable: TAction;
    actExpertDisable: TAction;
    actExpertAdd: TAction;
    actExpertRemove: TAction;
    actExpertMoveUp: TAction;
    actExpertMoveDown: TAction;
    actFileSave: TAction;
    actFileRevert: TAction;
    actFileExit: TAction;
    actHelpHelp: TAction;
    actHelpContents: TAction;
    actHelpAbout: TAction;
    ilStateImages: TImageList;
    MainMenu: TMainMenu;
    mitFile: TMenuItem;
    mitHelp: TMenuItem;
    mitFileRevertchanges: TMenuItem;
    mitFileSavetoregistry: TMenuItem;
    N1: TMenuItem;
    mitFileExit: TMenuItem;
    mitExpert: TMenuItem;
    mitExpertEnable: TMenuItem;
    mitExpertDisable: TMenuItem;
    mitSep1: TMenuItem;
    mitExpertAdd: TMenuItem;
    mitExpertRemove: TMenuItem;
    mitHelpHelp: TMenuItem;
    mitHelpContents: TMenuItem;
    mitSep2: TMenuItem;
    mi_ExpertMoveUp: TMenuItem;
    mi_ExpertMoveDown: TMenuItem;
    mi_Sep2: TMenuItem;
    mitHelpAbout: TMenuItem;
    procedure FormResize(Sender: TObject);
    procedure actExpertEnableExecute(Sender: TObject);
    procedure actExpertDisableExecute(Sender: TObject);
    procedure actExpertAddExecute(Sender: TObject);
    procedure actExpertRemoveExecute(Sender: TObject);
    procedure actFileSaveExecute(Sender: TObject);
    procedure actFileRevertExecute(Sender: TObject);
    procedure actFileExitExecute(Sender: TObject);
    procedure actHelpHelpExecute(Sender: TObject);
    procedure actHelpContentsExecute(Sender: TObject);
    procedure actHelpAboutExecute(Sender: TObject);
    procedure actExpertDisableUpdate(Sender: TObject);
    procedure actExpertEnableUpdate(Sender: TObject);
    procedure actExpertRemoveUpdate(Sender: TObject);
    procedure actExpertMoveUpExecute(Sender: TObject);
    procedure actExpertMoveDownExecute(Sender: TObject);
    procedure ActionsUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    FExpertManager: TExpertManagerExpert;
    FFileDrop: TDropFileTarget;
    procedure DropFiles(Sender: TObject; ShiftState: TShiftState; Point: TPoint; var Effect: Longint);
    procedure AddExperts(ExpertList: TStrings);
    procedure UpdateControlsState;
    procedure RefreshExpertListControl;
    function ConfirmIfGExperts(const FileName: string): Boolean;
    procedure LoadSettings;
    procedure SaveSettings;
    function TryGetSelected(out _Item: TListItem): boolean;
  protected
{$IFDEF IDE_IS_HIDPI_AWARE}
    // FImageScaler descends from TComponents and gets freed automatically
    FImageScaler: TImageListScaler;
    procedure ApplyDpi(_NewDpi: Integer; _NewBounds: PRect); override;
    procedure ArrangeControls; override;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateWithManager(AOwner: TComponent; Manager: TExpertManagerExpert);
    destructor Destroy; override;
  end;

  TExpertManagerExpert = class(TGX_Expert)
  private
    FInitialExperts: TStrings;
    FCurrentExperts: TStrings;
    FChanged: boolean;
    class function GetExpertsRegKeyName(IsEnabled: Boolean; var Section: string): string;
    class function IsExpertEnabledInList(_Experts: TStrings; _ExpertName: string): Boolean;
    class procedure ReadExpertsFromRegistry(Experts: TStrings; IsEnabled: Boolean);
    class procedure WriteExpertsToRegistry(Experts: TStrings; IsEnabled: Boolean); overload;
    procedure WriteExpertsToRegistry; overload;
    procedure ResetExperts;
    function FindExpert(const _ExpertName: string; out _Idx: integer): boolean;
  public
    class function GetName: string; override;
    class function AddExpertToRegistry(const ExpertName, FileName: string): TAddExpertToRegistryResult;
    class procedure RemoveExpertFromRegistry(const ExpertName: string; IsEnabled: Boolean);

    constructor Create; override;
    destructor Destroy; override;
    procedure Execute(Sender: TObject); override;
    function GetActionCaption: string; override;
    function HasConfigOptions: Boolean; override;

    function WasExpertEnabled(const _ExpertName: string): Boolean;
    function IsExpertEnabled(const _ExpertName: string): Boolean;

    function AddExpert(const _ExpertName, _ExpertDll: string): TAddExpertToRegistryResult;
    procedure RemoveExpert(const _ExpertName: string);
    procedure EnableExpert(const _ExpertName: string);
    procedure DisableExpert(const _ExpertName: string);

    procedure MoveDown(_Idx: Integer);
    procedure MoveUp(_Idx: Integer);

    property InitialExperts: TStrings read FInitialExperts;
    property CurrentExperts: TStrings read FCurrentExperts;
    property Changed: boolean read FChanged;
  end;

procedure ShowExpertManager; {$IFNDEF GX_BCB} export; {$ENDIF GX_BCB}
procedure InstallGExperts(Handle: HWND; InstHandle: HINST; CmdLine: PAnsiChar; CmdShow: Integer); cdecl; {$IFNDEF GX_BCB} export; {$ENDIF GX_BCB}
procedure RemoveGExperts(Handle: HWND; InstHandle: HINST; CmdLine: PAnsiChar; CmdShow: Integer); cdecl; {$IFNDEF GX_BCB} export; {$ENDIF GX_BCB}

implementation

{$R *.dfm}

uses
  CommCtrl,
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  GX_GExperts, GX_GxUtils, GX_GenericUtils, GX_OtaUtils,
  GX_ConfigurationInfo, GX_MessageBox, GX_SharedImages, GX_IdeUtils,
  GX_VerDepConst, u_dzVclUtils;

type
  TGxExpertState = (gesCurrentlyEnabled, gesNextTimeEnabled,
                    gesCurrentlyDisabled, gesNextTimeDisabled,
                    gesInvalid);


  TShowDisableCurrentMessage = class(TGxQuestionBoxAdaptor)
  protected
    function GetMessage: string; override;
  end;

const
  FalsePtr = nil;
  TruePtr = Pointer(1);

function GetListItemState(ListItem: TListItem): TGxExpertState;
begin
  if (ListItem <> nil) and (ListItem.StateIndex in [0..3]) then
    Result := TGxExpertState(ListItem.StateIndex)
  else
    Result := gesInvalid;
end;

{ TfmExpertManager }

procedure TfmExpertManager.actExpertEnableExecute(Sender: TObject);
var
  GxExpertState: TGxExpertState;
  Item: TListItem;
begin
  if not TryGetSelected(Item) then
    Exit; //==>

  GxExpertState := GetListItemState(Item);
  if (GxExpertState in [gesCurrentlyEnabled, gesNextTimeEnabled]) then
    FExpertManager.DisableExpert(Item.Caption)
  else
    FExpertManager.EnableExpert(Item.Caption);
  UpdateControlsState;
end;

procedure TfmExpertManager.actExpertDisableExecute(Sender: TObject);
var
  Item: TListItem;
begin
  if not TryGetSelected(Item) then
    Exit; //==>

  if not ConfirmIfGExperts(Item.SubItems[0]) then
    Exit; //==>

  FExpertManager.DisableExpert(Item.Caption);
  UpdateControlsState;
end;

procedure TfmExpertManager.actExpertAddExecute(Sender: TObject);
var
  Files: TStringList;
begin
  Files := TStringList.Create;
  try
    if ShowOpenDialog('Select expert DLL', 'dll', Files, 'IDE Experts (*.dll)|*.dll') then
      Self.AddExperts(Files);
  finally
    FreeAndNil(Files);
  end;
end;

procedure TfmExpertManager.actExpertRemoveExecute(Sender: TObject);
resourcestring
  SConfirmRemoval = 'Are you sure you want to remove the selected expert?';
var
  Item: TListItem;
begin
  if not TryGetSelected(Item) then
    Exit; //==>

  if MessageDlg(SConfirmRemoval, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    if not ConfirmIfGExperts(Item.SubItems[0]) then
      Exit; //==>

    FExpertManager.RemoveExpert(Item.Caption);
    UpdateControlsState;
  end;
end;

procedure TfmExpertManager.actFileExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfmExpertManager.actFileRevertExecute(Sender: TObject);
begin
  FExpertManager.ResetExperts;
  RefreshExpertListControl;
end;

procedure TfmExpertManager.actFileSaveExecute(Sender: TObject);
begin
  FExpertManager.WriteExpertsToRegistry;
end;

procedure TfmExpertManager.actHelpHelpExecute(Sender: TObject);
begin
  GxContextHelp(Self, 7);
end;

procedure TfmExpertManager.actHelpContentsExecute(Sender: TObject);
begin
  GxContextHelpContents(Self);
end;

procedure TfmExpertManager.actHelpAboutExecute(Sender: TObject);
begin
  ShowGXAboutForm;
end;

constructor TfmExpertManager.Create(AOwner: TComponent); // FI:W525 Missing INHERITED call in constructor
begin
  raise Exception.Create('Call constructor CreateWithManager');
end;

constructor TfmExpertManager.CreateWithManager(AOwner: TComponent; Manager: TExpertManagerExpert);
begin
  FExpertManager := Manager;
  inherited Create(AOwner);

  TControl_SetMinConstraints(Self);

  SetToolbarGradient(ToolBar);
  SetNonModalFormPopupMode(Self);

  FFileDrop := TDropFileTarget.Create(nil);
  FFileDrop.OnDrop := DropFiles;
  FFileDrop.Dragtypes := [dtCopy, dtMove, dtLink];
  FFileDrop.ShowImage := True;
  FFileDrop.Register(lvExperts);

  InitDpiScaler;

  LoadSettings;
  RefreshExpertListControl;
end;

{$IFDEF IDE_IS_HIDPI_AWARE}
procedure TfmExpertManager.ApplyDpi(_NewDpi: Integer; _NewBounds: PRect);
var
  il: TImageList;
begin
  inherited;
  il := GExpertsInst.GetScaledSharedDisabledImages(_NewDpi);
  ToolBar.DisabledImages := il;

  il := GExpertsInst.GetScaledSharedImages(_NewDpi);
  ToolBar.Images := il;
  Actions.Images := il;
  MainMenu.Images := il;

  if not Assigned(FImageScaler) then
    FImageScaler := TImageListScaler.Create(Self, ilStateImages);
  il := FImageScaler.GetScaledList(_NewDpi);
  lvExperts.StateImages := il;
  lvExperts.LargeImages := il;
end;

procedure TfmExpertManager.ArrangeControls;
begin
  inherited;
  TListView_Resize(lvExperts);
end;
{$ENDIF}

destructor TfmExpertManager.Destroy;
begin
  FExpertManager := nil;
  SaveSettings;

  FFileDrop.Unregister;
  FreeAndNil(FFileDrop);

  inherited Destroy;
end;

procedure TfmExpertManager.RefreshExpertListControl;
var
  i: Integer;
  ListItem: TListItem;
  ExpertList: TStrings;
  Items: TListItems;
  ExpertName: string;
  ExpertDll: string;
  SelectedItem: TListItem;
  SelectedIdx: Integer;
begin
  if TryGetSelected(SelectedItem) then
    SelectedIdx := SelectedItem.Index
  else
    SelectedIdx := -1;

  Items := lvExperts.Items;
  Items.BeginUpdate;
  try
    Items.Clear;

    ExpertList := FExpertManager.CurrentExperts;
    for i := 0 to ExpertList.Count - 1 do begin
      ExpertName := ExpertList.Names[i];
      ExpertDll := ExpertList.Values[ExpertName];
      ListItem := Items.Add;
      ListItem.Caption := ExpertName;
      ListItem.SubItems.Add(ExpertDll);
      if FExpertManager.IsExpertEnabled(ExpertName) then begin
        if FExpertManager.WasExpertEnabled(ExpertName) then
          ListItem.StateIndex := Ord(gesCurrentlyEnabled)
        else
          ListItem.StateIndex := Ord(gesNextTimeEnabled);
      end else begin
        if not FExpertManager.WasExpertEnabled(ExpertName) then
          ListItem.StateIndex := Ord(gesCurrentlyDisabled)
        else
          ListItem.StateIndex := Ord(gesNextTimeDisabled);
      end;

    end;
  finally
    Items.EndUpdate;
  end;
  if SelectedIdx <> -1 then
    lvExperts.Selected := Items[SelectedIdx];
end;

function TfmExpertManager.ConfirmIfGExperts(const FileName: string): Boolean;
begin
  Result := True;

  if SameFileName(FileName, ThisDllName) then
    Result := (ShowGxMessageBox(TShowDisableCurrentMessage) = mrYes);
end;

procedure TfmExpertManager.UpdateControlsState;
var
  CurrentItemCaption: string;
  Item: TListItem;
begin
  if TryGetSelected(Item) then
    CurrentItemCaption := Item.Caption
  else
    CurrentItemCaption := '';

  try
    RefreshExpertListControl;
  finally
    if CurrentItemCaption <> '' then
    begin
      Item := lvExperts.FindCaption(0, CurrentItemCaption, False, True, False);
      if Item <> nil then
        Item.Selected := True;
    end;
  end;
end;

procedure TfmExpertManager.AddExperts(ExpertList: TStrings);
resourcestring
  SCouldNotAddExpertDupe = '%s could not be added as an expert.' + sLineBreak
                          + sLineBreak
                          + 'The module you chose is already loaded as an expert DLL.';
  SCouldNotAddExpertInvalid = '%s could not be added as an expert.' + sLineBreak
                            + sLineBreak
                            + 'The module you chose probably is not a valid expert DLL.';
var
  i: Integer;
  ExpertName: string;
  ExpertEntry: string;
begin
  if ExpertList = nil then
    Exit;

  for i := 0 to ExpertList.Count -1 do
  begin
    ExpertEntry := ExpertList[i];

    ExpertName := ChangeFileExt(ExtractFileName(ExpertEntry), '');
    case FExpertManager.AddExpert(ExpertName, ExpertEntry) of
      aerOK:
        UpdateControlsState;
      aerDuplicate:
        MessageDlg(Format(SCouldNotAddExpertDupe, [ExpertEntry]), mtError, [mbOK], 0);
      aerNoExpert:
        MessageDlg(Format(SCouldNotAddExpertInvalid, [ExpertEntry]), mtError, [mbOK], 0);
    end;
  end;
end;

procedure TfmExpertManager.DropFiles(Sender: TObject; ShiftState: TShiftState; Point: TPoint; var Effect: Longint);
begin
  AddExperts(FFileDrop.Files);
end;

procedure TfmExpertManager.LoadSettings;
var
  Settings: IExpertSettings;
begin
  Settings := TExpertManagerExpert.GetSettings;
  // Do not localize.
  Settings.LoadForm('Window', Self);
  //lvExperts.Columns[0].Width := Settings.ReadInteger(SaveKey, 'Col1Width', lvExperts.Columns[0].Width);
  //lvExperts.Columns[1].Width := Settings.ReadInteger(SaveKey, 'Col2Width', lvExperts.Columns[1].Width);
end;

procedure TfmExpertManager.SaveSettings;
var
  Settings: IExpertSettings;
begin
  Settings := TExpertManagerExpert.GetSettings;
  // Do not localize.
  Settings.SaveForm('Window', Self);
  //Settings.WriteInteger(SaveKey, 'Col1Width', lvExperts.Columns[0].Width);
  //Settings.WriteInteger(SaveKey, 'Col2Width', lvExperts.Columns[1].Width);
end;

procedure TfmExpertManager.FormResize(Sender: TObject);
begin
  if lvExperts.HandleAllocated then begin
    ListView_SetColumnWidth(lvExperts.Handle, 0, ColumnTextWidth);
    ListView_SetColumnWidth(lvExperts.Handle, 1, ColumnHeaderWidth);
  end;
end;

{ TExpertManagerExpert }

constructor TExpertManagerExpert.Create;
var
  DisabledExperts: TStringList;
begin
  inherited Create;

  FInitialExperts := TStringList.Create;
  ReadExpertsFromRegistry(FInitialExperts, True);

  DisabledExperts := TStringList.Create;
  try
    ReadExpertsFromRegistry(DisabledExperts, False);
    FInitialExperts.AddStrings(DisabledExperts);
  finally
    FreeAndNil(DisabledExperts);
  end;

  FCurrentExperts := TStringList.Create;
  FCurrentExperts.AddStrings(FInitialExperts);
end;

destructor TExpertManagerExpert.Destroy;
begin
  // Nothing has been added to the ancestor's SaveSettings
  //SaveSettings;

  FreeAndNil(FCurrentExperts);
  FreeAndNil(FInitialExperts);

  inherited Destroy;
end;

function TExpertManagerExpert.GetActionCaption: string;
resourcestring
  SMenuCaption = '&Expert Manager...';
begin
  Result := SMenuCaption;
end;

class function TExpertManagerExpert.GetName: string;
begin
  Result := 'ExpertManager'; // Do not localize.
end;

procedure TExpertManagerExpert.Execute;
var
  frm: TfmExpertManager;
begin
  frm := TfmExpertManager.CreateWithManager(nil, Self);
  try
    SetFormIcon(frm);
    if frm.lvExperts.Items.Count > 0 then
    begin
      frm.lvExperts.Selected := frm.lvExperts.Items[0];
      frm.lvExperts.ItemFocused := frm.lvExperts.Items[0];
    end;
    frm.ShowModal;
  finally
    FreeAndNil(frm);
  end;
  IncCallCount;
end;

procedure TExpertManagerExpert.RemoveExpert(const _ExpertName: string);
var
  Idx: Integer;
begin
  if FindExpert(_ExpertName, Idx) then begin
    FCurrentExperts.Delete(Idx);
    FChanged := True;
  end;
end;

class procedure TExpertManagerExpert.RemoveExpertFromRegistry(const ExpertName: string; IsEnabled: Boolean);
var
  SectionName: string;
  ExpertSetting: TGExpertsSettings;
begin
  ExpertSetting := TGExpertsSettings.Create(GetExpertsRegKeyName(IsEnabled, SectionName));
  try
    ExpertSetting.DeleteKey(SectionName, ExpertName);
  finally
    FreeAndNil(ExpertSetting);
  end;
end;

procedure TExpertManagerExpert.ResetExperts;
begin
  FCurrentExperts.Assign(FInitialExperts);
  FChanged := False;
end;

function TExpertManagerExpert.AddExpert(const _ExpertName, _ExpertDll: string): TAddExpertToRegistryResult;
var
  i: Integer;
  ThisExpertName: string;
begin
  {$IFOPT D+} SendDebug('Adding Expert: '+_ExpertDll); {$ENDIF}

  // set Result here because otherwise Delphi 7 complains (wrongly) that it might not be set
  Result := aerOK;

  for i := 0 to FCurrentExperts.Count - 1 do begin
    ThisExpertName := FCurrentExperts.Names[i];
    if SameText(FCurrentExperts.Values[ThisExpertName], _ExpertDll) then begin
      Result := aerDuplicate;
      Exit; //==>
    end;
  end;

  if not IsValidExpertDll(_ExpertDll) then begin
    Result := aerNoExpert;
    Exit;
  end;

  FCurrentExperts.Values[_ExpertName] := _ExpertDll;
  if FindExpert(_ExpertName, i) then
    FCurrentExperts.Objects[i] := TruePtr;
  FChanged := True;
end;

class function TExpertManagerExpert.AddExpertToRegistry(const ExpertName, FileName: string): TAddExpertToRegistryResult;
var
  RegIni: TGExpertsSettings;
  ExpertList: TStringList;
  i: Integer;
begin
  {$IFOPT D+} SendDebug('Adding Expert: '+FileName); {$ENDIF}

  // set Result here because otherwise Delphi 7 complains (wrongly) that it might not be set
  Result := aerOK;

  // Make sure that this particular expert is not already loaded.
  ExpertList := TStringList.Create;
  try
    ReadExpertsFromRegistry(ExpertList, True); // Get enabled experts
    for i := 0 to ExpertList.Count - 1 do
    begin
      if ExpertList.Values[ExpertList.Names[i]] = FileName then
      begin
        Result := aerDuplicate;
        Exit;
      end;
    end;
  finally
    FreeAndNil(ExpertList);
  end;

  if not IsValidExpertDll(FileName) then begin
    Result := aerNoExpert;
    Exit;
  end;

  RegIni := TGExpertsSettings.Create(ConfigInfo.IdeRootRegistryKey);
  try
    RegIni.WriteString('Experts', ExpertName, FileName); // Do not localize.
  finally
    FreeAndNil(RegIni);
  end;
end;

function TExpertManagerExpert.FindExpert(const _ExpertName: string; out _Idx: integer): boolean;
begin
  _Idx := FCurrentExperts.IndexOfName(_ExpertName);
  Result := (_Idx >= 0);
end;

procedure TExpertManagerExpert.EnableExpert(const _ExpertName: string);
var
  Idx: Integer;
begin
  if FindExpert(_ExpertName, Idx) then begin
    FCurrentExperts.Objects[Idx] := TruePtr;
    FChanged := True;
  end;
end;

procedure TExpertManagerExpert.DisableExpert(const _ExpertName: string);
var
  Idx: Integer;
begin
  if FindExpert(_ExpertName, Idx) then begin
    FCurrentExperts.Objects[Idx] := FalsePtr;
    FChanged := True;
  end;
end;

class function TExpertManagerExpert.GetExpertsRegKeyName(IsEnabled: Boolean; var Section: string): string;
const
  // Do not localize.
  EnabledExpertRegistryLocation = 'Experts';
  DisabledExpertRegistryLocationSection = 'DisabledExperts';
begin
  if IsEnabled then
  begin
    Result := ConfigInfo.IdeRootRegistryKey;
    Section := EnabledExpertRegistryLocation;
  end
  else
  begin
    Result := AddSlash(ConfigInfo.GExpertsIdeRootRegistryKey) + ConfigurationKey;
    Section := DisabledExpertRegistryLocationSection;
  end;
end;

class procedure TExpertManagerExpert.ReadExpertsFromRegistry(Experts: TStrings; IsEnabled: Boolean);
var
  SectionName: string;
  Obj: Pointer;
  i: Integer;
  Settings: TGExpertsSettings;
begin
  Settings := TGExpertsSettings.Create(GetExpertsRegKeyName(IsEnabled, SectionName));
  try
    Settings.ReadSectionValues(SectionName, Experts);
  finally
    Settings.Free;
  end;
  if IsEnabled then
    Obj := TruePtr
  else
    Obj := FalsePtr;
  for i := 0 to Experts.Count - 1 do
    Experts.Objects[i] := Obj;
end;

procedure TExpertManagerExpert.WriteExpertsToRegistry;
begin
  WriteExpertsToRegistry(FCurrentExperts, True);
  WriteExpertsToRegistry(FCurrentExperts, False);
  FChanged := False;
end;

class procedure TExpertManagerExpert.WriteExpertsToRegistry(Experts: TStrings; IsEnabled: Boolean);
var
  SectionName: string;
  Obj: Pointer;
  i: Integer;
  Settings: TGExpertsSettings;
  sl: TStringList;
begin
  if IsEnabled then
    Obj := TruePtr
  else
    Obj := FalsePtr;
  sl := TStringList.Create;
  try
    for i := 0 to Experts.Count - 1 do begin
      if Experts.Objects[i] = Obj then
        sl.Add(Experts[i]);
    end;
    Settings := TGExpertsSettings.Create(GetExpertsRegKeyName(IsEnabled, SectionName));
    try
      Settings.WriteSectionValues(SectionName, sl);
    finally
      Settings.Free;
    end;
  finally
    FreeAndNil(sl);
  end;
end;

procedure InstallGExperts(Handle: HWND; InstHandle: HINST; CmdLine: PAnsiChar; CmdShow: Integer);
var
  Succeeded: Boolean;
begin
  try
    Succeeded := (TExpertManagerExpert.AddExpertToRegistry('GExperts', ThisDllName) = aerOK);
  except
    Succeeded := False;
  end;
  if Succeeded then
    ShowMessage(Format('GExperts has been registered for use in %s', [IDEEnglishName]))
  else begin
    ShowMessage(Format('GExperts could not be registered for use in %s.'#13#10
      + 'Starting the GExperts Expert Manager.', [IDEEnglishName]));
    ShowExpertManager;
  end;
end;

procedure RemoveGExperts(Handle: HWND; InstHandle: HINST; CmdLine: PAnsiChar; CmdShow: Integer);
var
  Succeeded: Boolean;
begin
  try
    TExpertManagerExpert.RemoveExpertFromRegistry('GExperts', true);
    Succeeded := True;
  except
    Succeeded := False;
  end;
  if Succeeded then
    ShowMessage('GExperts has been removed')
  else begin
    ShowMessage('GExperts could not be removed.'#13#10
      + 'Starting the GExperts Expert Manager.');
    ShowExpertManager;
  end;
end;

// Code used by ExpertManager executable - DLL exports

var
  ExpMgr: TExpertManagerExpert = nil;

procedure ShowExpertManager; {$IFNDEF GX_BCB} export; {$ENDIF GX_BCB}
begin
  {$IFOPT D+}SendDebug('Showing expert manager'); {$ENDIF}
  ExpMgr := nil;
  InitSharedResources;
  try
    ExpMgr := TExpertManagerExpert.Create;
    ExpMgr.Execute(nil);
  finally
    FreeAndNil(ExpMgr);
    FreeSharedResources;
  end;
end;

function TExpertManagerExpert.HasConfigOptions: Boolean;
begin
  Result := False;
end;

function TExpertManagerExpert.IsExpertEnabled(const _ExpertName: string): Boolean;
begin
  Result := IsExpertEnabledInList(FCurrentExperts, _ExpertName);
end;

class function TExpertManagerExpert.IsExpertEnabledInList(_Experts: TStrings;
  _ExpertName: string): Boolean;
var
  Idx: Integer;
begin
  Idx :=  _Experts.IndexOfName(_ExpertName);
  Result := (Idx >= 0);
  if Result then
    Result := (_Experts.Objects[Idx] <> nil);
end;

procedure TExpertManagerExpert.MoveDown(_Idx: Integer);
begin
  Assert(_Idx < FCurrentExperts.Count - 1);
  FCurrentExperts.Exchange(_Idx, _Idx + 1);
  FChanged := True;
end;

procedure TExpertManagerExpert.MoveUp(_Idx: Integer);
begin
  Assert(_Idx > 1);
  FCurrentExperts.Exchange(_Idx, _Idx - 1);
  FChanged := True;
end;

function TExpertManagerExpert.WasExpertEnabled(const _ExpertName: string): Boolean;
begin
  Result := IsExpertEnabledInList(FInitialExperts, _ExpertName);
end;

{ TShowDisableCurrentMessage }

function TShowDisableCurrentMessage.GetMessage: string;
resourcestring
  SConfirmGExpertsDisable = 'You are about to disable or remove the GExperts DLL ' +
                            'that contains the expert you are currently using.' + sLineBreak +
                            sLineBreak +
                            'If you proceed, you will not be able to use this Expert Manager '+
                            'the next time the IDE is started.' + sLineBreak +
                            sLineBreak +
                            'Do you want to proceed?';
begin
  Result := SConfirmGExpertsDisable;
end;


procedure TfmExpertManager.actExpertDisableUpdate(Sender: TObject);
var
  GxExpertState: TGxExpertState;
  ActionIsEnabled: Boolean;
  Item: TListItem;
begin
  ActionIsEnabled := False;

  if TryGetSelected(Item) then
  begin
    GxExpertState := GetListItemState(Item);
    if (GxExpertState in [gesCurrentlyEnabled, gesNextTimeEnabled]) then
      ActionIsEnabled := True;
  end;

  (Sender as TCustomAction).Enabled := ActionIsEnabled;
end;

procedure TfmExpertManager.actExpertEnableUpdate(Sender: TObject);
var
  GxExpertState: TGxExpertState;
  ActionIsEnabled: Boolean;
  Item: TListItem;
begin
  ActionIsEnabled := False;

  if TryGetSelected(Item) then
  begin
    GxExpertState := GetListItemState(Item);
    if (GxExpertState in [gesCurrentlyDisabled, gesNextTimeDisabled]) then
      ActionIsEnabled := True;
  end;

  (Sender as TCustomAction).Enabled := ActionIsEnabled;
end;

procedure TfmExpertManager.actExpertMoveDownExecute(Sender: TObject);
var
  Item: TListItem;
  Idx: Integer;
begin
  if not TryGetSelected(Item) then
    Exit; //==>
  Idx := Item.Index;
  if Idx >= lvExperts.Items.Count - 1 then
    Exit; //==>
  FExpertManager.MoveDown(Idx);
  lvExperts.Selected := lvExperts.Items[Idx + 1];
  RefreshExpertListControl;
end;

procedure TfmExpertManager.actExpertMoveUpExecute(Sender: TObject);
var
  Item: TListItem;
  Idx: Integer;
begin
  if not TryGetSelected(Item) then
    Exit; //==>
  Idx := Item.Index;
  if Idx <= 0 then
    Exit; //==>
  FExpertManager.MoveUp(Idx);
  lvExperts.Selected := lvExperts.Items[Idx - 1];
  RefreshExpertListControl;
end;

function TfmExpertManager.TryGetSelected(out _Item: TListItem): Boolean;
begin
  _Item := lvExperts.Selected;
  Result := Assigned(_Item);
end;

procedure TfmExpertManager.actExpertRemoveUpdate(Sender: TObject);
begin
  (Sender as TCustomAction).Enabled := (lvExperts.Selected <> nil);
end;

procedure TfmExpertManager.ActionsUpdate(Action: TBasicAction; var Handled: Boolean);
resourcestring
  SCurrentlyDisabled = 'Expert DLL is currently disabled and not loaded';
  SCurrentlyEnabled  = 'Expert DLL is currently enabled and active';
  SNextTimeEnabled   = 'Expert DLL will be active next time IDE is started';
  SNextTimeDisabled  = 'Expert DLL will be inactive next time IDE is started';

  function GetDescriptiveState(State: TGxExpertState): string;
  begin
    case State of
      gesCurrentlyDisabled: Result := SCurrentlyDisabled;
      gesCurrentlyEnabled:  Result := SCurrentlyEnabled;
      gesNextTimeDisabled:  Result := SNextTimeDisabled;
      gesNextTimeEnabled:   Result := SNextTimeEnabled;
    else
      Result := '';
    end;
  end;

var
  GxExpertState: TGxExpertState;
  Selected: TListItem;
begin
  Selected := lvExperts.Selected;
  if Selected <> nil then
  begin
    GxExpertState := GetListItemState(Selected);
    TStatusBar_SetLongSimpleText(StatusBar, GetDescriptiveState(GxExpertState));
    actExpertMoveUp.Enabled := (Selected.Index > 0);
    actExpertMoveDown.Enabled := (Selected.Index < lvExperts.Items.Count -1);
  end
  else begin
    StatusBar.SimpleText := '';
    actExpertMoveUp.Enabled := False;
    actExpertMoveDown.Enabled := False;
  end;

  Handled := False;
end;

procedure TfmExpertManager.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if FExpertManager.Changed then begin
    case MessageDlg('You have unsaved changes. Do you want to save them?', mtWarning, [mbYes, mbNo, mbCancel], 0) of
      mrYes: FExpertManager.WriteExpertsToRegistry;
      mrNo: ;
    else
      CanClose := False;
    end;
  end;
end;

procedure TfmExpertManager.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    actFileExit.Execute;
    Key := 0;
  end;
end;

procedure TfmExpertManager.FormShow(Sender: TObject);
var
  GxInst : TGExperts;
begin
  // Works around a bug in Delphi 5 under XP where the menu paints white
  GxInst := GExpertsInst(False);
  if Assigned(GxInst) then
    MainMenu.Images := GxInst.GetSharedImages;
end;

initialization
  RegisterGX_Expert(TExpertManagerExpert);
end.

