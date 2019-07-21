unit GX_IdeEnhance;

{$I GX_CondDefine.inc}

interface

uses
  Classes, Graphics, ComCtrls, Menus,
  GX_MultiLinePalette, GX_MultilineHost, GX_IdeFormEnhancer;

type
  TIdeEnhancements = class(TObject)
  private
    // Fonts
    FOIFont: TFont;
    FOldOIFont: TFont;
    FOIFontEnabled: Boolean;
    FOICustomFontNames: Boolean;
    // File saving
    FAutoSave: Boolean;
    FAutoSaveInterval: Integer;
    // Menus
    {$IFDEF VER150} // Delphi 7 only
    procedure SetCPTabSelectButton(Value: Boolean);
    procedure TabSelectButtonClick(Sender: TObject);
    {$ENDIF VER150}
    procedure SetAutoSave(const Value: Boolean);
    procedure SetAutoSaveInterval(const Value: Integer);
    procedure SetOIFont(Value: TFont);
    procedure SetOIFontEnabled(Value: Boolean);
    procedure OIFontChange(Sender: TObject);
    procedure SetOICustomFontNames(const Value: Boolean);
  private
    // Component palette
    FCPMultiLine: Boolean;
    FCPHotTracking: Boolean;
    FCPAsButtons: Boolean;
    FCPRaggedRight: Boolean;
    FCPScrollOpposite: Boolean;
    FCPFlatButtons: Boolean;
    FCPTabsInPopup: Boolean;
    FCPTabsInPopupAlphaSort: Boolean;
    FOldCPPopupEvent: TNotifyEvent;
    FCPFontEnabled: Boolean;
    FCPFont: TFont;
    FOldCPFont: TFont;

    FMultiLineTabDockHostManager: TGxMultiLineTabDockHostsManager;
    FMultiLineTabManager: TMultiLineTabManager;

    FIdeFormsAllowResize: Boolean;
    FIdeFormsRememberPosition: Boolean;
    FEnhanceApplicationSettingsDialog: Boolean;
    FEnhanceBuildEventsDialog: Boolean;
    FEnhanceSearchPath: Boolean;
    FEnhanceInstallPackages: Boolean;
    FEnhanceToolProperties: Boolean;
    FEnhanceGotoDialog: Boolean;
    FEnhanceDockForms: Boolean;
    FAutoCloseMessageWindow: Boolean;

    procedure InstallMultiLineComponentTabs;
    procedure RemoveMultiLineComponentTabs;
    procedure AddTabsToPopup(Sender: TObject);
    procedure DeleteCPPopupMenuItems(Popup: TPopupMenu);
    procedure SetActiveTab(Sender: TObject);
    procedure SetCPMultiLine(Value: Boolean);
    procedure SetCPAsButtons(Value: Boolean);
    procedure SetCPTabsInPopup(Value: Boolean);
    procedure SetCPTabsInPopupAlphaSort(Value: Boolean);
    procedure InstallMultiLineHostTabs;
    procedure RemoveMultiLineHostTabs;
    function GetDefaultMultiLineTabDockHost: Boolean;
    procedure SetDefaultMultiLineTabDockHost(const Value: Boolean);
    function GetMultiLineTabDockHost: Boolean;
    procedure SetMultiLineTabDockHost(const Value: Boolean);
    procedure SetCPFont(Value: TFont);
    procedure SetCPFontEnabled(Value: Boolean);
    procedure CPFontChange(Sender: TObject);
    procedure SetCPFlatButtons(const Value: Boolean);
    procedure SetCPRaggedRight(const Value: Boolean);
    procedure SetCPScrollOpposite(const Value: Boolean);
    procedure Remove;
    function ConfigurationKey: string;
    procedure SetEnhanceIDEForms(const Value: Boolean);
    function GetEnhanceIDEForms: Boolean;
    procedure SetEnhanceSearchPath(Value: Boolean);
    procedure SetEnhanceToolProperties(const Value: Boolean);
    procedure SetEnhanceInstallPackages(const Value: Boolean);
    procedure SetEnhanceGotoDialog(const Value: Boolean);
    procedure SetEnhanceDockForms(const Value: Boolean);
    procedure SetIdeFormsAllowResize(const Value: Boolean);
    procedure SetIdeFormsRememberPosition(const Value: Boolean);
    function GetOIHideHotCmds: boolean;
    procedure SetOIHideHotCmds(const Value: Boolean);
    function GetOIHideDescPane: boolean;
    procedure SetOIHideDescPane(const Value: boolean);
    procedure SetEnhanceBuildEventsDialog(const Value: boolean);
    procedure SetEnhanceApplicationSettingsDialog(const Value: boolean);
    procedure SetAutoCloseMessageWindow(const Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Initialize;
    procedure LoadSettings;
    procedure SaveSettings;

    // IDE
    property EnhanceIDEForms: Boolean read GetEnhanceIDEForms write SetEnhanceIDEForms;
    property IdeFormsAllowResize: Boolean read FIdeFormsAllowResize write SetIdeFormsAllowResize;
    property IdeFormsRememberPosition: Boolean read FIdeFormsRememberPosition write SetIdeFormsRememberPosition;

    // Install Packages dialog
    property EnhanceInstallPackages: boolean read FEnhanceInstallPackages write SetEnhanceInstallPackages;
    // Search path
    property EnhanceSearchPath: Boolean read FEnhanceSearchPath write SetEnhanceSearchPath;
    // Tool Options dialog
    property EnhanceToolProperties: Boolean read FEnhanceToolProperties write SetEnhanceToolProperties;
    // Goto dialog
    property EnhanceGotoDialog: Boolean read FEnhanceGotoDialog write SetEnhanceGotoDialog;
    // Dock forms
    property EnhanceDockForms: Boolean read FenhanceDockForms write SetEnhanceDockForms;
    // automatically close messages window
    property AutoCloseMessageWindow: Boolean read FAutoCloseMessageWindow write SetAutoCloseMessageWindow;
    // Build Events dialog
    property  EnhanceBuildEventsDialog: boolean read FEnhanceBuildEventsDialog write SetEnhanceBuildEventsDialog;
    // Application Settings dialog
    property EnhanceApplicationSettingsDialog: boolean read FEnhanceApplicationSettingsDialog write SetEnhanceApplicationSettingsDialog;
    // Fonts
    property OIFontEnabled: Boolean read FOIFontEnabled write SetOIFontEnabled;
    property OIFont: TFont read FOIFont;
    property OICustomFontNames: Boolean read FOICustomFontNames write SetOICustomFontNames;
    // File saving
    property AutoSave: Boolean read FAutoSave write SetAutoSave;
    property AutoSaveInterval: Integer read FAutoSaveInterval write SetAutoSaveInterval;
    property CPFontEnabled: Boolean read FCPFontEnabled write SetCPFontEnabled;
    property CPFont: TFont read FCPFont;
    // Object Inspector
    property OIHideHotCmds: boolean read GetOIHideHotCmds write SetOIHideHotCmds;
    property OIHideDescPane: boolean read GetOIHideDescPane write SetOIHideDescPane;
    // Component palette
    property CPMultiLine: Boolean read FCPMultiLine write SetCPMultiLine;
    property CPHotTracking: Boolean read FCPHotTracking write FCPHotTracking;
    property CPAsButtons: Boolean read FCPAsButtons write SetCPAsButtons;
    property CPFlatButtons: Boolean read FCPFlatButtons write SetCPFlatButtons;
    property CPScrollOpposite: Boolean read FCPScrollOpposite write SetCPScrollOpposite;
    property CPRaggedRight: Boolean read FCPRaggedRight write SetCPRaggedRight;
    property CPTabsInPopup: Boolean read FCPTabsInPopup write SetCPTabsInPopup;
    property CPTabsInPopupAlphaSort: Boolean read FCPTabsInPopupAlphaSort write SetCPTabsInPopupAlphaSort;
    // Multi-line tab dock host
    property MultiLineTabDockHost: Boolean read GetMultiLineTabDockHost write SetMultiLineTabDockHost;
    property DefaultMultiLineTabDockHost: Boolean read GetDefaultMultiLineTabDockHost write SetDefaultMultiLineTabDockHost;
  end;

function IdeEnhancements: TIdeEnhancements;
procedure FreeIdeEnhancements;

implementation

uses
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  {$IFDEF MSWINDOWS} VCLEditors, {$ENDIF MSWINDOWS}
  {$IFDEF VER150} Controls, Buttons, {$ENDIF VER150}
  SysUtils, Forms,
  GX_GenericUtils, GX_GxUtils, GX_IdeUtils, GX_OtaUtils, GX_ConfigurationInfo, 
  GX_IdeSearchPathEnhancer, GX_IdeProjectOptionsEnhancer,
  GX_IdeToolPropertiesEnhancer, GX_IdeInstallPackagesEnhancer, 
  GX_IdeGotoEnhancer, GX_IdeObjectInspectorEnhancer, GX_IdeBuildEventsEnhancer, 
  GX_IdeApplicationSettingsEnhancer, GX_IdeMessageAutoClose, GX_IdeDockFormEnhancer;

{ TIdeEnhancements }

constructor TIdeEnhancements.Create;
begin
  {$IFOPT D+} SendDebug('TIdeEnhancements.Create'); {$ENDIF}
  inherited Create;

  if IsStandAlone then
    Exit;

  FOIFont := TFont.Create;
  FOIFont.OnChange := OIFontChange;

  FCPFont := TFont.Create;
  FCPFont.OnChange := CPFontChange;
end;

procedure TIdeEnhancements.Initialize;
begin
  Assert(Application.MainForm <> nil, 'No MainForm found');

  {$IFOPT D+} SendDebug('Installing IDE Enhancements and loading settings'); {$ENDIF}
  LoadSettings;
  {$IFOPT D+} SendDebug('Loaded IDE Enhancement settings'); {$ENDIF}

  if CPMultiLine then
    InstallMultiLineComponentTabs;
end;

procedure TIdeEnhancements.Remove;
begin
  EnhanceIDEForms := False;
  // MultiLine component palette
  CPMultiLine := False;
  CPAsButtons := False;
  CPHotTracking := False;
  CPTabsInPopup := False;
  CPTabsInPopupAlphaSort := False;
  // Fonts
  CPFontEnabled := False;
  RemoveMultiLineComponentTabs;
  RemoveMultiLineHostTabs;
  OIFontEnabled := False;
  // Don't call SaveSettings after this point
end;

procedure TIdeEnhancements.SetEnhanceIDEForms(const Value: Boolean);
begin
  TIDEFormEnhancements.SetEnabled(Value);
end;

procedure TIdeEnhancements.SetEnhanceInstallPackages(const Value: Boolean);
begin
  FEnhanceInstallPackages := Value;
  TGxIdeInstallPackagesEnhancer.SetEnabled(Value and EnhanceIDEForms);
end;

procedure TIdeEnhancements.SetEnhanceSearchPath(Value: Boolean);
begin
  FEnhanceSearchPath := Value;
  Value := Value and EnhanceIDEForms;
  TGxIdeSearchPathEnhancer.SetEnabled(Value);
  TGxIdeProjectOptionsEnhancer.SetEnabled(Value);
end;

procedure TIdeEnhancements.SetEnhanceToolProperties(const Value: Boolean);
begin
  FEnhanceToolProperties := Value;
  TGxIdeToolPropertiesEnhancer.SetEnabled(Value and EnhanceIDEForms);
end;

procedure TIdeEnhancements.SetEnhanceBuildEventsDialog(const Value: boolean);
begin
  FEnhanceBuildEventsDialog := Value;
  TGxIdeBuildEventsEnhancer.SetEnabled(Value and EnhanceIDEForms);
end;

procedure TIdeEnhancements.SetEnhanceApplicationSettingsDialog(const Value: boolean);
begin
  FEnhanceApplicationSettingsDialog := Value;
  TGxIdeApplicationSettingsEnhancer.SetEnabled(Value and EnhanceIDEForms);
end;

procedure TIdeEnhancements.SetEnhanceGotoDialog(const Value: Boolean);
begin
  FEnhanceGotoDialog := Value;
  TGxIdeGotoEnhancer.SetEnabled(Value and EnhanceIDEForms);
end;

procedure TIdeEnhancements.SetEnhanceDockForms(const Value: Boolean);
begin
  FEnhanceDockForms := Value;
  TGxIdeDockFormEnhancer.SetEnabled(Value and EnhanceIDEForms);
end;

procedure TIdeEnhancements.SetIdeFormsAllowResize(const Value: Boolean);
begin
  FIdeFormsAllowResize := Value;
  TIDEFormEnhancements.SetAllowResize(Value and EnhanceIDEForms);
end;

procedure TIdeEnhancements.SetIdeFormsRememberPosition(const Value: Boolean);
begin
  FIdeFormsRememberPosition := Value;
  TIDEFormEnhancements.SetRememberPosition(Value and EnhanceIDEForms);
end;

destructor TIdeEnhancements.Destroy;
begin
  if not IsStandAlone then begin
    Remove;

    FreeAndNil(FOIFont);
    FreeAndNil(FCPFont);
  end;
  inherited Destroy;
end;

procedure TIdeEnhancements.LoadSettings;
var
  ExpSettings: IExpertSettings;
begin
  Assert(ConfigInfo <> nil, 'No ConfigInfo found');

  // do not localize any of the below items
  ExpSettings := ConfigInfo.GetExpertSettings(ConfigurationKey);
  EnhanceIDEForms := ExpSettings.ReadBool('EnhanceIDEForms', False);
  IdeFormsAllowResize := ExpSettings.ReadBool('IdeFormsAllowResize', False);
  IdeFormsRememberPosition := ExpSettings.ReadBool('IdeFormsRememberPosition', False);
  EnhanceSearchPath := ExpSettings.ReadBool('EnhanceSearchPath', False);
  EnhanceToolProperties := ExpSettings.ReadBool('EnhanceToolProperties', False);
  EnhanceInstallPackages := ExpSettings.ReadBool('EnhanceInstallPackages', False);
  EnhanceGotoDialog := ExpSettings.ReadBool('EnhanceGotoDialog', False);
  EnhanceDockForms := ExpSettings.ReadBool('EnhanceDockForms', False);
  AutoCloseMessageWindow := ExpSettings.ReadBool('AutoCloseMessageWindow', False);
  EnhanceBuildEventsDialog := ExpSettings.ReadBool('EnhanceBuildEventsDialog', False);
  EnhanceApplicationSettingsDialog := ExpSettings.ReadBool('EnhanceApplicationSettingsDialog', False);

  // File saving
  AutoSave := ExpSettings.ReadBool('AutoSave', False);
  AutoSaveInterval := ExpSettings.ReadInteger('AutoSaveInterval', 5);

  // Object Inspector
  ExpSettings.LoadFont('OIFont', OIFont);
  OIFontEnabled := ExpSettings.ReadBool('EnableOIFont', False);
  OICustomFontNames := ExpSettings.ReadBool('OICustomFontNames', False);
  OIHideDescPane := ExpSettings.ReadBool('ObjectInspectorHideDescPane', False);
  OIHideHotCmds := ExpSettings.ReadBool('ObjectInspectorHideHotCmds', False);

  // Component palette
  CPFontEnabled := ExpSettings.ReadBool('EnableCPFont', False);
  ExpSettings.LoadFont('CPFont', CPFont);
  CPMultiLine := ExpSettings.ReadBool('CPMultiLine', False);
  CPScrollOpposite := ExpSettings.ReadBool('CPScrollOpposite', False);
  CPRaggedRight := ExpSettings.ReadBool('CPRaggedRight', False);
  CPFlatButtons := ExpSettings.ReadBool('CPFlatButtons', False);
  CPAsButtons := ExpSettings.ReadBool('CPAsButtons', False);
  CPTabsInPopup := ExpSettings.ReadBool('CPTabsInPopup', False);
  CPTabsInPopupAlphaSort := ExpSettings.ReadBool('CPTabsInPopupAlphaSort', False);
  CPHotTracking := ExpSettings.ReadBool('CPHotTracking', False);

  // MultiLine tab dock host
  MultiLineTabDockHost := ExpSettings.ReadBool('MultiLineTabDockHost', False);
  DefaultMultiLineTabDockHost := ExpSettings.ReadBool('DefaultMultiLineTabDockHost', True);
end;

procedure TIdeEnhancements.SaveSettings;
var
  ExpSettings: IExpertSettings;
begin
  Assert(ConfigInfo <> nil, 'No ConfigInfo found');

  ExpSettings := ConfigInfo.GetExpertSettings(ConfigurationKey);
  ExpSettings.WriteBool('EnhanceIDEForms', EnhanceIDEForms);
  ExpSettings.WriteBool('IdeFormsAllowResize', IdeFormsAllowResize);
  ExpSettings.WriteBool('IdeFormsRememberPosition', IdeFormsRememberPosition);
  ExpSettings.WriteBool('EnhanceSearchPath', EnhanceSearchPath);
  ExpSettings.WriteBool('EnhanceToolProperties', EnhanceToolProperties);
  ExpSettings.WriteBool('EnhanceInstallPackages', EnhanceInstallPackages);
  ExpSettings.WriteBool('EnhanceGotoDialog', EnhanceGotoDialog);
  ExpSettings.WriteBool('EnhanceDockForms', EnhanceDockForms);
  ExpSettings.WriteBool('AutoCloseMessageWindow', AutoCloseMessageWindow);
  ExpSettings.WriteBool('EnhanceBuildEventsDialog', EnhanceBuildEventsDialog);
  ExpSettings.WriteBool('EnhanceApplicationSettingsDialog', EnhanceApplicationSettingsDialog);

  // File saving
  ExpSettings.WriteBool('AutoSave', AutoSave);
  ExpSettings.WriteInteger('AutoSaveInterval', AutoSaveInterval);
  // Fonts
  ExpSettings.WriteBool('EnableOIFont', OIFontEnabled);
  ExpSettings.WriteBool('OICustomFontNames', OICustomFontNames);

  ExpSettings.SaveFont('OIFont', OIFont);

  ExpSettings.WriteBool('ObjectInspectorHideHotCmds', OIHideHotCmds);
  ExpSettings.WriteBool('ObjectInspectorHideDescPane', OIHideDescPane);

  // Component palette
  ExpSettings.SaveFont('CPFont', CPFont);
  ExpSettings.WriteBool('EnableCPFont', CPFontEnabled);
  ExpSettings.WriteBool('CPTabsInPopupAlphaSort', CPTabsInPopupAlphaSort);
  ExpSettings.WriteBool('CPTabsInPopup', CPTabsInPopup);
  ExpSettings.WriteBool('CPMultiLine', CPMultiLine);
  ExpSettings.WriteBool('CPScrollOpposite', CPScrollOpposite);
  ExpSettings.WriteBool('CPRaggedRight', CPRaggedRight);
  ExpSettings.WriteBool('CPHotTracking', CPHotTracking);
  ExpSettings.WriteBool('CPAsButtons', CPAsButtons);
  ExpSettings.WriteBool('CPFlatButtons', CPFlatButtons);

  // MultiLine tab dock host
  ExpSettings.WriteBool('MultiLineTabDockHost', MultiLineTabDockHost);
  ExpSettings.WriteBool('DefaultMultiLineTabDockHost', DefaultMultiLineTabDockHost);
end;

procedure TIdeEnhancements.AddTabsToPopup(Sender: TObject);
var
  CPPopupMenu: TPopupMenu;

  procedure AddPopupMenuItems;
  var
    StartInsertingAt: Integer;
    i: Integer;
    Menu: TMenuItem;
    TabNames: TStringList;
    TabControl: TTabControl;
  begin
    Menu := TMenuItem.Create(nil);
    Menu.Caption := '-';
    Menu.Tag := -1;
    Menu.Name := 'GX_PopupSeparator';
    CPPopupMenu.Items.Add(Menu);

    StartInsertingAt := CPPopupMenu.Items.Count;

    TabControl := GetComponentPaletteTabControl;
    if TabControl <> nil then
    begin
      TabNames := TStringList.Create;
      try
        for i := 0 to TabControl.Tabs.Count - 1 do
          TabNames.AddObject(TabControl.Tabs[i], TObject(i));
        if CPTabsInPopupAlphaSort then
          TabNames.Sort;
        for i := 0 to TabControl.Tabs.Count - 1 do
        begin
          Menu := TMenuItem.Create(nil);
          Menu.Caption := TabNames[i];
          Menu.Tag := -1;
          Menu.Name := 'GX_Palette' + IntToStr(Integer(TabNames.Objects[i]));
          Menu.RadioItem := True;
          Menu.GroupIndex := 99;
          Menu.Checked := Integer(TabNames.Objects[i]) = TabControl.TabIndex;
          Menu.OnClick := SetActiveTab;
          // This allows a max of 20 tabs per column.  Not perfect, but
          // still nicer than menu items disappearing off the screen.
          if (i > 0) and ((StartInsertingAt + i - 1) mod 20 = 0) then
            Menu.Break := mbBarBreak;
          CPPopupMenu.Items.Add(Menu)
        end;
      finally
        FreeAndNil(TabNames);
      end;
    end;
  end;

begin
  if (Sender = nil) or (not (Sender is TPopupMenu)) then
    Exit;
  CPPopupMenu := TPopupMenu(Sender);
  DeleteCPPopupMenuItems(CPPopupMenu);
  if Assigned(FOldCPPopupEvent) then
    FOldCPPopupEvent(Sender);
  AddPopupMenuItems;
end;

procedure TIdeEnhancements.DeleteCPPopupMenuItems(Popup: TPopupMenu);
var
  i: Integer;
  Menu: TMenuItem;
begin
  i := 0;
  while i <= Popup.Items.Count - 1 do
  begin
    if Popup.Items[i].Tag = -1 then
    begin
      Menu := Popup.Items[i];
      Popup.Items.Delete(i);
      FreeAndNil(Menu);
    end
    else
      Inc(i);
  end;
end;

procedure TIdeEnhancements.SetActiveTab(Sender: TObject);
var
  TabControl: TTabControl;
  Tab: string;
  i: Integer;
begin
  TabControl := GetComponentPaletteTabControl;
  if TabControl <> nil then
  begin
    Tab := TMenuItem(Sender).Caption;

    // Compensate for AutoHotKeys
    Tab := StringReplace(Tab, '&', '', [rfReplaceAll]);

    for i := 0 to TabControl.Tabs.Count - 1 do
      if TabControl.Tabs[i] = Tab then
      begin
        TabControl.TabIndex := i;
        TabControl.OnChange(TabControl);
        Break;
      end;
  end;
end;

procedure TIdeEnhancements.SetAutoCloseMessageWindow(const Value: Boolean);
begin
  FAutoCloseMessageWindow := Value;
  TGxMessageAutoClose.SetEnabled(Value and EnhanceIDEForms);
end;

procedure TIdeEnhancements.SetAutoSave(const Value: Boolean);
begin
  // Do something here
  FAutoSave := Value;
end;

procedure TIdeEnhancements.SetAutoSaveInterval(const Value: Integer);
begin
  // Do something here
  FAutoSaveInterval := Value;
end;

{ --- Window menu enhancement --- }

procedure TIdeEnhancements.SetCPMultiLine(Value: Boolean);
var
  CPTabControl: TTabControl;
begin
  {$IFOPT D+} SendDebug('Setting multiline palette to ' + BooleanText(Value)); {$ENDIF}
  if FCPMultiLine <> Value then
  begin
    FCPMultiLine := Value;
    CPTabControl := GetComponentPaletteTabControl;
    if CPTabControl = nil then
    begin
      {$IFOPT D+} SendDebug('Unable to reset OldCPResizeHandler (no tab control)'); {$ENDIF}
      Exit;
    end;
    if FCPMultiLine then
      InstallMultiLineComponentTabs
    else
      RemoveMultiLineComponentTabs;
  end;
end;

procedure TIdeEnhancements.SetCPFlatButtons(const Value: Boolean);
var
  TabControl: TTabControl;
begin
  if FCPFlatButtons <> Value then
  begin
    FCPFlatButtons := Value;
    TabControl := GetComponentPaletteTabControl;
    if TabControl = nil then
      Exit;

    if CPAsButtons then
    begin
      if FCPFlatButtons then
        TabControl.Style := tsFlatButtons
      else
        TabControl.Style := tsButtons;
    end;
  end;
end;

procedure TIdeEnhancements.SetCPAsButtons(Value: Boolean);
var
  TabControl: TTabControl;
begin
  if FCPAsButtons <> Value then
  begin
    FCPAsButtons := Value;
    TabControl := GetComponentPaletteTabControl;
    if TabControl = nil then
      Exit;

    {$IFOPT D+} SendDebug('Removing CP Buttons'); {$ENDIF}
    if Value then
    begin
      if CPFlatButtons then
        TabControl.Style := tsFlatButtons
      else
        TabControl.Style := tsButtons;
    end
    else
      TabControl.Style := tsTabs;
  end;
end;

procedure TIdeEnhancements.SetCPRaggedRight(const Value: Boolean);
var
  TabControl: TTabControl;
begin
  if FCPRaggedRight <> Value then
  begin
    FCPRaggedRight := Value;
    TabControl := GetComponentPaletteTabControl;
    if TabControl = nil then
      Exit;

    TabControl.RaggedRight := FCPRaggedRight;
  end;
end;

procedure TIdeEnhancements.SetCPScrollOpposite(const Value: Boolean);
var
  TabControl: TTabControl;
begin
  TabControl := GetComponentPaletteTabControl;
  if Assigned(TabControl) then
  begin
    if Value <> TabControl.ScrollOpposite then
      TabControl.ScrollOpposite := Value;
    FCPScrollOpposite := TabControl.ScrollOpposite;
  end;
end;

procedure TIdeEnhancements.SetCPTabsInPopupAlphaSort(Value: Boolean);
begin
  if FCPTabsInPopupAlphaSort <> Value then
    FCPTabsInPopupAlphaSort := Value;
end;

{$IFDEF VER150}
procedure TIdeEnhancements.TabSelectButtonClick(Sender: TObject);
var
  Button: TSpeedButton;
  MainForm: TCustomForm;
begin
  if Sender is TSpeedButton then
  begin
    Button := TSpeedButton(Sender);
    MainForm := GetIdeMainForm;
    if not Assigned(Button.PopupMenu) and Assigned(MainForm) then
    begin
      Button.PopupMenu := TPopupMenu.Create(MainForm);
      Button.PopupMenu.OnPopup := AddTabsToPopup;
    end;
    if Assigned(Button.PopupMenu) then
      Button.PopupMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
  end;
end;

procedure TIdeEnhancements.SetCPTabSelectButton(Value: Boolean);
const
  ButtonName = 'GXTabSelectButton';
var
  MainForm: TCustomForm;
  Button: TSpeedButton;
begin
  if Value then
  begin // Create CP select button
    MainForm := GetIdeMainForm;
    if not Assigned(MainForm) then
      Exit;
    if MainForm.FindComponent(ButtonName) <> nil then
      Exit;

    Button := TSpeedButton.Create(MainForm);
    Button.Align := alRight;
    Button.Width := 18;
    Button.Caption := '...';
    Button.Name := ButtonName;
    Button.Parent := GetComponentPaletteTabControl;
    Button.OnClick := TabSelectButtonClick;
  end
  else begin // Remove CP select button
    MainForm := GetIdeMainForm;
    if not Assigned(MainForm) then
      Exit;

    Button := TSpeedButton(MainForm.FindComponent(ButtonName));
    FreeAndNil(Button);
  end;
end;

procedure TIdeEnhancements.SetCPTabsInPopup(Value: Boolean);
begin
  if FCPTabsInPopup <> Value then
  begin
    FCPTabsInPopup := Value;
    SetCPTabSelectButton(Value);
  end;
end;

{$ELSE not VER150}

procedure TIdeEnhancements.SetCPTabsInPopup(Value: Boolean);
var
  CPPopupMenu: TPopupMenu;
begin
  if FCPTabsInPopup <> Value then
  begin
    FCPTabsInPopup := Value;
    CPPopupMenu := GetComponentPalettePopupMenu;
    if CPPopupMenu = nil then
      Exit;
    if Value then
    begin
      FOldCPPopupEvent := CPPopupMenu.OnPopup;
      CPPopupMenu.OnPopup := AddTabsToPopup;
    end
    else
      CPPopupMenu.OnPopup := FOldCPPopupEvent;
  end;
end;

{$ENDIF not VER150}

procedure TIdeEnhancements.SetOIFont(Value: TFont);
var
  OIForm: TCustomForm;
begin
  OIForm := GetObjectInspectorForm;
  if OIForm <> nil then
  begin
    if FOldOIFont = nil then
    begin
      FOldOIFont := TFont.Create;
      FOldOIFont.Assign(OIForm.Font);
    end;
    OIForm.Font.Assign(Value)
  end;
end;

procedure TIdeEnhancements.SetOIFontEnabled(Value: Boolean);
begin
  if FOIFontEnabled <> Value then
  begin
    FOIFontEnabled := Value;
    if Value then
      SetOIFont(OIFont)
    else if FOldOIFont <> nil then
    begin
      SetOIFont(FOldOIFont);
      FreeAndNil(FOldOIFont);
    end;
  end;
end;

procedure TIdeEnhancements.OIFontChange(Sender: TObject);
begin
  {$IFOPT D+} SendDebug('OI font changed'); {$ENDIF}
  if OIFontEnabled then
    SetOIFont(OIFont);
end;

procedure TIdeEnhancements.SetCPFont(Value: TFont);
var
  CPTabControl: TTabControl;
begin
  CPTabControl := GetComponentPaletteTabControl;
  if CPTabControl <> nil then
  begin
    if FOldCPFont = nil then
    begin
      FOldCPFont := TFont.Create;
      FOldCPFont.Assign(CPTabControl.Font);
    end;
    CPTabControl.Font.Assign(Value)
  end;
end;

procedure TIdeEnhancements.SetCPFontEnabled(Value: Boolean);
begin
  if FCPFontEnabled <> Value then
  begin
    FCPFontEnabled := Value;
    if Value then
      SetCPFont(CPFont)
    else if FOldCPFont <> nil then
    begin
      SetCPFont(FOldCPFont);
      FreeAndNil(FOldCPFont);
    end;
  end;
end;

procedure TIdeEnhancements.CPFontChange(Sender: TObject);
begin
  {$IFOPT D+} SendDebug('CP font changed'); {$ENDIF}
  if CPFontEnabled then
    SetCPFont(CPFont);
end;

procedure TIdeEnhancements.InstallMultiLineComponentTabs;
begin
  if GetComponentPaletteTabControl = nil then
    Exit;

  if FMultiLineTabManager = nil then
    FMultiLineTabManager := TMultiLineTabManager.Create(GetIdeMainForm);
end;

procedure TIdeEnhancements.RemoveMultiLineComponentTabs;
begin
  FreeAndNil(FMultiLineTabManager);
end;

procedure TIdeEnhancements.SetDefaultMultiLineTabDockHost(const Value: Boolean);
begin
  GX_MultilineHost.DefaultToMultiLine := Value;
end;

function TIdeEnhancements.GetDefaultMultiLineTabDockHost: Boolean;
begin
  Result := GX_MultilineHost.DefaultToMultiLine;
end;

function TIdeEnhancements.GetEnhanceIDEForms: Boolean;
begin
  Result := TIDEFormEnhancements.GetEnabled;
end;

function TIdeEnhancements.GetMultiLineTabDockHost: Boolean;
begin
  Result := (FMultiLineTabDockHostManager <> nil);
end;

function TIdeEnhancements.GetOIHideDescPane: boolean;
begin
  Result := TGxIdeObjectInspectorEnhancer.GetHideDescriptionPane;
end;

procedure TIdeEnhancements.SetOIHideDescPane(const Value: boolean);
begin
  TGxIdeObjectInspectorEnhancer.SetHideDescriptionPane(Value);
end;

function TIdeEnhancements.GetOIHideHotCmds: boolean;
begin
  Result :=  TGxIdeObjectInspectorEnhancer.GetHideHotCmds;
end;

procedure TIdeEnhancements.SetOIHideHotCmds(const Value: Boolean);
begin
  TGxIdeObjectInspectorEnhancer.SetHideHotCmds(Value);
end;

procedure TIdeEnhancements.SetMultiLineTabDockHost(const Value: Boolean);
begin
  if Value then
    InstallMultiLineHostTabs
  else
    RemoveMultiLineHostTabs;
end;

procedure TIdeEnhancements.InstallMultiLineHostTabs;
begin
  if MultilineTabDockHostPossible and (FMultiLineTabDockHostManager = nil) then
    FMultiLineTabDockHostManager := TGxMultiLineTabDockHostsManager.Create;
end;

procedure TIdeEnhancements.RemoveMultiLineHostTabs;
begin
  FreeAndNil(FMultiLineTabDockHostManager);
end;

procedure TIdeEnhancements.SetOICustomFontNames(const Value: Boolean);
begin
  {$IFDEF MSWINDOWS}
  FontNamePropertyDisplayFontNames := Value;
  {$ENDIF MSWINDOWS}
  FOICustomFontNames := Value;
end;

function TIdeEnhancements.ConfigurationKey: string;
begin
  Result := 'IDEEnhancements';
end;

// Internal Singleton management code

var
  PrivateIdeEnhancements: TIdeEnhancements = nil;
  CanCreate: Boolean = True;

function IdeEnhancements: TIdeEnhancements;
begin
  {$IFOPT D+} SendDebug('Calling IdeEnhancements'); {$ENDIF D+}
  Assert(CanCreate, 'CanCreate not set');

  if PrivateIdeEnhancements = nil then
    PrivateIdeEnhancements := TIdeEnhancements.Create;

  Result := PrivateIdeEnhancements;
end;

procedure FreeIdeEnhancements;
begin
  {$IFOPT D+} SendDebug('FreeIdeEnhancements'); {$ENDIF D+}
  CanCreate := False;

  FreeAndNil(PrivateIdeEnhancements);
end;

initialization
  {$IFOPT D+} SendDebug('Initializing IDE enhancements unit'); {$ENDIF D+}

finalization
  FreeIdeEnhancements;
end.

