unit GX_MenuActions;

{$I GX_CondDefine.inc}

interface

uses
  GX_Actions, GX_Experts;

type
  IGxMenuActionManager = interface(IUnknown)
    ['{79950A81-D020-11D3-A941-B048DE000000}']
    function GetAlphabetical: Boolean;
    procedure SetAlphabetical(const DoAlphabetize: Boolean);
    function GetPlaceGxMainMenuInToolsMenu: Boolean;

    function RequestMenuExpertAction(Expert: TGX_Expert): IGxAction;
    procedure ArrangeMenuItems;

    property Alphabetical: Boolean read GetAlphabetical write SetAlphabetical;
    property PlaceGxMainMenuInToolsMenu: Boolean read GetPlaceGxMainMenuInToolsMenu;
  end;

function GXMenuActionManager: IGxMenuActionManager;

procedure CreateGXMenuActionManager;
procedure FreeGXMenuActionManager;

implementation

uses
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  SysUtils, Windows, Classes, Graphics, ActnList, Menus,
  GX_GenericClasses, GX_ActionBroker, GX_ConfigurationInfo,
  GX_GExperts, GX_GenericUtils, GX_IdeUtils, GX_OtaUtils;

// We can enable a hack / kludge to get around
// menu shortcut an initialization issue in
// the IDE.
{$DEFINE GX_UseMenuShortCutInitializationWorkAround}

// ****************************************************************************

// Implement a lookup table that allows for "historical"
// ordering of expert menu item entries.

type
  TMenuItemArray = array of TMenuItem;

  TGXMenuActionManager = class(TSingletonInterfacedObject, IGxMenuActionManager)
  private
    FConfigAction: IGxAction;
    FAboutAction: IGxAction;
    FMoreAction: IGxAction;
    FGExpertsTopLevelMenu: TMenuItem;
    procedure ConfigClick(Sender: TObject);
    procedure AboutClick(Sender: TObject);
    {$IFDEF GX_UseMenuShortCutInitializationWorkAround}
      procedure TopLevelMenuClick(Sender: TObject);
    {$ENDIF GX_UseMenuShortCutInitializationWorkAround}
  private
    function GetAlphabetical: Boolean;
    function GetPlaceGxMainMenuInToolsMenu: Boolean;

    procedure SetAlphabetical(const DoAlphabetize: Boolean);

    function CreateAction(const ACaption, AName: string; OnExecute: TNotifyEvent;
      Bitmap: Graphics.TBitmap; ShortCut: TShortCut; ExpertIndex, MenuItemIndex: Integer): IGxAction;
    procedure GetMenuItems(out MenuItems: TMenuItemArray; SkipMoreItems: Boolean = True);
    procedure MoreActionExecute(Sender: TObject);
    procedure SortMenuItems(var MenuItems: TMenuItemArray);
    procedure NormalizeMenuItems;
  protected
    // IGxMenuActionManager
    function RequestMenuExpertAction(Expert: TGX_Expert): IGxAction;
    function GetToolsMenuItem(Main: TMainMenu; out ToolsMenuItem: TMenuItem): Boolean;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure ArrangeMenuItems;

    property Alphabetical: Boolean read GetAlphabetical write SetAlphabetical;
    property PlaceGxMainMenuInToolsMenu: Boolean read GetPlaceGxMainMenuInToolsMenu;
  end;

var
  PrivateGXMenuActionManager: TGXMenuActionManager = nil;

const
  NonExpertItemTagStart = MaxInt - 100;
  MoreItemTag = NonExpertItemTagStart;
  SepItemTag = NonExpertItemTagStart + 1;
  ConfigItemTag = NonExpertItemTagStart + 2;
  AboutItemTag = NonExpertItemTagStart + 3;
  MoreMenuItemName = 'GExpertsMoreMenuItem';


function GXMenuActionManager: IGxMenuActionManager;
begin
  Assert(PrivateGXMenuActionManager <> nil, 'PrivateGXMenuActionManager is nil');
  Result := PrivateGXMenuActionManager;
end;

procedure CreateGXMenuActionManager;
begin
  Assert(PrivateGXMenuActionManager = nil, 'PrivateGXMenuActionManager is not nil upon creation');

  PrivateGXMenuActionManager := TGXMenuActionManager.Create;
end;

procedure FreeGXMenuActionManager;
begin
  // Anything else would be an error in assumptions;
  // nothing fatal, but not good.
  {$IFOPT D+}Assert(PrivateGXMenuActionManager <> nil, 'PrivateGXMenuActionManager is nil');{$ENDIF}

  FreeAndNil(PrivateGXMenuActionManager);
end;

{ TGXMenuActionManager }

constructor TGXMenuActionManager.Create;
resourcestring
  SGxConfigMenu = '&Configuration...';
  SGxAboutMenu = 'About...';
var
  MainMenu: TMainMenu;
  Separator, ToolsMenuItem: TMenuItem;
begin
  inherited Create;

  // Create GExperts drop down menu.
  FGExpertsTopLevelMenu := TMenuItem.Create(nil);
  {$IFDEF GX_UseMenuShortCutInitializationWorkAround}
     FGExpertsTopLevelMenu.OnClick := TopLevelMenuClick;
  {$ENDIF GX_UseMenuShortCutInitializationWorkAround}
  FMoreAction := GxActionBroker.RequestAction('GExpertsMoreAction', nil); // Do not localize.
  FMoreAction.Caption := 'More';
  FMoreAction.OnExecute := MoreActionExecute;

  FGExpertsTopLevelMenu.Caption := 'GE&xperts'; // Do not localize.
  FGExpertsTopLevelMenu.Name := 'GExperts'; // Do not localize.

  // Now insert three menu items that are always appended to
  // the GExperts main menu item.

  // Insert separator.
  Separator := TMenuItem.Create(FGExpertsTopLevelMenu);
  Separator.Caption := '-'; // Do not localize.
  Separator.Name := 'GX_Sep1'; // Do not localize.
  Separator.Tag := SepItemTag;
  FGExpertsTopLevelMenu.Add(Separator);

  // Add hard-coded actions.
  FConfigAction := CreateAction(SGxConfigMenu, 'Configuration', ConfigClick, nil, 0, ConfigItemTag, 1); // Do not localize.
  FAboutAction := CreateAction(SGxAboutMenu, 'About', AboutClick, nil, 0, AboutItemTag, 2); // Do not localize.

  MainMenu := GxOtaGetIdeMainMenu;
  Assert(Assigned(MainMenu), 'MainMenu component not found');

  // Insert GExperts drop down menu.
  if PlaceGxMainMenuInToolsMenu and GetToolsMenuItem(MainMenu, ToolsMenuItem) then
    ToolsMenuItem.Insert(0, FGExpertsTopLevelMenu)
  else
    MainMenu.Items.Insert(MainMenu.Items.Count - 2, FGExpertsTopLevelMenu);

  Assert(FGExpertsTopLevelMenu.Count = 3);
end;

destructor TGXMenuActionManager.Destroy;
begin
  // Release our main menu actions.
  FConfigAction := nil;
  FAboutAction := nil;
  FMoreAction := nil;

  // Free the top-level GExperts menu item.
  // This also frees also actions and contained
  // menu items.
  FreeAndNil(FGExpertsTopLevelMenu);

  inherited Destroy;
end;

procedure TGXMenuActionManager.ConfigClick(Sender: TObject);
begin
  ShowGXConfigurationForm;
end;

procedure TGXMenuActionManager.AboutClick(Sender: TObject);
begin
  ShowGXAboutForm;
end;

{$IFDEF GX_UseMenuShortCutInitializationWorkAround}
procedure TGXMenuActionManager.TopLevelMenuClick(Sender: TObject);
var
  i: Integer;
  Item: TMenuItem;
  AttachedAction: TCustomAction;
  MenuItems: TMenuItemArray;
begin
  GetMenuItems(MenuItems);
  for i := 0 to (Length(MenuItems) - 1) do
  begin
    Item := MenuItems[i];

    if Item.Action is TCustomAction then
    begin
      AttachedAction := TCustomAction(Item.Action);

      if Item.ShortCut <> AttachedAction.ShortCut then
        Item.ShortCut := AttachedAction.ShortCut;
    end;
  end;
end;
{$ENDIF GX_UseMenuShortCutInitializationWorkAround}

function TGXMenuActionManager.GetAlphabetical: Boolean;
begin
  Result := ConfigInfo.AlphabetizeMenu;
end;

procedure TGXMenuActionManager.SetAlphabetical(const DoAlphabetize: Boolean);
begin
  if DoAlphabetize = Self.Alphabetical then
    Exit;

  ConfigInfo.AlphabetizeMenu := DoAlphabetize;
  // ArrangeMenuItems is called later
end;

function TGXMenuActionManager.RequestMenuExpertAction(Expert: TGX_Expert): IGxAction;
begin
  Assert(Expert <> nil, 'Invalid nil Expert parameter for RequestMenuExpertAction');
  // Create the action.
  Result := CreateAction(Expert.GetActionCaption, Expert.GetActionName, Expert.Click,
                         Expert.Bitmap, Expert.ShortCut, Expert.ExpertIndex, 0);
end;

// Create an action and add it to the GExperts menu.
function TGXMenuActionManager.CreateAction(const ACaption, AName: string; OnExecute: TNotifyEvent;
  Bitmap: Graphics.TBitmap; ShortCut: TShortCut; ExpertIndex, MenuItemIndex: Integer): IGxAction;
var
  TempMenuItem: TMenuItem;
begin
  Assert(Assigned(FGExpertsTopLevelMenu));

  // Create the action with a linked menu item
  Result := GxActionBroker.RequestMenuAction(AName, Bitmap);

  TempMenuItem := (Result as IGxMenuAction).AssociatedMenuItem;
  Assert(Assigned(TempMenuItem));

  // Save the associated expert's index into the menu
  // item for later sorting.
  TempMenuItem.Tag := ExpertIndex;

  // Insert the menu item.
  FGExpertsTopLevelMenu.Insert(MenuItemIndex, TempMenuItem);

  Result.Caption := ACaption;
  Result.OnExecute := OnExecute;
  Result.ShortCut := ShortCut;
end;

function TGXMenuActionManager.GetPlaceGxMainMenuInToolsMenu: Boolean;
begin
  Result := ConfigInfo.PlaceGxMainMenuInToolsMenu;
end;

function TGXMenuActionManager.GetToolsMenuItem(Main: TMainMenu; out ToolsMenuItem: TMenuItem): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := Main.Items.Count - 1 downto 0 do
  begin
    if Main.Items[i].Name = 'ToolsMenu' then
    begin
      Result := True;
      ToolsMenuItem := Main.Items[i];
      Break;
    end;
  end;
end;

procedure TGXMenuActionManager.ArrangeMenuItems;

  function NewMoreItem(Num: Integer): TMenuItem;
  begin
    Result := TMenuItem.Create(FGExpertsTopLevelMenu);
    Result.Caption := 'More';
    Result.Action := FMoreAction.GetAction;
    Result.Name := MoreMenuItemName + IntToStr(Num);
    Result.Tag := MoreItemTag;
  end;

var
  i: Integer;
  ScreenRect: TRect;
  ScreenHeight: Integer;
  MaxMenuItems: Integer;
  MoreMenuItem: TMenuItem;
  ParentItem: TMenuItem;
  Item: TMenuItem;
  CurrentIndex: Integer;
  MenuItems: TMenuItemArray;
begin
  {$IFOPT D+} SendDebug('Arranging menu items'); {$ENDIF}
  NormalizeMenuItems;
  GetMenuItems(MenuItems);
  SortMenuItems(MenuItems);
  ScreenRect := GetScreenWorkArea(GetIdeMainForm);
  ScreenHeight := ScreenRect.Bottom - ScreenRect.Top - 75;
  if RunningDelphi7OrGreater then
  begin
    MaxMenuItems := ScreenHeight div GetMainMenuItemHeight;
    MaxMenuItems := Max(8, MaxMenuItems);
  end
  else
    MaxMenuItems := ScreenHeight;

  ParentItem := FGExpertsTopLevelMenu;
  CurrentIndex := 0;

  for i := 0 to Length(MenuItems) - 1 do
  begin
    Item := MenuItems[i];
    if (CurrentIndex = MaxMenuItems - 1) and (i < (Length(MenuItems) - 1)) then
    begin
      MoreMenuItem := NewMoreItem(i);
      ParentItem.Add(MoreMenuItem);
      ParentItem := MoreMenuItem;
      CurrentIndex := 0;
    end;
    if Item.Parent <> ParentItem then
    begin
      Item.Parent.Remove(Item);
      ParentItem.Add(Item);
    end;
    Item.MenuIndex := CurrentIndex;
    Inc(CurrentIndex);
  end;
end;

procedure TGXMenuActionManager.MoreActionExecute(Sender: TObject);
begin
  // This is only here to enable the menu item
end;

procedure TGXMenuActionManager.GetMenuItems(out MenuItems: TMenuItemArray; SkipMoreItems: Boolean);

  procedure AddMenuItemsForParent(Parent: TMenuItem);
  var
    i: Integer;
    Item: TMenuItem;
  begin
    if Parent = nil then
      Exit;
    for i := 0 to Parent.Count - 1 do
    begin
      Item := Parent.Items[i];
      if (not SkipMoreItems) or (Item.Tag <> MoreItemTag) then
      begin
        SetLength(MenuItems, Length(MenuItems) + 1);
        MenuItems[Length(MenuItems) - 1] := Item;
      end;
    end;

    for i := 0 to Parent.Count - 1 do
    begin
      Item := Parent.Items[i];
      if Item.Count > 0 then
        AddMenuItemsForParent(Item);
    end;
  end;

begin
  Assert(Assigned(FGExpertsTopLevelMenu));
  AddMenuItemsForParent(FGExpertsTopLevelMenu);
end;

procedure TGXMenuActionManager.SortMenuItems(var MenuItems: TMenuItemArray);
var
  i, j: Integer;
  TempItem: TMenuItem;
  Condition: Boolean;
begin
  Assert(Assigned(MenuItems));
  for i := 0 to Length(MenuItems) - 1 do
  begin
    for j := i to Length(MenuItems) - 1 do
    begin
      if Alphabetical then
        Condition :=  (MenuItems[j].Tag < NonExpertItemTagStart) and
          (AnsiCompareText(StripHotkey(MenuItems[i].Caption), StripHotkey(MenuItems[j].Caption)) > 0)
      else
        Condition := MenuItems[i].Tag > MenuItems[j].Tag;
      if Condition then
      begin
        TempItem := MenuItems[i];
        MenuItems[i] := MenuItems[j];
        MenuItems[j] := TempItem;
      end;
    end;
  end;
end;

procedure TGXMenuActionManager.NormalizeMenuItems;
var
  MenuItems: TMenuItemArray;
  i: Integer;
  Item: TMenuItem;
begin
  GetMenuItems(MenuItems, False);
  for i := 0 to Length(MenuItems) - 1 do
  begin
    Item := MenuItems[i];
    if Item.Parent <> FGExpertsTopLevelMenu then
    begin
      Item.Parent.Remove(Item);
      FGExpertsTopLevelMenu.Add(Item);
    end;
  end;

  for i := 0 to Length(MenuItems) - 1 do
  begin
    if MenuItems[i].Tag = MoreItemTag then
      MenuItems[i].Free;
  end;
end;

initialization

finalization
  // When Delphi itself crashes just before shutdown (common in D5-D7 when
  // writing the DSK and DOF files), this assertion just compounds problems.
  {$IFOPT D+}Assert(PrivateGXMenuActionManager = nil, 'PrivateGXMenuActionManager is not nil during finalization');{$ENDIF}

end.

