unit GX_KbdShortCutBroker;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils, Classes, Menus;

type
  EDuplicateShortCut = class(Exception);

type
  TTriggerMethod = TNotifyEvent;

  IGxKeyboardShortCut = interface(IUnknown)
    ['{D97839F1-CF61-11D3-A93F-D0E07D000000}']
    function GetShortCut: TShortCut;
    procedure SetShortCut(const Value: TShortCut);

    property ShortCut: TShortCut read GetShortCut write SetShortCut;
  end;

  IGxKeyboardShortCutForMenuItem = interface(IGxKeyboardShortCut)
    ['{D97839F2-CF61-11D3-A93F-D0E07D000000}']
    function GetMenuItemName: string;

    property MenuItemName: string read GetMenuItemName;
  end;

  IGxKeyboardShortCutBroker = interface(IUnknown)
    ['{0C1C3891-CFEA-11D3-A940-AA8F24000000}']
    // Request an *IDE-global* keyboard shortcut.
    function RequestOneKeyShortCut(const Trigger: TTriggerMethod; ShortCut: TShortCut = 0): IGxKeyboardShortCut;

    // Request an IDE keyboard shortcut which at the same
    // time is a keyboard shortcut for a menu item.
    function RequestMenuShortCut(const Trigger: TTriggerMethod; const MenuItem: TMenuItem): IGxKeyboardShortCut;

    // If a number of keyboard shortcuts are going to be
    // changed, use BeginUpdate and EndUpdate to delay
    // the updating so that the number of cycles for
    // handler installation / removal is minimized.
    procedure BeginUpdate;
    procedure EndUpdate;

    ///<summary>
    /// @param Immediate = True forces an immediate call of InstallKeyboardBindings
    ///                    False delays it
    procedure DoUpdateKeyBindings(_Immediate: boolean);
  end;

function GxKeyboardShortCutBroker: IGxKeyboardShortCutBroker;

implementation

uses
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  ToolsAPI,
  Forms, Controls, Types, Graphics, Messages, Windows, Contnrs,
  GX_GenericClasses, GX_GExperts, GX_IdeUtils, GX_ConfigurationInfo,
  GX_OtaUtils, GX_TimedCallback;

// First of all we have shared code; in
// particular, we share a large chunk
// from the broker and the basic
// shortcut container.

type
  TGxBaseKeyboardShortCutBroker = class(TSingletonInterfacedObject, IGxKeyboardShortCutBroker)
  private
    FShortCutList: TObjectList;
    FKeyboardName: string;
  private
    procedure NotifyOneShortCutDestruction(AGxKeyboardShortCut: TObject);
    procedure RemoveOneKeyShortCut(AGxOneKeyShortCut: TObject);
    procedure UpdateShortCut(AGxKeyboardShortCut: TObject; NewShortCut: TShortCut);
    procedure AssertNoDuplicateShortCut(const Value: TShortCut); virtual;

    function Updating: Boolean; virtual;
    // Note that while DoUpdateKeyBindings has the magic "key binding"
    // in the identifier name, it is agnostic to the actual method used
    // for binding keys to actions. IOW, DoUpdateKeyBindings does not
    // imply the use if IOTAKeyboardBindingServices (although that
    // is exactly the way it is implemented in a descendant class).
    procedure DoUpdateKeyBindings(_Immediate: boolean); virtual; abstract;
    procedure RemoveShortCut(AShortcutList: TObjectList; AGxKeyboardShortCut: TObject);
  public
    constructor Create;
    destructor Destroy; override;

    procedure BeginUpdate;
    procedure EndUpdate;

    function RequestOneKeyShortCut(const ATrigger: TTriggerMethod; AShortCut: TShortCut = 0): IGxKeyboardShortCut; virtual;
    function RequestMenuShortCut(const ATrigger: TTriggerMethod; const AMenuItem: TMenuItem): IGxKeyboardShortCut; virtual; abstract;
    function GetKeyboardName: string;
  end;

var
  PrivateGxKeyboardShortCutBroker: TGxBaseKeyboardShortCutBroker;

type
  TGxKeyboardShortCut = class(TInterfacedObject)
  protected
    FOwner: TGxBaseKeyboardShortCutBroker;
    FTrigger: TTriggerMethod;
  public
    constructor Create(AOwner: TGxBaseKeyboardShortCutBroker; ATrigger: TTriggerMethod);
    procedure Execute;
  end;

type
  TGxOneKeyShortCut = class(TGxKeyboardShortCut,
                           IGxKeyboardShortCut,
                           IGxKeyboardShortCutForMenuItem)
  private
    FShortCut: TShortCut;
    FMenuItemName: string;
  protected
    // IGxKeyboardShortCut
    function GetShortCut: TShortCut; 
    function GetTrigger: TTriggerMethod;
    procedure SetShortCut(const Value: TShortCut);

    // IGxKeyboardShortCutForMenuItem
    function GetMenuItemName: string;
  public
    constructor Create(AOwner: TGxBaseKeyboardShortCutBroker; ATrigger: TTriggerMethod;
      AShortCut: TShortCut);
    destructor Destroy; override;

    property ShortCut: TShortCut read GetShortCut write SetShortCut;
    property Trigger: TTriggerMethod read GetTrigger write FTrigger;
    property MenuItemName: string read GetMenuItemName write FMenuItemName;
  end;

// ****************************************************************************

function LocateKeyboardShortCut(ShortCutList: TObjectList; KeyCode: TShortCut): TGxOneKeyShortCut;
var
  i: Integer;
  AShortCutItem: TGxOneKeyShortCut;
begin
  Assert(Assigned(ShortCutList));

  Result := nil;

  i := ShortCutList.Count;
  while i > 0 do
  begin
    Dec(i);
    AShortCutItem := ShortCutList[i] as TGxOneKeyShortCut;
    Assert(Assigned(AShortCutItem));
    if AShortCutItem.GetShortCut = KeyCode then
    begin
      Result := AShortCutItem;
      Break;
    end;
  end;
end;

// ****************************************************************************

{ TGxBaseKeyboardShortCutBroker }

procedure TGxBaseKeyboardShortCutBroker.AssertNoDuplicateShortCut(const Value: TShortCut);
resourcestring
  SDuplicateShortCut = 'The shortcut "%s" has already been assigned.';
var
  i: Integer;
  AGxShortCut: TGxOneKeyShortCut;
begin
  Assert(FShortCutList <> nil);

  for i := 0 to FShortCutList.Count-1 do
  begin
    AGxShortCut := FShortCutList[i] as TGxOneKeyShortCut;
    if AGxShortCut.GetShortCut = Value then
    begin
      // The shortcut being passed in has already
      // been claimed by someone else. Complain.
      raise EDuplicateShortCut.CreateFmt(SDuplicateShortCut, [ShortCutToText(Value)]);
    end;
  end;
end;

procedure TGxBaseKeyboardShortCutBroker.BeginUpdate;
begin
  // By default, we do not support BeginUpdate / EndUpdate
end;

constructor TGxBaseKeyboardShortCutBroker.Create;
const
  DoOwnObjects = True;
begin
  inherited Create;

  FShortCutList := TObjectList.Create(not DoOwnObjects);
end;

destructor TGxBaseKeyboardShortCutBroker.Destroy;
begin
  if Assigned(FShortCutList) then
  begin
    Assert(FShortCutList.Count = 0);

    FreeAndNil(FShortCutList);
  end;

  inherited Destroy;
end;

procedure TGxBaseKeyboardShortCutBroker.EndUpdate;
begin
  // By default, we do not support BeginUpdate / EndUpdate
end;

function TGxBaseKeyboardShortCutBroker.GetKeyboardName: string;
begin
  Result := FKeyboardName;
end;

procedure TGxBaseKeyboardShortCutBroker.NotifyOneShortCutDestruction(
  AGxKeyboardShortCut: TObject);
begin
  Assert(Assigned(AGxKeyboardShortCut));

  RemoveOneKeyShortCut(AGxKeyboardShortCut);
end;

procedure TGxBaseKeyboardShortCutBroker.RemoveShortCut(AShortcutList: TObjectList;
  AGxKeyboardShortCut: TObject);
begin
  Assert(AShortCutList <> nil);

  // Since all keyboard shortcuts are exposed and
  // managed through interfaces, they auto-destroy
  // themselves. Hence we must guarantee that the
  // internal list to keep their references does
  // not destroy them, too.
  Assert(AShortCutList.OwnsObjects = False);
  Assert(AShortCutList.Remove(AGxKeyboardShortCut) <> -1);

  if not Updating then
    DoUpdateKeyBindings(False);
end;

procedure TGxBaseKeyboardShortCutBroker.RemoveOneKeyShortCut(AGxOneKeyShortCut: TObject);
begin
  RemoveShortCut(FShortCutList, AGxOneKeyShortCut);
end;

function TGxBaseKeyboardShortCutBroker.RequestOneKeyShortCut(
  const ATrigger: TTriggerMethod; AShortCut: TShortCut): IGxKeyboardShortCut;
var
  AShortCutContainer: TGxKeyboardShortCut;
begin
  Assert(Assigned(ATrigger), 'ATrigger not assigned');
  Assert(AShortCut <> 0, 'AShortCut is 0');

  AShortCutContainer := TGxOneKeyShortCut.Create(Self, ATrigger, AShortCut);
  FShortCutList.Add(AShortCutContainer);

  Result := AShortCutContainer as IGxKeyboardShortCut;
end;

procedure TGxBaseKeyboardShortCutBroker.UpdateShortCut(
  AGxKeyboardShortCut: TObject; NewShortCut: TShortCut);
var
  ListIndex: Integer;
  KbdShortCut: TGxOneKeyShortCut;
begin
  Assert(FShortCutList <> nil);

  ListIndex := FShortCutList.IndexOf(AGxKeyboardShortCut);
  Assert(ListIndex <> -1);

  KbdShortCut := FShortCutList.Items[ListIndex] as TGxOneKeyShortCut;
  if NewShortCut <> KbdShortCut.GetShortCut then
  begin
    // Verify that the new shortcut has not been claimed
    // already; throws an exception if the shortcut has
    // been claimed already.
    // This is disabled because it caused more problems than it helped
    //AssertNoDuplicateShortCut(NewShortCut);

    // We need to directly update the field here,
    // as the class itself always forwards requests
    // for changes to the ShortCut via a property
    // setter to this method.
    KbdShortCut.FShortCut := NewShortCut;

    if not Updating then
      DoUpdateKeyBindings(False);
  end;
end;

function TGxBaseKeyboardShortCutBroker.Updating: Boolean;
begin
  // By default, we do not support BeginUpdate / EndUpdate
  Result := False;
end;

{ TGxKeyboardShortCut }

constructor TGxKeyboardShortCut.Create(AOwner: TGxBaseKeyboardShortCutBroker;
  ATrigger: TTriggerMethod);
begin
  inherited Create;

  Assert(AOwner <> nil);
  Assert(Assigned(ATrigger));

  FOwner := AOwner;
  FTrigger := ATrigger;
end;

procedure TGxKeyboardShortCut.Execute;
begin
  if Assigned(FTrigger) then
    FTrigger(nil);
end;

{ TGxOneKeyShortCut }

constructor TGxOneKeyShortCut.Create(AOwner: TGxBaseKeyboardShortCutBroker;
  ATrigger: TTriggerMethod; AShortCut: TShortCut);
begin
  inherited Create(AOwner, ATrigger);
  FShortCut := AShortCut;
end;

destructor TGxOneKeyShortCut.Destroy;
begin
  if FOwner <> nil then
    FOwner.NotifyOneShortCutDestruction(Self);

  FOwner := nil;

  inherited Destroy;
end;

function TGxOneKeyShortCut.GetMenuItemName: string;
begin
  Result := FMenuItemName;
end;

function TGxOneKeyShortCut.GetShortCut: TShortCut;
begin
  Result := FShortCut;
end;

function TGxOneKeyShortCut.GetTrigger: TTriggerMethod;
begin
  Result := FTrigger;
end;

procedure TGxOneKeyShortCut.SetShortCut(const Value: TShortCut);
begin
  Assert(FOwner <> nil);

  // UpdateShortCut will update the internal
  // status of FShortCut on success.
  FOwner.UpdateShortCut(Self, Value);
end;

// ****************************************************************************

const
  InvalidIndex = -1;

type
  TGxNativeKeyboardShortCutBroker = class(TGxBaseKeyboardShortCutBroker, IGxKeyboardShortCutBroker)
  private
    FKeyboardBindingIndex: Integer;
    FUpdateCount: Integer;
    FInstallingKeyboardBinding: Boolean;
{$IFDEF KEYBOARD_SHORTCUT_BROKER_FIX_ENABLED}
    // This fixes a problem where pressing Insert causes a noticable delay (Bug #147)
    // because it results in multiple uninstalling and installing the GExperts keyboard binding.
    FDoInstallCallback: TTimedCallback;
{$ENDIF}
  private
    procedure InstallKeyboardBindings;
    procedure RemoveKeyboardBindings;
    procedure RemoveRemainingShortCuts;

    //procedure UpdateShortCut(AGxKeyboardShortCut: TObject; NewShortCut: TShortCut); override;
    function Updating: Boolean; override;
    procedure DoUpdateKeyBindings(_Immediate: boolean); override;

    procedure AssertNoDuplicateShortCut(const Value: TShortCut); override;
{$IFDEF KEYBOARD_SHORTCUT_BROKER_FIX_ENABLED}
    procedure DelayedInstallKeyboardBindings(_Sender: TObject);
{$ENDIF}
  public
    function RequestMenuShortCut(const Trigger: TTriggerMethod; const MenuItem: TMenuItem): IGxKeyboardShortCut; override;

    procedure BeginUpdate;
    procedure EndUpdate;
  public
    constructor Create;
    destructor Destroy; override;
  end;

type
  TGxKeyboardBinding = class(TNotifierObject,
                             IOTAKeyboardBinding)
  private
    // IOTAKeyboardBinding
    function GetBindingType: TBindingType;
    function GetDisplayName: string;
    function GetName: string;
    procedure BindKeyboard(const BindingServices: IOTAKeyBindingServices);
  private
    FOwner: TGxNativeKeyboardShortCutBroker;
    procedure KeyBindingHandler(const Context: IOTAKeyContext; KeyCode: TShortCut;
      var BindingResult: TKeyBindingResult);
  public
    constructor Create(AOwner: TGxNativeKeyboardShortCutBroker);
    destructor Destroy; override;
  end;


{ TGxNativeKeyboardShortCutBroker }

procedure TGxNativeKeyboardShortCutBroker.AssertNoDuplicateShortCut(const Value: TShortCut);
begin
  inherited;
  // Here we could try to implement additional
  // sanity checks, where we query the IDE for
  // shortcuts that are in use.
end;

procedure TGxNativeKeyboardShortCutBroker.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

constructor TGxNativeKeyboardShortCutBroker.Create;
begin
  inherited Create;

  FKeyboardBindingIndex := InvalidIndex;
end;

destructor TGxNativeKeyboardShortCutBroker.Destroy;
begin
{$IFDEF KEYBOARD_SHORTCUT_BROKER_FIX_ENABLED}
  FreeAndNil(FDoInstallCallback);
{$ENDIF}

  Assert(not Updating);

  RemoveKeyboardBindings;
  // In an expert dies during destruction, it might still have a shortcut
  {$IFOPT D+} if FShortCutList.Count > 0 then SendDebugError(IntToStr(FShortCutList.Count) + ' shortcuts remain unregistered!'); {$ENDIF}
  RemoveRemainingShortCuts;

  inherited Destroy;
end;

{$IFDEF KEYBOARD_SHORTCUT_BROKER_FIX_ENABLED}
procedure TGxNativeKeyboardShortCutBroker.DelayedInstallKeyboardBindings(_Sender: TObject);
begin
  InstallKeyboardBindings;
end;
{$ENDIF}

procedure TGxNativeKeyboardShortCutBroker.DoUpdateKeyBindings(_Immediate: boolean);
begin
  RemoveKeyboardBindings;
{$IFDEF KEYBOARD_SHORTCUT_BROKER_FIX_ENABLED}
  // In Delphi 10.3 Rio the IDE restarts the keyboard services every time the user presses Insert
  // which causes multiple calls to this method. Since this means that the GExperts keyboard
  // binding was removed and installed every single time this caused a noticable delay (bug #147).
  // This is a workaround for this problem. It works by enabling a 500 ms timer before reinstalling
  // the keyboard bindings again.
  // todo: This is a hack and there should be a better way to handle this.
  if _Immediate then begin
    FreeAndNil(FDoInstallCallback);
    InstallKeyboardBindings;
  end else begin
    if not Assigned(FDoInstallCallback) then
      FDoInstallCallback := TTimedCallback.Create(DelayedInstallKeyboardBindings, 500, False)
    else
      FDoInstallCallback.Reset;
  end;
{$ELSE}
  InstallKeyboardBindings;
{$ENDIF}
end;

procedure TGxNativeKeyboardShortCutBroker.EndUpdate;
begin
  Assert(FUpdateCount >= 1);

  Dec(FUpdateCount);
  if (FUpdateCount = 0) then
  begin
    if RunningDelphi8OrGreater then
    begin
      if Assigned(GExpertsInst(False)) then
        if not GExpertsInst.StartingUp then
          DoUpdateKeyBindings(True);
    end
    else
      DoUpdateKeyBindings(True);
  end
end;

procedure TGxNativeKeyboardShortCutBroker.InstallKeyboardBindings;
var
  IKeyboardServices: IOTAKeyboardServices;
  IKeyboardBinding: IOTAKeyboardBinding;
begin
  if FKeyboardBindingIndex <> InvalidIndex then
    Exit;

  // Starting with Delphi XE3 apparently this gets called again from within
  // the call to IKeyboardServices.AddKeyboardBinding, so FKeyboardBindingIndex
  // isn't set. Therefore this workaround: It prevents the second call
  // and the resulting exception(s)
  if FInstallingKeyboardBinding then
    Exit;

  FInstallingKeyboardBinding := True;
  try
    // XE5, and probably older versions, will AV when you add a keyboard binding
    // (IKeyboardServices.AddKeyboardBinding), when Delphi is shutting down.
    // The AV is in a TMenuItem which is nil.
    if Assigned(Application) and (csDestroying in Application.ComponentState) then
      Exit;

    if not ConfigInfo.EnableKeyboardShortcuts then
      Exit;

    if FShortCutList.Count = 0 then
      Exit;

    IKeyboardServices := GxOtaGetKeyboardServices;
    Assert(Assigned(IKeyboardServices));

    IKeyboardBinding := TGxKeyboardBinding.Create(Self);
    FKeyboardName := IKeyboardBinding.Name;
    try
      FKeyboardBindingIndex := IKeyboardServices.AddKeyboardBinding(IKeyboardBinding);
    except
      on E: Exception do
      begin
        {$IFOPT D+} SendDebugError('Error registering keyboard shortcuts with IDE: ' + E.Message); {$ENDIF}
        raise E.Create('Error registering keyboard shortcuts with IDE: ' +E.Message);
      end;
    end;
  finally
    FInstallingKeyboardBinding := False;
  end;
end;

procedure TGxNativeKeyboardShortCutBroker.RemoveKeyboardBindings;
var
  IKeyboardServices: IOTAKeyboardServices;
begin
  // If the keyboard binding has been
  // installed, remove it - otherwise
  // ignore the request to remove it.
  if FKeyboardBindingIndex <> InvalidIndex then begin
    IKeyboardServices := GxOtaGetKeyboardServices;
    try
      IKeyboardServices.RemoveKeyboardBinding(FKeyboardBindingIndex);
    except
      on E: Exception do
        raise E.Create('Error removing keyboard shortcuts from IDE: ' + E.Message);
    end;
    FKeyboardBindingIndex := InvalidIndex;
  end;
end;

procedure TGxNativeKeyboardShortCutBroker.RemoveRemainingShortCuts;
begin
  BeginUpdate;
  try
    while FShortCutList.Count > 0 do
      RemoveOneKeyShortCut(FShortCutList.Items[0]);
  finally
    EndUpdate;
  end;
end;

function TGxNativeKeyboardShortCutBroker.RequestMenuShortCut(
  const Trigger: TTriggerMethod; const MenuItem: TMenuItem): IGxKeyboardShortCut;
var
  AShortCutContainer: TGxOneKeyShortCut;
begin
  Assert(Assigned(Trigger), 'Trigger not assigned');
  Assert(Assigned(MenuItem), 'MenuItem not assigned');
  Assert(MenuItem.Name <> '', 'MenuItem.Name is empty');

  AShortCutContainer := TGxOneKeyShortCut.Create(Self, Trigger, 0);
  AShortCutContainer.FTrigger := Trigger;
  AShortCutContainer.FMenuItemName := MenuItem.Name;

  Result := AShortCutContainer as IGxKeyboardShortCut;

  FShortCutList.Add(AShortCutContainer);
end;

function TGxNativeKeyboardShortCutBroker.Updating: Boolean;
begin
  Result := (FUpdateCount > 0);
end;

{ TGxKeyboardBinding }

procedure TGxKeyboardBinding.BindKeyboard(const BindingServices: IOTAKeyBindingServices);
const
  DefaultKeyBindingsFlag = kfImplicitShift + kfImplicitModifier + kfImplicitKeypad;
var
  i: Integer;
  KeyboardName: string;
  AShortCutItem: TGxOneKeyShortCut;
begin
  Assert(FOwner <> nil);
  Assert(FOwner.FShortCutList <> nil);

  if RunningDelphi7OrGreater then
    KeyboardName := ''
  else
    KeyboardName := PrivateGxKeyboardShortCutBroker.GetKeyboardName;

  for i := 0 to FOwner.FShortCutList.Count-1 do
  begin
    AShortCutItem := FOwner.FShortCutList[i] as TGxOneKeyShortCut;
    if AShortCutItem.ShortCut <> 0 then
    begin
      BindingServices.AddKeyBinding([AShortCutItem.ShortCut], KeyBindingHandler, nil,
        DefaultKeyBindingsFlag, KeyboardName, AShortCutItem.MenuItemName);
    end;
  end;
end;

constructor TGxKeyboardBinding.Create(AOwner: TGxNativeKeyboardShortCutBroker);
begin
  inherited Create;

  // Store reference to the keyboard shortcut broker here;
  // we will iterate over the broker for requested shortcuts
  // and install every item found there.
  Assert(AOwner <> nil);
  FOwner := AOwner;
end;

destructor TGxKeyboardBinding.Destroy;
begin
  FOwner := nil;
  inherited Destroy;
end;

function TGxKeyboardBinding.GetBindingType: TBindingType;
begin
  Result := btPartial;
end;

function TGxKeyboardBinding.GetDisplayName: string;
resourcestring
  SDisplayName = 'GExperts Shortcuts';
begin
  Result := SDisplayName;
end;

function TGxKeyboardBinding.GetName: string;
begin
  Result := 'GExperts.' + Self.ClassName; // Do not localize.
end;

procedure TGxKeyboardBinding.KeyBindingHandler(
  const Context: IOTAKeyContext; KeyCode: TShortCut;
  var BindingResult: TKeyBindingResult);
var
  AShortCutItem: TGxOneKeyShortCut;
begin
  BindingResult := krUnhandled;

  // Locate the shortcut in our list and dispatch
  // to the Execute function.
  Assert(FOwner <> nil);
  Assert(FOwner.FShortCutList <> nil);

  AShortCutItem := LocateKeyboardShortCut(FOwner.FShortCutList, KeyCode) as TGxOneKeyShortCut;
  if Assigned(AShortCutItem) and Assigned(AShortCutItem.Trigger) then
  begin
    BindingResult := krHandled;
    try
      AShortCutItem.Execute;
    except
      on E: Exception do
      begin
        // If we don't handle these, the hotkey is passed to the editor (inserts
        // a character) or another expert (may show another error, dialog, etc.)
        ApplicationShowException(E);
      end;
    end;
  end;
end;

function GxKeyboardShortCutBroker: IGxKeyboardShortCutBroker;
begin
  if PrivateGxKeyboardShortCutBroker = nil then
    PrivateGxKeyboardShortCutBroker := TGxNativeKeyboardShortCutBroker.Create;

  Result := PrivateGxKeyboardShortCutBroker as IGxKeyboardShortCutBroker;
end;

procedure ReleasePrivateGxShortCutBroker;
begin
  FreeAndNil(PrivateGxKeyboardShortCutBroker);
end;

initialization

finalization
  ReleasePrivateGxShortCutBroker;

end.
