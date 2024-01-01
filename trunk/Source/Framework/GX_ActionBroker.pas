unit GX_ActionBroker;

{ All editor toolbar buttons will get their actions from this broker.
  This broker also provides transparent access to the Open
  Tools API's ActionList.

  Note: Except for adding a method or two to facilitate notification of
        dynamic menu item enabling and disabling, these interfaces
        should be stable and cover all use cases. }

interface

{$I GX_CondDefine.inc}

uses
  Windows, SysUtils, Classes, ActnList, Graphics, Actions,
  GX_Actions;

type
  IGxActionBroker = interface(IUnknown)
    ['{EC7632E1-D1A3-11D3-A944-DA3A65000000}']
    ///<summary>
    /// Access to all actions available to the broker; this
    /// includes GExperts's own actions as well as the IDE's
    /// actions. Do not hold on to an action instance for
    /// any prolonged period of time. </summary>
    function GetActions(Index: Integer): TContainedAction;
    function GetActionCount: Integer;
    ///<summary>
    /// Finds an action in the list of available actions
    /// Includes all actions registered / requested via
    /// GExperts and the actions contained in the IDE's
    /// action list.
    /// @param ActionName is the name of the action to find, including(!) its category
    /// @Note that by finding an IDE action and querying for its category name,
    ///       it is possible to group a GExperts action into the same category as a
    ///       given IDE action. </summary>
    function FindAction(const ActionName: string): TContainedAction;
    ///<summary>
    /// Finds a GExperts tools action. This is short for calling
    /// FindAction(GenerateActionName(ActionName)) </summary>
    function FindGExpertsAction(const ActionName: string): TContainedAction;
    ///<summary>
    /// Finds a GExperts tools action. This is short for calling
    /// FindAction(GenerateMenuActionName(ActionName)) </summary>
    function FindGExpertsMenuAction(const ActionName: string): TContainedAction;
    ///<summary>
    /// Request and register a newly created action interface
    /// instance that represents an activity that does not live in
    /// the GExperts menu.
    /// After requesting, in particular Caption and OnExecute
    /// need to be set to make the Action "useful" </summary>
    function RequestAction(const ActionName: string; Bitmap: Graphics.TBitmap = nil): IGxAction;
    ///<summary>
    /// Request and register an action interface instance
    /// that represents an activity that lives in the GExperts menu.
    /// After requesting, in particular Caption and OnExecute
    /// need to be set to make the Action "useful" </summary>
    function RequestMenuAction(const ActionName: string; Bitmap: Graphics.TBitmap): IGxMenuAction;
    ///<summary>
    /// Fill Categories with the all of the action categories </summary>
    procedure GetCategories(Categories: TStrings);
    ///<summary>
    /// Generates the name for a menu action as set by RequestMenuAction
    /// by prefixing GExpertsActionCategory ('GExperts')  </summary>
    function GenerateMenuActionName(const AActionName: string): string;
    ///<summary>
    /// Generates the name for a menu action as set by RequestAction
    /// (by prefixing GExpertsActionCategory ('GExperts') + GxGenericActionQualifier ('Tools') </summary>
    function GenerateActionName(const AActionName: string): string;
  end;


// Get an instance to the GExperts action broker.
function GxActionBroker: IGxActionBroker;

const  // Do not localize.
  GxGenericActionQualifier = 'Tools';

resourcestring
  SNoButtonCategory = '(None)';
  SAllButtonsCategory = '(All)';

implementation

uses
  Forms, Menus,
  ToolsAPI,
  Rescaler,
  GX_GxUtils, GX_IdeUtils, GX_OtaUtils, GX_KbdShortCutBroker,
  GX_GenericClasses, GX_GenericUtils, GX_DbugIntf;

type
  // Special action that implements menu actions.
  TGxMenuAction = class(TGxCustomAction, IUnknown, IGxAction, IGxMenuAction)
  private
    FAssociatedMenuItem: TMenuItem;
    procedure SetShortCut(Value: TShortCut); {$ifdef GX_VER240_up} override; {$endif}
  protected
    function GetAssociatedMenuItem: TMenuItem;
  public
    constructor Create(_Owner: TComponent; const _Name: string); reintroduce;
  end;

type
  TGxToolsAction = class(TGxCustomAction, IUnknown, IGxAction)
  private
    procedure SetShortCut(Value: TShortCut); {$ifdef GX_VER240_up} override; {$endif}
    procedure doOnExecute(Sender: TObject);
  end;

type
  TGxActionBroker = class(TSingletonInterfacedObject, IGxActionBroker)
  private
    function GetIdeActionList: TCustomActionList;
    function GetActionOwner: TCustomForm;
    procedure RegisterActionWithIde(const AAction: TCustomAction; Bitmap: Graphics.TBitmap);
  protected
    // IGxActionBroker
    function GetActions(Index: Integer): TContainedAction;
    function GetActionCount: Integer;
    function FindAction(const Name: string): TContainedAction;
    function FindGExpertsAction(const ActionName: string): TContainedAction;
    function FindGExpertsMenuAction(const ActionName: string): TContainedAction;
    function RequestAction(const ActionName: string; Bitmap: Graphics.TBitmap = nil): IGxAction;
    function RequestMenuAction(const ActionName: string; Bitmap: Graphics.TBitmap): IGxMenuAction;
    procedure GetCategories(Categories: TStrings);
    function GenerateMenuActionName(const AActionName: string): string;
    function GenerateActionName(const AActionName: string): string;
  public
    constructor Create;
    destructor Destroy; override;
  end;

var
  PrivateGxActionBroker: TGxActionBroker;

function GxActionBroker: IGxActionBroker;
begin
  if not Assigned(PrivateGxActionBroker) then
    PrivateGxActionBroker := TGxActionBroker.Create;

  Result := PrivateGxActionBroker;
end;

procedure FreeGxActionBroker;
begin
  FreeAndNil(PrivateGxActionBroker);
end;

{ TGxActionBroker }

constructor TGxActionBroker.Create;
begin
  inherited Create;
end;

destructor TGxActionBroker.Destroy;
begin
  inherited Destroy;
end;

function TGxActionBroker.FindAction(const Name: string): TContainedAction;
var
  TempAction: TContainedAction;
  ActionList: TCustomActionList;
  i: Integer;
begin
  Result := nil;

  ActionList := GetIdeActionList;
  Assert(Assigned(ActionList));

  for i := 0 to ActionList.ActionCount-1 do
  begin
    TempAction := ActionList.Actions[i];
    Assert(Assigned(TempAction));

    if SameText(TempAction.Name, Name) then
    begin
      Result := TempAction;
      Break;
    end;
  end;
end;

function TGxActionBroker.FindGExpertsAction(const ActionName: string): TContainedAction;
begin
  Result := FindAction(GenerateActionName(ActionName));
end;

function TGxActionBroker.FindGExpertsMenuAction(const ActionName: string): TContainedAction;
begin
  Result := FindAction(GenerateMenuActionName(ActionName));
end;

function TGxActionBroker.GenerateActionName(const AActionName: string): string;
begin
  Result := GExpertsActionCategory + GxGenericActionQualifier + AActionName;
end;

function TGxActionBroker.GenerateMenuActionName(const AActionName: string): string;
begin
  Result := GExpertsActionCategory + AActionName;
end;

function TGxActionBroker.GetActionCount: Integer;
begin
  Result := GetIdeActionList.ActionCount;
end;

function TGxActionBroker.GetActionOwner: TCustomForm;
begin
  Result := GetIdeMainForm;
end;

function TGxActionBroker.GetActions(Index: Integer): TContainedAction;
begin
  Result := GetIdeActionList.Actions[Index];
end;

function TGxActionBroker.GetIdeActionList: TCustomActionList;
var
  NTAServices: INTAServices;
begin
  Assert(Assigned(BorlandIDEServices));
  NTAServices := BorlandIDEServices as INTAServices;

  Assert(Assigned(NTAServices));
  Result := NTAServices.ActionList;

  Assert(Assigned(Result));
end;

function CreateScaledBitmap(Bitmap: Graphics.TBitmap): Graphics.TBitmap;
const
  RequiredWidth = 16;
  RequiredHeight = 16;
var
  R: TRect;
  TempBitmap: Graphics.TBitmap;
  w, h: Integer;
begin
  w := Bitmap.Width;
  h := Bitmap.Height;
  Result := Graphics.TBitmap.Create;
  if (w = RequiredWidth) and (h = RequiredHeight) then begin
    Result.Assign(Bitmap);
    Exit; //==>
  end;

  // TempBitmap stores a copy of the bitmap but with a transparent color
  // of clBtnFace.  This prevents the rescaling of the image edges
  // from being discolored by the (usually odd) transparent color.
  TempBitmap := Graphics.TBitmap.Create;
  try
    TempBitmap.Height := h;
    TempBitmap.Width := w;
    TempBitmap.Canvas.Brush.Color := clBtnFace;
    TempBitmap.Transparent := True;
    R := Rect(0, 0, w + 1, h + 1);
    TempBitmap.Canvas.FillRect(R);
    Bitmap.Transparent := True;
    TempBitmap.Canvas.Draw(0, 0, Bitmap);
    Result.Width := RequiredWidth;
    Result.Height := RequiredHeight;
    Result.Transparent := True;
    if not Rescaler.Rescale(TempBitmap, Result, False) then
      Result.Canvas.StretchDraw(Rect(0, 0, Result.Width, Result.Height), TempBitmap);
  finally
    FreeAndNil(TempBitmap);
  end;
end;

procedure TGxActionBroker.RegisterActionWithIde(const AAction: TCustomAction; Bitmap: Graphics.TBitmap);
const
  GxBitmapSuffix = 'GxImage'; // Do not localize.
var
  NTAServices: INTAServices;
  ReadyBitmap: Graphics.TBitmap;

  BitmapName: string;
begin
  if IsStandAlone then
    Exit;

  Assert(Assigned(AAction));
  AAction.ActionList := GetIdeActionList;

  if Assigned(Bitmap) then
  begin
    NTAServices := BorlandIDEServices as INTAServices;
    Assert(Assigned(NTAServices));

    ReadyBitmap := CreateScaledBitmap(Bitmap);
    try
      BitmapName := AAction.Name + GxBitmapSuffix;
      {$IFDEF GX_VER170_up}
      ReadyBitmap.Transparent := False; // Prevent invisible enabled icons in XE6 (disabled ones might still be invisible/ghosted)
      {$ENDIF}
      if ReadyBitmap.Transparent then
        AAction.ImageIndex := NTAServices.AddMasked(ReadyBitmap, ReadyBitmap.TransparentColor, BitmapName)
      else
        AAction.ImageIndex := NTAServices.AddMasked(ReadyBitmap, GXTransparencyColor, BitmapName);
    finally
      FreeAndNil(ReadyBitmap);
    end;
  end;
end;

function TGxActionBroker.RequestAction(const ActionName: string; Bitmap: Graphics.TBitmap = nil): IGxAction;
var
  GxToolsAction: TGxToolsAction;
begin
  Assert(IsValidIdent(ActionName));

  GxToolsAction := TGxToolsAction.Create(GetActionOwner, GenerateActionName(ActionName));

  RegisterActionWithIde(GxToolsAction, Bitmap);

  Result := GxToolsAction;
end;

function TGxActionBroker.RequestMenuAction(const ActionName: string; Bitmap: Graphics.TBitmap): IGxMenuAction;
var
  GxMenuAction: TGxMenuAction;
begin
  Assert(IsValidIdent(ActionName));

  GxMenuAction := TGxMenuAction.Create(GetActionOwner, GenerateMenuActionName(ActionName));

  RegisterActionWithIde(GxMenuAction, Bitmap);

  Result := GxMenuAction;
end;

procedure TGxActionBroker.GetCategories(Categories: TStrings);
var
  i: Integer;
  Category: string;
begin
  Assert(Assigned(Categories));
  Categories.Clear;
  for i := 0 to GxActionBroker.GetActionCount - 1 do
  begin
    Category := GxActionBroker.GetActions(i).Category;
    if Trim(Category) = '' then
      Category := SNoButtonCategory;
    EnsureStringInList(Categories, Category);
  end;
end;

{ TGxMenuAction }

constructor TGxMenuAction.Create(_Owner: TComponent; const _Name: string);
const
  MenuItemAppendix = '_MenuItem'; // Do not localize.
begin
  inherited Create(_Owner, _Name);

  FAssociatedMenuItem := TMenuItem.Create(Self);
  FAssociatedMenuItem.Name := _Name + MenuItemAppendix;
  FAssociatedMenuItem.Action := Self;
end;

function TGxMenuAction.GetAssociatedMenuItem: TMenuItem;
begin
  Result := FAssociatedMenuItem;
end;

procedure TGxMenuAction.SetShortCut(Value: TShortCut);
begin
  if Assigned(IdeShortCut) and (IdeShortCut.ShortCut <> Value) then
    IdeShortCut := nil;  // Unregisters the shortcut with the IDE

  if Value <> 0 then begin
    if Assigned(FAssociatedMenuItem) and not Assigned(IdeShortCut) then
    begin
      IdeShortCut := GxKeyboardShortCutBroker.RequestMenuShortCut(OnExecute, FAssociatedMenuItem);

      Assert(Assigned(IdeShortCut));
      IdeShortCut.ShortCut := Value;
    end;
  end;

  inherited;
end;

{ TGxToolsAction }

procedure TGxToolsAction.doOnExecute(Sender: TObject);
begin
  if Assigned(OnExecute) then
    OnExecute(Sender);
end;

procedure TGxToolsAction.SetShortCut(Value: TShortCut);
begin
  // Not necessary under Delphi 5/6 since the callbacks never happen anyway
  if RunningDelphi7OrGreater then
  begin
    if Assigned(IdeShortCut) and (IdeShortCut.ShortCut <> Value) then
      IdeShortCut := nil;  // Unregisters the shortcut with the IDE

    if Value <> 0 then begin
      if not Assigned(IdeShortCut) then
      begin
        {$IFOPT D+} if not Assigned(OnExecute) then
          SendDebugError(Self.ClassName + '(' + Self.Caption + ') tried to register a shortcut but OnExecute was not assigned.'); {$ENDIF}
        IdeShortCut := GxKeyboardShortCutBroker.RequestOneKeyShortCut(doOnExecute, Value);

        Assert(Assigned(IdeShortCut));
      end;
    end;
  end;

  inherited SetShortCut(Value);
end;

initialization

finalization

  FreeGxActionBroker;

end.

