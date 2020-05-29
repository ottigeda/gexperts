unit GX_MenusForEditorExpert;

{$I GX_CondDefine.inc}

interface

uses
  Menus, ActnList, GX_Experts;

type
  TMenusForEditorExperts = class(TGX_Expert)
  private
    procedure PopulatePopupMenu(const PopupMenu: TPopupMenu);
  protected
    procedure UpdateAction(Action: TCustomAction); override;
    function SupportsSubmenu: Boolean;
  public
    function GetActionCaption: string; override;
    class function GetName: string; override;
    function GetDisplayName: string; override;
    function HasConfigOptions: Boolean; override;
    function HasSubmenuItems: Boolean; override;
    function HasCallCount: Boolean; override;
    procedure CreateSubMenuItems(MenuItem: TMenuItem); override;
    procedure Execute(Sender: TObject); override;
    procedure ShowPopup(Sender: TObject);
  end;

implementation

uses
  SysUtils, Windows, Controls,
  GX_GExperts, GX_OtaUtils, GX_ActionBroker, GX_IdeUtils,
  GX_EditorExpert, GX_EditorExpertManager, ComCtrls, Forms;

var
  InternalPopupMenu: TPopupMenu;

function GetInternalPopupMenu: TPopupMenu;
const
  MenuName = 'GxMenusForEditorExpertsInternalPopupMenu';
begin
  if not Assigned(InternalPopupMenu) then
  begin
    InternalPopupMenu := NewPopupMenu(nil, MenuName, paCenter, False, []);
{$IFNDEF GX_VER320_up}
    // assigninig icons has redraw problems in themed IDEs (Delphi 10.3 and 10.2)
    // todo: Figure out the real problem and assign them again
    InternalPopupMenu.Images := GxOtaGetIdeImageList;
{$ENDIF}
  end;

  Result := InternalPopupMenu;
end;

procedure ReleaseInternalPopupMenu;
begin
  FreeAndNil(InternalPopupMenu);
end;

{ TMenusForEditorExperts }

procedure TMenusForEditorExperts.Execute(Sender: TObject);
var
  IsToolButton: Boolean;
begin
  IsToolButton := Assigned(Sender) and (Sender is TCustomAction) and ((Sender as TCustomAction).ActionComponent is TToolButton);
  if SupportsSubmenu and (not IsToolButton) and Assigned(Sender) then
  begin
    // The submenu items perform all actions
  end
  else begin
    ShowPopup(Sender);
  end;
end;

procedure TMenusForEditorExperts.ShowPopup(Sender: TObject);
var
  MousePosition: TPoint;
  APopupMenu: TPopupMenu;
begin
  MousePosition := Mouse.CursorPos;
  APopupMenu := GetInternalPopupMenu;
  Assert(Assigned(APopupMenu));
  PopulatePopupMenu(APopupMenu);

// this seems to work for theming the menu, but I won't bother right now
//  If Supports(BorlandIDEServices, IOTAIDEThemingServices, ITS) Then begin
//    If ITS.IDEThemingEnabled Then
//    ITS.ApplyTheme(APopupMenu);
//  end;

  APopupMenu.Popup(MousePosition.x, MousePosition.y);
end;

// Note: Partially duplicated below
procedure TMenusForEditorExperts.CreateSubMenuItems(MenuItem: TMenuItem);
var
  i: Integer;
  AGExpertsInstance: TGExperts;
  AEditorExpertManager: TGxEditorExpertManager;
  AEditorExpert: TEditorExpert;

  ExpertMenuEntry: TMenuItem;
begin
  inherited;
  Assert(Assigned(MenuItem));
  MenuItem.Clear;

  AGExpertsInstance := GExpertsInst;
  Assert(Assigned(AGExpertsInstance));

  AEditorExpertManager := AGExpertsInstance.EditorExpertManager;
  // If editor experts are not enabled, then the editor
  // expert manager is not present; exit if this is the case.
  if not Assigned(AEditorExpertManager) then
    Exit;

  for i := 0 to AEditorExpertManager.EditorExpertCount-1 do
  begin
    AEditorExpert := AEditorExpertManager.EditorExpertList[i];
    Assert(Assigned(AEditorExpert));

    if AEditorExpert.Active then
    begin
      ExpertMenuEntry := TMenuItem.Create(MenuItem);
      ExpertMenuEntry.Action := GxActionBroker.FindAction(AEditorExpert.GetActionName);

      MenuItem.Add(ExpertMenuEntry);
    end;
  end;
end;

procedure TMenusForEditorExperts.PopulatePopupMenu(const PopupMenu: TPopupMenu);

  procedure ClearMenuItems(AMenu: TMenu);
  begin
    Assert(Assigned(AMenu));
    AMenu.Items.Clear;
  end;

var
  i: Integer;
  AGExpertsInstance: TGExperts;
  AEditorExpertManager: TGxEditorExpertManager;
  AEditorExpert: TEditorExpert;

  ExpertMenuEntry: TMenuItem;
begin
  Assert(Assigned(PopupMenu));
  ClearMenuItems(PopupMenu);

  AGExpertsInstance := GExpertsInst;
  Assert(Assigned(AGExpertsInstance));

  AEditorExpertManager := AGExpertsInstance.EditorExpertManager;
  // If editor experts are not enabled, then the editor
  // expert manager is not present; exit if this is the case.
  if not Assigned(AEditorExpertManager) then
    Exit;

  for i := 0 to AEditorExpertManager.EditorExpertCount-1 do
  begin
    AEditorExpert := AEditorExpertManager.EditorExpertList[i];
    Assert(Assigned(AEditorExpert));

    if AEditorExpert.Active then
    begin
      ExpertMenuEntry := TMenuItem.Create(PopupMenu);
      ExpertMenuEntry.Action := GxActionBroker.FindAction(AEditorExpert.GetActionName);

      PopupMenu.Items.Add(ExpertMenuEntry);
    end;
  end;
end;

function TMenusForEditorExperts.GetActionCaption: string;
resourcestring
  SCaption = 'Editor Experts';
begin
  Result := SCaption;
end;

function TMenusForEditorExperts.GetDisplayName: string;
resourcestring
  SDisplayName = 'Editor Experts';
begin
  Result := SDisplayName;
end;

class function TMenusForEditorExperts.GetName: string;
begin
  Result := 'EditorExpertsMenu';
end;

function TMenusForEditorExperts.HasCallCount: Boolean;
begin
  Result := False;
end;

function TMenusForEditorExperts.HasConfigOptions: Boolean;
begin
  Result := False;
end;

function TMenusForEditorExperts.HasSubmenuItems: Boolean;
begin
  Result := SupportsSubmenu;
end;

function TMenusForEditorExperts.SupportsSubmenu: Boolean;
begin
  // The Delphi 7- IDEs seem to clear out the submenu item shortcuts
  Result := RunningDelphi8OrGreater;
  if Result then begin
    // on small screens the sub menu cannot be displayed properly
    Result := Assigned(Screen.ActiveForm)  and Assigned(Screen.ActiveForm.Monitor)
      and (Screen.ActiveForm.Monitor.Height >= 1024);
  end;
end;

procedure TMenusForEditorExperts.UpdateAction(Action: TCustomAction);
begin
  Action.Enabled := Assigned(GExpertsInst.EditorExpertManager)
    and (GExpertsInst.EditorExpertManager.EditorExpertCount > 0);
  // todo: Is this a good idea? Shouldn't the action always be visible?
  Action.Visible := Action.Enabled;
end;

initialization
  RegisterGX_Expert(TMenusForEditorExperts);

finalization
  ReleaseInternalPopupMenu;

end.
