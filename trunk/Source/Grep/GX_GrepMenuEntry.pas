unit GX_GrepMenuEntry;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils,
  Classes,
  Menus,
  ActnList,
  GX_Experts;

const
  // do not translate
  GrepSearchName = 'GrepSearch';
  GrepResultsName = 'GrepResults';
  GrepNextItemName = 'GrepNextItem';
  GrepPrevItemName = 'GrepPrevItem';

type
  TGrepMenuEntryExpert = class(TGX_Expert)
  private
    procedure PopulatePopupMenu(const PopupMenu: TPopupMenu);
  protected
    procedure UpdateAction(Action: TCustomAction); override;
    function SupportsSubmenu: Boolean;
  public
    class function GetName: string; override;
    constructor Create; override;
    destructor Destroy; override;
    function GetActionCaption: string; override;
    function HasConfigOptions: Boolean; override;
    function HasSubmenuItems: Boolean; override;
    function HasCallCount: Boolean; override;
    procedure CreateSubMenuItems(MenuItem: TMenuItem); override;
    procedure Execute(Sender: TObject); override;
    procedure ShowPopup(Sender: TObject);
  end;

var
  gblGrepMenuEntryExpert: TGrepMenuEntryExpert = nil;

implementation

uses
  Types,
  Controls,
  ComCtrls,
  Forms,
  GX_OtaUtils,
  GX_GExperts,
  GX_ActionBroker,
  GX_IdeUtils;

{ TGrepMenuEntryExpert }

var
  InternalPopupMenu: TPopupMenu;

function GetInternalPopupMenu: TPopupMenu;
const
  MenuName = 'GxMenusForEditorExpertsInternalPopupMenu';
begin
  if not Assigned(InternalPopupMenu) then begin
    InternalPopupMenu := NewPopupMenu(nil, MenuName, paCenter, False, []);
{$IFNDEF ICONS_IN_POPUP_MENUS_ARE_BROKEN}
    InternalPopupMenu.Images := GxOtaGetIdeImageList;
{$ENDIF}
  end;

  Result := InternalPopupMenu;
end;

procedure ReleaseInternalPopupMenu;
begin
  FreeAndNil(InternalPopupMenu);
end;

constructor TGrepMenuEntryExpert.Create;
begin
  inherited;
  gblGrepMenuEntryExpert := Self;
end;

destructor TGrepMenuEntryExpert.Destroy;
begin
  gblGrepMenuEntryExpert := nil;
  ReleaseInternalPopupMenu;
  inherited;
end;

procedure TGrepMenuEntryExpert.CreateSubMenuItems(MenuItem: TMenuItem);
var
  GExpertsInstance: TGExperts;

  procedure HandleExpert(const _ExpertName: string);
  var
    Expert: TGX_Expert;
    ExpertMenuEntry: TMenuItem;
  begin
    if not GExpertsInstance.FindExpert(_ExpertName, Expert) then
      Exit; //==>
    if not Expert.Active then
      Exit; //==>

    ExpertMenuEntry := TMenuItem.Create(MenuItem);
    if Expert.HasMenuItem then begin
      ExpertMenuEntry.Action := GxActionBroker.FindGExpertsMenuAction(Expert.GetActionName)
    end else begin
      ExpertMenuEntry.Action := GxActionBroker.FindGExpertsAction(Expert.GetActionName);
    end;
    MenuItem.Add(ExpertMenuEntry);
  end;

begin
  inherited;
  Assert(Assigned(MenuItem));
  MenuItem.Clear;

  GExpertsInstance := GExpertsInst;
  Assert(Assigned(GExpertsInstance));

  HandleExpert(GrepSearchName);
  HandleExpert(GrepResultsName);
  HandleExpert(GrepPrevItemName);
  HandleExpert(GrepNextItemName);
end;

procedure TGrepMenuEntryExpert.Execute(Sender: TObject);
var
  IsToolButton: Boolean;
begin
  IsToolButton := Assigned(Sender) and (Sender is TCustomAction)
    and (TCustomAction(Sender).ActionComponent is TToolButton);
  if SupportsSubmenu and (not IsToolButton) and Assigned(Sender) then begin
    // The submenu items perform all actions
  end else begin
    ShowPopup(Sender);
  end;
end;

function TGrepMenuEntryExpert.GetActionCaption: string;
begin
  Result := '&Grep';
end;

class function TGrepMenuEntryExpert.GetName: string;
begin
  Result := 'GrepMenuEntry';
end;

function TGrepMenuEntryExpert.HasCallCount: Boolean;
begin
  Result := False;
end;

function TGrepMenuEntryExpert.HasConfigOptions: Boolean;
begin
  Result := False;
end;

function TGrepMenuEntryExpert.HasSubmenuItems: Boolean;
begin
  Result := True;
end;

procedure TGrepMenuEntryExpert.PopulatePopupMenu(const PopupMenu: TPopupMenu);
var
  GExpertsInstance: TGExperts;

  procedure HandleExpert(const _ExpertName: string);
  var
    Expert: TGX_Expert;
    ExpertMenuEntry: TMenuItem;
  begin
    if not GExpertsInstance.FindExpert(_ExpertName, Expert) then
      Exit; //==>
    if not Expert.Active then
      Exit; //==>

    ExpertMenuEntry := TMenuItem.Create(PopupMenu);
    if Expert.HasMenuItem then begin
      ExpertMenuEntry.Action := GxActionBroker.FindGExpertsMenuAction(Expert.GetActionName)
    end else begin
      ExpertMenuEntry.Action := GxActionBroker.FindGExpertsAction(Expert.GetActionName);
    end;
    PopupMenu.Items.Add(ExpertMenuEntry);
  end;

begin
  GExpertsInstance := GExpertsInst;
  Assert(Assigned(GExpertsInstance));

  PopupMenu.Items.Clear;
  HandleExpert(GrepSearchName);
  HandleExpert(GrepResultsName);
  HandleExpert(GrepPrevItemName);
  HandleExpert(GrepNextItemName);
end;

procedure TGrepMenuEntryExpert.ShowPopup(Sender: TObject);
var
  MousePosition: TPoint;
  APopupMenu: TPopupMenu;
begin
  MousePosition := Mouse.CursorPos;
  APopupMenu := GetInternalPopupMenu;
  Assert(Assigned(APopupMenu));
  PopulatePopupMenu(APopupMenu);

  APopupMenu.Popup(MousePosition.X, MousePosition.Y);
end;

function TGrepMenuEntryExpert.SupportsSubmenu: Boolean;
begin
  // The Delphi 7- IDEs seem to clear out the submenu item shortcuts
  Result := RunningDelphi8OrGreater;
  if Result then begin
    // on small screens the sub menu cannot be displayed properly
    Result := Assigned(Screen.ActiveForm) and Assigned(Screen.ActiveForm.Monitor)
      and (Screen.ActiveForm.Monitor.Height >= 1024);
  end;
end;

procedure TGrepMenuEntryExpert.UpdateAction(Action: TCustomAction);
var
  b: Boolean;
  GExpertsInstance: TGExperts;
begin
  GExpertsInstance := GExpertsInst(True);
  b := False;
  if GExpertsInstance.IsExpertActive(GrepSearchName) then
    b := True;
  if GExpertsInstance.IsExpertActive(GrepResultsName) then
    b := True;
  if GExpertsInstance.IsExpertActive(GrepNextItemName) then
    b := True;
  if GExpertsInstance.IsExpertActive(GrepPrevItemName) then
    b := True;

  Action.Enabled := b;
  // todo: Is this a good idea? Shouldn't the action always be visible?
  Action.Visible := Action.Enabled;
end;

initialization
  RegisterGX_Expert(TGrepMenuEntryExpert);

end.
