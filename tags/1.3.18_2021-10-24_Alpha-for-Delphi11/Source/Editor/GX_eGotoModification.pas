unit GX_eGotoModification;

{$I GX_CondDefine.inc}

interface
{$IFDEF GX_VER220_up} // RAD Studio XE 1 (16; BDS 8)
uses
  Windows,
  SysUtils,
  Classes,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  ComCtrls,
  Menus,
  ToolsAPI,
  DockForm,
  GX_EditorExpert;

// There are already IDE actions for navigating to the previous/next modification in the editor
// window. They have keyboard shortcuts but apparently no menu entries:
//
// http://docwiki.embarcadero.com/RADStudio/Tokyo/en/Code_Editor#Finding_the_Next_and_Previous_Changes
//
// The purpose of these editor experts is to make this founctionality more visible by creating
// menu entries for them.

type
  TOnEditViewChanged = procedure(_Sender: TObject; const _EditWindow: INTAEditWindow) of object;

  TEditorNotifier = class(TNotifierObject, INTAEditServicesNotifier)
  private
    FOnEditViewOpened: TOnEditViewChanged;
    FOnEditViewClosed: TOnEditViewChanged;
    procedure doOnEditWindowOpened(const _EditWindow: INTAEditWindow);
    procedure doOnEditWindowClosed(const _EditWindow: INTAEditWindow);
  private
    procedure WindowShow(const EditWindow: INTAEditWindow; Show, LoadedFromDesktop: Boolean);
    procedure WindowNotification(const EditWindow: INTAEditWindow; Operation: TOperation);
    procedure WindowActivated(const EditWindow: INTAEditWindow);
    procedure WindowCommand(const EditWindow: INTAEditWindow; Command, Param: Integer; var Handled: Boolean);
    procedure EditorViewActivated(const EditWindow: INTAEditWindow; const EditView: IOTAEditView);
    procedure EditorViewModified(const EditWindow: INTAEditWindow; const EditView: IOTAEditView);
    procedure DockFormVisibleChanged(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
    procedure DockFormUpdated(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
    procedure DockFormRefresh(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
  public
    constructor Create(_OnEditViewOpened: TOnEditViewChanged; _OnEditViewClosed: TOnEditViewChanged);
  end;

type
  TGotoModificationBaseExpert = class(TEditorExpert)
  private
    FNotifier: Integer;
{$IFDEF EDITOR_POPUP_MENU_GETS_RECREATED}
    FOrigPopupEvent: TNotifyEvent;
    procedure InstallPopupHook(_pm: TPopupMenu);
    procedure HandleOnMenuPopup(_Sender: TObject);
    procedure AppendMenuItem(_pm: TPopupMenu);
    procedure RemoveMenuItem(_pm: TPopupMenu);
{$ENDIF}
    procedure HandleWindowOpened(_Sender: TObject; const _EditWindow: INTAEditWindow);
    procedure CreateMenuItem(_EditForm: TCustomForm);
  protected
    procedure SetActive(New: Boolean); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

type
  TGotoPrevModificationExpert = class(TGotoModificationBaseExpert)
  public
    function GetDefaultShortCut: TShortCut; override;
    procedure Execute(Sender: TObject); override;
    function GetDisplayName: string; override;
    function GetHelpString: string; override;
    function HasConfigOptions: Boolean; override;
  end;

type
  TGotoNextModificationExpert = class(TGotoModificationBaseExpert)
  public
    function GetDefaultShortCut: TShortCut; override;
    procedure Execute(Sender: TObject); override;
    function GetDisplayName: string; override;
    function GetHelpString: string; override;
    function HasConfigOptions: Boolean; override;
  end;
{$ENDIF}
implementation
{$IFDEF GX_VER220_up} // RAD Studio XE 1 (16; BDS 8)

uses
{$IFOPT D+}GX_DbugIntf,
{$ENDIF}
  Registry,
  u_dzVclUtils,
  GX_OtaUtils;

{ Called when a new edit view is created(opInsert) or destroyed(opRemove) }

constructor TEditorNotifier.Create(_OnEditViewOpened, _OnEditViewClosed: TOnEditViewChanged);
begin
  inherited Create;
  FOnEditViewOpened := _OnEditViewOpened;
  FOnEditViewClosed := _OnEditViewClosed;
end;

procedure TEditorNotifier.DockFormRefresh(const EditWindow: INTAEditWindow;
  DockForm: TDockableForm);
begin
  // we don't care
end;

procedure TEditorNotifier.DockFormUpdated(const EditWindow: INTAEditWindow;
  DockForm: TDockableForm);
begin
  // we don't care
end;

procedure TEditorNotifier.DockFormVisibleChanged(const EditWindow: INTAEditWindow;
  DockForm: TDockableForm);
begin
  // we don't care
end;

procedure TEditorNotifier.EditorViewActivated(const EditWindow: INTAEditWindow;
  const EditView: IOTAEditView);
begin
  // we don't care
end;

procedure TEditorNotifier.EditorViewModified(const EditWindow: INTAEditWindow;
  const EditView: IOTAEditView);
begin
  // we don't care
end;

procedure TEditorNotifier.WindowActivated(const EditWindow: INTAEditWindow);
begin
  // we don't care
end;

procedure TEditorNotifier.WindowCommand(const EditWindow: INTAEditWindow; Command, Param: Integer;
  var Handled: Boolean);
begin
  // we don't care
end;

procedure TEditorNotifier.WindowNotification(const EditWindow: INTAEditWindow;
  Operation: TOperation);
begin
  case Operation of
    opInsert: doOnEditWindowOpened(EditWindow);
    opRemove: doOnEditWindowClosed(EditWindow);
  end;

end;

procedure TEditorNotifier.doOnEditWindowOpened(const _EditWindow: INTAEditWindow);
begin
  if Assigned(FOnEditViewOpened) then
    FOnEditViewOpened(Self, _EditWindow);
end;

procedure TEditorNotifier.doOnEditWindowClosed(const _EditWindow: INTAEditWindow);
begin
  if Assigned(FOnEditViewClosed) then
    FOnEditViewClosed(Self, _EditWindow);
end;

procedure TEditorNotifier.WindowShow(const EditWindow: INTAEditWindow; Show,
  LoadedFromDesktop: Boolean);
begin
  // we don't care
end;

{ TGotoModificationBaseExpert }

constructor TGotoModificationBaseExpert.Create;
begin
  inherited Create;
  FNotifier := GxOtaGetEditorServices.AddNotifier(TEditorNotifier.Create(HandleWindowOpened, nil));
end;

destructor TGotoModificationBaseExpert.Destroy;
begin
  if FNotifier <> InvalidNotifierIndex then begin
    GxOtaGetEditorServices.RemoveNotifier(FNotifier);
  end;
  inherited;
end;

procedure TGotoModificationBaseExpert.HandleWindowOpened(_Sender: TObject; const _EditWindow: INTAEditWindow);
begin
  CreateMenuItem(_EditWindow.GetForm);
end;

{$IFDEF EDITOR_POPUP_MENU_GETS_RECREATED}

procedure TGotoModificationBaseExpert.InstallPopupHook(_pm: TPopupMenu);
begin
  FOrigPopupEvent := _pm.OnPopup;
  _pm.OnPopup := Self.HandleOnMenuPopup;
end;

procedure TGotoModificationBaseExpert.HandleOnMenuPopup(_Sender: TObject);
begin
  if _Sender is TPopupMenu then begin
    // I tried to only call RemoveMenuItem here, but this failed when the user pressed Shift+Ctrl+T for adding a todo
    // https://sourceforge.net/p/gexperts/bugs/223/
    // Using Items.Clear avoids this problem but might create other ones.
    TPopupMenu(_Sender).Items.Clear;
  end;
  if Assigned(FOrigPopupEvent) then begin
    FOrigPopupEvent(_Sender);
  end;
  if _Sender is TPopupMenu then begin
    // sometimes Delphi 10.4.1 passes a TMenuItem here, no idea how this can happen, but it caused a runtime error
    AppendMenuItem(TPopupMenu(_Sender));
  end;
end;

procedure TGotoModificationBaseExpert.RemoveMenuItem(_pm: TPopupMenu);
var
  cmp: TComponent;
  MiName: string;
begin
  MiName := 'GX' + Self.GetName;
  cmp := _pm.FindComponent(MiName);
  if Assigned(cmp) then begin
    cmp.Free;
  end;
end;

procedure TGotoModificationBaseExpert.AppendMenuItem(_pm: TPopupMenu);
var
  cmp: TComponent;
  mi: TMenuItem;
  MiName: string;
begin
  // we always want our entry to be at the end, we remove it first
  RemoveMenuItem(_pm);

  // The IDE uses the same popup menu everywhere in the editor window.
  // We only want to add to the editor.
  if _pm.Items.Count = 0 then begin
    // empty menu, e.g. on the gutter, when there is no breakpoint
    Exit; //==>
  end;
  cmp := _pm.FindComponent('BreakpointEnable');
  if Assigned(cmp) then begin
    // breakpoint popup menu
    Exit; //==>
  end;

  mi := TPopupMenu_AppendMenuItem(_pm, Self.GetDisplayName, Self.Execute);
  mi.Name := MiName;
  mi.ShortCut := Self.ShortCut;
end;
{$ENDIF}

procedure TGotoModificationBaseExpert.CreateMenuItem(_EditForm: TCustomForm);

{$IFNDEF EDITOR_POPUP_MENU_GETS_RECREATED}

  procedure AppendMenuItem(_pm: TPopupMenu);
  var
    cmp: TComponent;
    mi: TMenuItem;
    MiName: string;
  begin
    MiName := 'GX' + Self.GetName;
    cmp := _pm.FindComponent(MiName);
    if Assigned(cmp) then begin
      Exit; //==>
    end;

    mi := TPopupMenu_AppendMenuItem(_pm, Self.GetDisplayName, Self.Execute);
    mi.Name := MiName;
    mi.ShortCut := Self.ShortCut;
  end;
{$ENDIF}

var
  EditView: IOTAEditView;
  cmp: TComponent;
  pm: TPopupMenu;
begin
  if not Active then
    Exit; //==>

  if not Assigned(_EditForm) then begin
    if not GxOtaTryGetTopMostEditView(EditView) then
      Exit; //==>
    _EditForm := EditView.GetEditWindow.Form;
  end;
  cmp := _EditForm.FindComponent('EditorLocalMenu');
  if not Assigned(cmp) or not (cmp is TPopupMenu) then
    Exit; //==>
  pm := TPopupMenu(cmp);

{$IFDEF EDITOR_POPUP_MENU_GETS_RECREATED}
  // Adding an entry to the editor popup menu does not work with Delphi 10.3 and later.
  // The menu seems to be created dynamically and that fails if we add to it.
  InstallPopupHook(pm);
{$ELSE}
  AppendMenuItem(pm);
{$ENDIF}
end;

procedure TGotoModificationBaseExpert.SetActive(New: Boolean);
begin
  inherited;
  if New then begin
    CreateMenuItem(nil);
  end else begin
//    if Assigned(FMenuItem) then begin
//      FreeAndNil(FMenuItem);
//    end;
  end;
end;

{ TGotoPrevModificationExpert }

procedure TGotoPrevModificationExpert.Execute(Sender: TObject);
var
  View: IOTAEditView;
begin
  if not GxOtaTryGetTopMostEditView(View) then
    Exit; //==>

  View.NavigateToModification(sdBackward, mtAnyMod);
end;

function TGotoPrevModificationExpert.GetDefaultShortCut: TShortCut;
begin
  // This is the default shortcut the IDE uses for this functionality, so it should be
  // safe to hijack it.
  // And even if it reverts back to the IDE action, the shortcut would still
  // work. ;-)
  Result := Menus.ShortCut(VK_F7, [ssCtrl, ssShift])
end;

function TGotoPrevModificationExpert.GetDisplayName: string;
resourcestring
  SDisplayName = 'Goto Previous Modification';
begin
  Result := SDisplayName;
end;

function TGotoPrevModificationExpert.GetHelpString: string;
resourcestring
  SGotoPrevModificationExpertHelp =
    'Navigates to the previous modification in the editor window.';
begin
  Result := SGotoPrevModificationExpertHelp;
end;

function TGotoPrevModificationExpert.HasConfigOptions: Boolean;
begin
  Result := False;
end;

{ TGotoNextModificationExpert }

procedure TGotoNextModificationExpert.Execute(Sender: TObject);
var
  View: IOTAEditView;
begin
  if not GxOtaTryGetTopMostEditView(View) then
    Exit; //==>
  View.NavigateToModification(sdForward, mtAnyMod);
end;

function TGotoNextModificationExpert.GetDefaultShortCut: TShortCut;
begin
  // This is the default shortcut the IDE uses for this functionality, so it should be
  // safe to hijack it.
  // And even if it reverts back to the IDE action, the shortcut would still
  // work. ;-)
  Result := Menus.ShortCut(VK_F8, [ssCtrl, ssShift])
end;

function TGotoNextModificationExpert.GetDisplayName: string;
resourcestring
  SDisplayName = 'Goto Next Modification';
begin
  Result := SDisplayName;
end;

function TGotoNextModificationExpert.GetHelpString: string;
resourcestring
  SGotoNextModificationExpertHelp =
    'Navigates to the next modification in the editor window.';
begin
  Result := SGotoNextModificationExpertHelp;
end;

function TGotoNextModificationExpert.HasConfigOptions: Boolean;
begin
  Result := False;
end;

initialization
  RegisterEditorExpert(TGotoPrevModificationExpert);
  RegisterEditorExpert(TGotoNextModificationExpert);
{$ENDIF}

end.



