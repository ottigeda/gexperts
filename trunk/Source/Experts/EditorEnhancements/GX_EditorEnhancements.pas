unit GX_EditorEnhancements;

{$I GX_CondDefine.inc}

interface

uses
  Classes, Controls,
  GX_EditorFormServices, GX_ConfigurationInfo,
  GX_HideNavbar, GX_Experts;

type
  TComponentNotificationEvent = procedure(Component: TComponent; Operation: TOperation) of object;
  TNotificationProxy = class(TComponent)
  private
    FRedirectionEvent: TComponentNotificationEvent;
  protected
    procedure Notification(Component: TComponent; Operation: TOperation); override;
  public
    constructor Create(_Owner: TComponent; _RedirectionEvent: TComponentNotificationEvent); reintroduce;
  end;

type
  TEditorEnhancements = class(TGX_Expert)
  private
    FNotificationProxy: TNotificationProxy;
    FHideNavigationToolbarExpert: IHideNavigationToolbarExpert;
    FToolBarAlign: TAlign;
    FToolBarActionsList: TStringList;
    FToolBarList: TList;
    // Tab properties for Delphi 6 / 7
    FButtons: Boolean;
    FButtonsFlat: Boolean;
    FMultiLine: Boolean;
    FHotTrack: Boolean;
    FToolBarVisible: Boolean;
    FMiddleButtonClose: Boolean;
    FHideNavbar: Boolean;
    procedure AddToolBar;
    procedure RemoveToolBar;
    procedure EditorFormListener(EventCode: TEditFormEventCode; EditFormProxy: IGxEditFormProxy);
    procedure LoadToolBarSettings(_Settings: IExpertSettings);
    procedure SaveToolBarSettings(_Settings: IExpertSettings);

    procedure Install;
    procedure Remove;
    procedure HandleFreeNotification(Component: TComponent; Operation: TOperation);
    procedure HandleConfigureToolbar(_Sender: TObject);
  protected
    procedure SetActive(_Value: Boolean); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    function CanHaveShortCut: Boolean; override;
    procedure Configure(_Owner: TWinControl); override;
    procedure Execute(Sender: TObject); override;
    function GetDisplayName: string; override;
    function GetHelpString: string; override;
    class function GetName: string; override;
    function HasMenuItem: Boolean; override;
    procedure InternalLoadSettings(_Settings: IExpertSettings); override;
    procedure InternalSaveSettings(_Settings: IExpertSettings); override;
    function IsDefaultActive: Boolean; override;

    procedure ApplyToolBarSettings;
  end;

implementation

uses
  {$IFOPT D+} GX_DbugIntf, GX_ActionBroker, {$ENDIF}
  SysUtils, ExtCtrls, ComCtrls, Forms,
  u_dzVclUtils,
  GX_GenericUtils, GX_OtaUtils, GX_Configure, GX_Toolbar, GX_ToolbarConfig,
  GX_ToolBarDropDown, GX_GxUtils, GX_IdeUtils,
  GX_EditorEnhancementsConfig;

constructor TNotificationProxy.Create(_Owner: TComponent; _RedirectionEvent: TComponentNotificationEvent);
begin
  FRedirectionEvent := _RedirectionEvent;
  inherited Create(_Owner);
end;

procedure TNotificationProxy.Notification(Component: TComponent; Operation: TOperation);
begin
  if Assigned(FRedirectionEvent) then
    FRedirectionEvent(Component, Operation);
  inherited;
end;

const
  ToolBarActionsKey = 'EditorEnhancements' +PathDelim+ 'ToolBarActions';
  ToolBarActionsCountValue = 'Count';
  ToolBarActionsActionPrefixValue = 'Action';

{ TEditorEnhancements }

procedure TEditorEnhancements.AddToolBar;
const
  GX_ToolBarComponentName = 'GX_ToolBar'; // Do not localize.
var
  i: Integer;
  EditorFormServices: IGxEditorFormServices;
  EditorFormProxy: IGxEditFormProxy;
  EditorForm: TCustomForm;
  ToolBar: TGXToolBar;

  EditControl: TControl;
  EditControlPanel: TPanel;
begin
  // Create list of toolbars on demand
  if not Assigned(FToolBarList) then
    FToolBarList := TList.Create;
  EditControlPanel := nil;

  EditorFormServices := GxEditorFormServices;
  for i := 0 to EditorFormServices.EditorFormProxyCount - 1 do
  begin
    EditorFormProxy := EditorFormServices.EditorFormProxies[i];
    Assert(Assigned(EditorFormProxy));

    EditorForm := EditorFormProxy.EditorForm;
    Assert(Assigned(EditorForm));

    if EditorFormProxy.IsSourceEditor and (not Assigned(EditorForm.FindComponent(GX_ToolBarComponentName))) then
    begin
      ToolBar := TGXToolBar.Create(EditorForm);
      toolbar.OnConfigureToolbar := HandleConfigureToolbar;
      FToolBarList.Add(ToolBar);
      ToolBar.FreeNotification(FNotificationProxy);
      ToolBar.Align := FToolBarAlign;
      ToolBar.Name := GX_ToolBarComponentName;

      EditControl := EditorFormProxy.EditControl;
      Assert(Assigned(EditControl));

      if RunningDelphi8OrGreater then begin
        // There are toolbar focus problems when the Parent is EditorPanel in D8+
        if Assigned(EditControl.Parent) then
          EditControlPanel := EditControl.Parent.Parent as TPanel; // CodePanel
      end
      else
        EditControlPanel := EditControl.Parent as TPanel; // EditorPanel

      if not Assigned(EditControlPanel) then
        Exit;

      ToolBar.Parent := EditControlPanel;
      ToolBar.Images := GxOtaGetIdeImageList;

      ToolBar.BringToFront;

      ToolBar.RecreateToolBarButtons(FToolBarActionsList);

      // Finally make the toolbar visible depending on settings
      ToolBar.Visible := FToolBarVisible;

      ToolBar.SetEditorControls(FMultiLine, FMiddleButtonClose, FHotTrack, FButtons, FButtonsFlat);
    end;
  end;
end;

procedure TEditorEnhancements.ApplyToolBarSettings;
var
  i: Integer;
  GExpertsToolBar: TGXToolBar;
begin
  // Immediately exit if there are no toolbars around
  if not Assigned(FToolBarList) then
    Exit;

  {$IFOPT D+} SendDebug(Format('Applying settings to %d toolbars', [FToolBarList.Count])); {$ENDIF}
  for i := 0 to FToolBarList.Count - 1 do
  begin
    GExpertsToolBar := FToolBarList.Items[i];

    {$IFOPT D+} SendDebug('Hiding GExpertsToolBar'); {$ENDIF}
    GExpertsToolBar.Visible := False;

    {$IFOPT D+} SendDebug('Aligning GExpertsToolBar'); {$ENDIF}
    GExpertsToolBar.Align := FToolBarAlign;
    GExpertsToolBar.RecreateToolBarButtons(FToolBarActionsList);

    {$IFOPT D+} SendDebug('Setting GExpertsToolBar.Visible to ' + BooleanText(FToolBarVisible)); {$ENDIF}
    GExpertsToolBar.Visible := FToolBarVisible;

    GExpertsToolBar.SetEditorControls(FMultiLine, FMiddleButtonClose, FHotTrack, FButtons, FButtonsFlat);
  end;

  {$IFOPT D+} SendDebug('Successfully applied toolbar settings'); {$ENDIF}
end;

function TEditorEnhancements.CanHaveShortCut: Boolean;
begin
  Result := false;
end;

procedure TEditorEnhancements.Configure(_Owner: TWinControl);
begin
  if TfmEditorEnhancementsConfig.Execute(_Owner, FToolBarVisible, FToolBarAlign, FToolBarActionsList,
    FHotTrack, FMultiLine, FMiddleButtonClose, FButtons, FButtonsFlat,
    FHideNavbar) then begin
    SaveSettings;
    ApplyToolBarSettings;
    FHideNavigationToolbarExpert.SetVisible(not FHideNavbar);
  end;
end;

constructor TEditorEnhancements.Create;
begin
  inherited Create;

  FNotificationProxy := TNotificationProxy.Create(nil, HandleFreeNotification);

  FHideNavigationToolbarExpert := CreateHideNavigationToolbarExpert;

  // Editor tab control
  FHotTrack := True;

  // Toolbar settings
  FToolBarVisible := True;
  FToolBarAlign := alTop;

  FToolBarActionsList := TStringList.Create;
  InitializeGXToolBarDropdowns;
end;

destructor TEditorEnhancements.Destroy;
begin
  {$IFOPT D+} SendDebug('Destroying Editor Enhancements'); {$ENDIF}

  Remove;

  FreeAndNil(FNotificationProxy);

  FHideNavigationToolbarExpert := Nil;

  FreeAndNil(FToolBarActionsList);

  {$IFOPT D+} SendDebug('Editor Enhancements Destroyed'); {$ENDIF}

  inherited Destroy;
end;

procedure TEditorEnhancements.EditorFormListener(
  EventCode: TEditFormEventCode; EditFormProxy: IGxEditFormProxy);
begin
  {$IFOPT D+} SendDebug('Got notification of editor form change'); {$ENDIF}
  if EventCode = efAfterCreate then
    AddToolBar;
end;

procedure TEditorEnhancements.Execute(Sender: TObject);
begin
  // do nothing
end;

function TEditorEnhancements.GetDisplayName: string;
begin
  Result := 'Editor Enhancements';
end;

function TEditorEnhancements.GetHelpString: string;
begin
  Result := 'Enhancements for the code editor including a toolbar';
end;

class function TEditorEnhancements.GetName: string;
begin
  Result := 'EditorEnhancements';
end;

procedure TEditorEnhancements.Install;
begin
  if not EditorEnhancementsPossible then 
    Exit; //==>

  AddToolBar;

  GxEditorFormServices.AddListener(EditorFormListener);
end;

procedure TEditorEnhancements.InternalLoadSettings(_Settings: IExpertSettings);
begin
  LoadToolBarSettings(_Settings);

  // These setting are only available in Delphi 6 and 7.
  FMiddleButtonClose := _Settings.ReadBool('MiddleButtonClose', FMiddleButtonClose);
  FMultiLine := _Settings.ReadBool('MultiLine', FMultiLine);
  FHotTrack := _Settings.ReadBool('HotTrack', FHotTrack);
  FButtons := _Settings.ReadBool('Buttons', FButtons);
  FButtonsFlat := _Settings.ReadBool('ButtonsFlat', FButtonsFlat);

  FToolBarVisible := _Settings.ReadBool('ToolBarVisible', FToolBarVisible);
  FHideNavbar := _Settings.ReadBool('HideNavbar', FHideNavbar);
  FToolBarAlign := TAlign(_Settings.ReadInteger('ToolBarAlign', Ord(FToolBarAlign)));
  Assert(FToolBarAlign in [Low(TAlign)..High(TAlign)]);

  FHideNavigationToolbarExpert.SetVisible(not FHideNavbar);
end;

procedure TEditorEnhancements.LoadToolBarSettings(_Settings: IExpertSettings);
const
  // Do not localize default action names.
  DefaultToolBarActions: array[0..4] of string =
    ('EditCutCommand', 'EditCopyCommand', 'EditPasteCommand', 'EditUndoCommand',
     'EditRedoCommand');
var
  i: Integer;
begin
  _Settings.ReadStrings('Toolbar', FToolBarActionsList, ToolBarActionsActionPrefixValue);
  for i := FToolBarActionsList.Count - 1 downto 0 do begin
    // Close just causes AVs, so we don't allow it
    if StrContains('FileClose', FToolBarActionsList[i]) then
      FToolBarActionsList.Delete(i);
  end;

  if (FToolBarActionsList.Count = 0) and (not IsStandAlone) then begin
    for i := Low(DefaultToolBarActions) to High(DefaultToolBarActions) do begin
{$IFOPT D+} Assert(Assigned(GxActionBroker.FindAction(DefaultToolBarActions[i])),
                   'Could not locate default IDE action named ' + DefaultToolBarActions[i]);
{$ENDIF D+}
      FToolBarActionsList.Add(DefaultToolBarActions[i]);
    end;

    SaveToolBarSettings(_Settings);
  end;
end;

procedure TEditorEnhancements.HandleConfigureToolbar(_Sender: TObject);
begin
  if TfmToolbarConfig.Execute(nil, FToolBarActionsList) then begin
    SaveSettings;
    ApplyToolBarSettings;
  end;
end;

procedure TEditorEnhancements.HandleFreeNotification(Component: TComponent; Operation: TOperation);
var
  i: Integer;
begin
  if Operation = opRemove then
  begin
    Assert(Assigned(FToolBarList));
    i := FToolBarList.IndexOf(Component);
    if i >= 0 then
      FToolBarList.Delete(i);
  end;
end;

function TEditorEnhancements.HasMenuItem: Boolean;
begin
  Result := False;
end;

procedure TEditorEnhancements.Remove;
begin
  if not EditorEnhancementsPossible then 
    Exit;

  GxEditorFormServices.RemoveListener(EditorFormListener);
  RemoveToolBar;
end;

procedure TEditorEnhancements.RemoveToolBar;
var
  GExpertsToolBar: TGXToolBar;
begin
  if not Assigned(FToolBarList) then
    Exit;

  {$IFOPT D+} SendDebug(Format('Editor Enhancements: Removing %d toolbar(s)', [FToolBarList.Count])); {$ENDIF}
  // Note: Since we used FreeNotification on the toolbar
  // to instruct it to notify ourselves of its very own
  // deletion, GExpertsToolBar.Free will essentially
  // call back into Notifcation where the toolbar is
  // removed from the FToolBarList (.Delete).
  // Hence the slightly weird structure of this loop.
  while FToolBarList.Count > 0 do
  begin
    GExpertsToolBar := FToolBarList.Items[0];
    FreeAndNil(GExpertsToolBar);
    {$IFOPT D+} SendDebug('Editor Enhancements: Successfully freed a toolbar'); {$ENDIF}
  end;

  {$IFOPT D+} SendDebug('Freeing toolbar list'); {$ENDIF}
  FreeAndNil(FToolBarList);
end;

procedure TEditorEnhancements.InternalSaveSettings(_Settings: IExpertSettings);
begin
  SaveToolBarSettings(_Settings);

  _Settings.WriteBool('MiddleButtonClose', FMiddleButtonClose);
  _Settings.WriteBool('MultiLine', FMultiLine);
  _Settings.WriteBool('HotTrack', FHotTrack);
  _Settings.WriteBool('Buttons', FButtons);
  _Settings.WriteBool('ButtonsFlat', FButtonsFlat);
  _Settings.WriteBool('ToolBarVisible', FToolBarVisible);
  _Settings.WriteBool('HideNavbar', FHideNavbar);
  _Settings.WriteInteger('ToolBarAlign', Ord(FToolBarAlign));
end;

function TEditorEnhancements.IsDefaultActive: Boolean;
begin
  Result := False;
end;

procedure TEditorEnhancements.SaveToolBarSettings(_Settings: IExpertSettings);
begin
  Assert(Assigned(_Settings));
  Assert(Assigned(FToolBarActionsList));

  _Settings.WriteStrings('Toolbar', FToolBarActionsList, ToolBarActionsActionPrefixValue);
end;

procedure TEditorEnhancements.SetActive(_Value: Boolean);
begin
  if FActive = _Value then
    Exit; //==>

  FActive := _Value;
  if FActive then
    Install
  else
    Remove;
end;

initialization
  RegisterGX_Expert(TEditorEnhancements);
end.

