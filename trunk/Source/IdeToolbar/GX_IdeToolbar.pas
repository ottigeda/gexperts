unit GX_IdeToolbar;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils,
  Classes,
  Controls,
  ComCtrls,
  ExtCtrls,
  GX_ConfigurationInfo;

type
  TGXIdeToolBar = class(TToolBar)
  private
    function CreateToolBarButtons(_ToolBarActionsList: TStrings; out _Width: Integer): Boolean;
    procedure FreeToolBarButtons;
    procedure CreateToolBarConfigurationMenu(_OnClick: TNotifyEvent);
  public
    constructor Create(_Owner: TComponent); override;
  end;

type
  TGXIdeToolBarPanel = class(TPanel)
  private
    FToolbar: TGXIdeToolBar;
    FOnConfigure: TNotifyEvent;
    // OnClick handler for the toolbar configuration menu item
    procedure ConfigureToolBarButtonsClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    ///<summary>
    /// @returns True, if all buttons could be created, False if not </summary>
    function RecreateToolBarButtons(_ToolBarActionsList: TStrings): Boolean;
    property OnConfigure: TNotifyEvent read FOnConfigure write FOnConfigure;
  end;

implementation

uses
{(*}{$IFOPT D+}GX_DbugIntf,{$ENDIF}{*)}
  Forms,
  DbugIntf,
  Actions,
  Menus,
  ActnList,
  ToolWin,
  u_dzVclUtils,
  GX_UnspecifiedImage,
  GX_GenericUtils,
  GX_OtaUtils,
  GX_ToolbarConfig,
  GX_ActionBroker,
  GX_Consts;

{ TGXIdeToolBarPanel }

const
  SGxIdeToolBar = 'GxIdeToolBarPanel';

constructor TGXIdeToolBarPanel.Create(AOwner: TComponent);
begin
{(*}{$IFOPT D+}SendDebug('Creating GxIdeToolBarPanel');{$ENDIF}{*)}

  inherited;

  Name := SGxIdeToolBar;
  Left := 0;
  Top := 0;
  BevelOuter := bvNone;
  Caption := '';
  Parent := AOwner as TWinControl;

  FToolbar := TGXIdeToolBar.Create(Self);

  RegisterUnspecifiedBitmapWithIde;

  FToolbar.Images := Ota.GetIdeImageList;
  SetToolBarGradient(FToolbar, False);
end;

procedure TGXIdeToolBarPanel.ConfigureToolBarButtonsClick(Sender: TObject);
begin
  if Assigned(FOnConfigure) then
    FOnConfigure(Self);
end;

function TGXIdeToolBarPanel.RecreateToolBarButtons(_ToolBarActionsList: TStrings): Boolean;
var
  w: Integer;
begin
  Visible := False;
  try
    FToolbar.FreeToolBarButtons;
    Result := FToolbar.CreateToolBarButtons(_ToolBarActionsList, w);
    if w > 0 then
      Width := w
    else
      Width := Height;
    FToolbar.CreateToolBarConfigurationMenu(ConfigureToolBarButtonsClick);
    FToolbar.Visible := True;
  finally
    Visible := True;
  end;
end;

{ TGXIdeToolBar }

constructor TGXIdeToolBar.Create(_Owner: TComponent);
begin
  inherited Create(_Owner);
  Name := '';
  DockSite := False;
  Flat := True;
  AutoSize := True;
  Wrapable := False;
  Visible := False;
  BorderWidth := 0;
  BevelOuter := bvNone;
  BevelInner := bvNone;
  EdgeBorders := [];

  Parent := TWinControl(_Owner);
  Top := 0;
  Height := 32;
  Left := 0;
  Align := alTop;
  ShowHint := True;
end;

function TGXIdeToolBar.CreateToolBarButtons(_ToolBarActionsList: TStrings; out _Width: Integer): Boolean;
var
  ToolButton: TToolButton;
  IdeAction: TContainedAction;
  ActionName: string;
  i: Integer;
  bc: Integer;
begin
  Assert(Assigned(_ToolBarActionsList));

  Result := True;
  for i := _ToolBarActionsList.Count - 1 downto 0 do begin
    ActionName := _ToolBarActionsList[i];
    if ActionName = '' then
      Continue; //==^

    if ActionName = SeparatorMenuItemString then begin
      ToolButton := TToolButton.Create(Self);
      ToolButton.Style := tbsSeparator;
      ToolButton.Parent := Self;
      ToolButton.Visible := True;
      ToolButton.Width := 8;
    end else if not Ota.TryGetIdeActionByName(ActionName, IdeAction) then begin
      // The action does not exist within the realm of the current IDE.
      // We probably want to consider adding the tool button nevertheless, but
      // with a warning or text description that the action is missing?
      Result := False;
{(*}{$IFOPT D+}SendDebugFmt('CreateToolBarButtons: Action %s does not exist in this IDE instance.', [ActionName]);{$ENDIF}{*)}
    end else begin
      ToolButton := TToolButton.Create(Self);
      ToolButton.Wrap := False;
{$IFDEF GX_VER160_up}
      if IdeAction is TControlAction then
        ToolButton.Style := tbsDropDown;
{$ENDIF}
      ToolButton.Action := IdeAction;
      if ToolButton.Caption = '' then
        ToolButton.Caption := IdeAction.Name;
      if ToolButton.Hint = '' then
        ToolButton.Hint := StripHotkey(ToolButton.Caption);
      ToolButton.Parent := Self;
      ToolButton.AutoSize := True;
      if IdeAction is TCustomAction then
        if TCustomAction(IdeAction).ImageIndex = -1 then begin
            // It would be better to just show the caption, but I don't know how...
          ToolButton.ImageIndex := GxUnspecifiedImageIndex;
        end;
    end;
  end;

  bc := ButtonCount;
  if bc > 0 then begin
    ToolButton := Buttons[bc - 1];
    _Width := ToolButton.Left + ToolButton.Width;
  end else
    _Width := 0;
end;

procedure TGXIdeToolBar.CreateToolBarConfigurationMenu(_OnClick: TNotifyEvent);
resourcestring
  SConfigureToolBar = 'Configure Toolbar...';
var
  pm: TPopupMenu;
begin
  pm := TPopupMenu.Create(Self);
  TPopupMenu_AppendMenuItem(pm, SConfigureToolBar, _OnClick);
  PopupMenu := pm;
end;

procedure TGXIdeToolBar.FreeToolBarButtons;
begin
  while ComponentCount > 0 do
    Components[0].Free;
end;

end.

