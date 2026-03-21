unit GX_IdeToolbarExpert;

{$I GX_CondDefine.inc}

interface

uses
  GX_Experts,
  SysUtils,
  Classes,
  Controls;

implementation

uses
{(*}{$IFOPT D+}GX_DbugIntf,{$ENDIF}{*)}
  Menus,
  Forms,
  u_dzVclUtils,
  GX_Consts,
  GX_ActionBroker,
  GX_GenericUtils,
  GX_OtaUtils,
  GX_GExperts,
  GX_ConfigurationInfo,
  GX_IdeToolbar,
  GX_ToolbarConfig,
  GX_TimedCallback;

type
  TGX_IdeToolbarExpert = class(TGX_Expert)
  private
    FIdeToolbar: TGXIdeToolBarPanel;
    FToolBarActionsList: TStringList;
    procedure CreateAndShowToolbar;
    procedure CreateDefaultActionList;
    procedure HandleOnConfigure(_Sender: TObject);
    procedure DelayedRecreateToolbarButtons(_Sender: TObject);
  protected
    procedure SetActive(New: Boolean); override;
  public
    ///<summary>
    /// @returns False </summary>
    function CanHaveShortCut: Boolean; override;
    constructor Create; override;
    destructor Destroy; override;
    function GetActionCaption: string; override;
    function GetHelpString: string; override;
    ///<summary>
    /// @returns False </summary>
    function HasMenuItem: Boolean; override;
    ///<summary>
    /// Shows Toolbar configuration dialog  </summary>
    procedure Configure(_Owner: TWinControl); override;
    ///<summary>
    /// @returns False, we do not want to annoy people with a toolbar that most won't need. </summary>
    function IsDefaultActive: Boolean; override;
    procedure InternalLoadSettings(_Settings: IExpertSettings); override;
    procedure InternalSaveSettings(_Settings: IExpertSettings); override;
    ///<summary>
    /// Does nothing </summary>
    procedure Execute(Sender: TObject); override;
  end;

{ TGX_IdeToolbarExpert }

procedure TGX_IdeToolbarExpert.Execute(Sender: TObject);
begin
  // does nothing
end;

function TGX_IdeToolbarExpert.CanHaveShortCut: Boolean;
begin
  Result := False;
end;

procedure TGX_IdeToolbarExpert.HandleOnConfigure(_Sender: TObject);
begin
  Configure(nil);
end;

procedure TGX_IdeToolbarExpert.Configure(_Owner: TWinControl);
begin
  if TfmToolbarConfig.Execute(_Owner, FToolBarActionsList) then begin
    SaveSettings;
    if Assigned(FIdeToolbar) then
      FIdeToolbar.RecreateToolBarButtons(FToolBarActionsList);
  end;
end;

constructor TGX_IdeToolbarExpert.Create;
begin
  inherited Create;

  FToolBarActionsList := TStringList.Create;
end;

destructor TGX_IdeToolbarExpert.Destroy;
begin
  FreeAndNil(FToolBarActionsList);
  FreeAndNil(FIdeToolbar);

  inherited Destroy;
end;

function TGX_IdeToolbarExpert.GetActionCaption: string;
resourcestring
  SIdeToolbarExpertMenuCaption = 'IDE Toolbar';
begin
  Result := SIdeToolbarExpertMenuCaption;
end;

function TGX_IdeToolbarExpert.GetHelpString: string;
resourcestring
  SIdeToolbarExpertHelp =
    'Shows an additional toolbar in the IDE' + sLineBreak +
    'This is meant to be used instead of the broken toolbars in some IDEs';
begin
  Result := SIdeToolbarExpertHelp;
end;

function TGX_IdeToolbarExpert.HasMenuItem: Boolean;
begin
  Result := False;
end;

procedure TGX_IdeToolbarExpert.InternalLoadSettings(_Settings: IExpertSettings);
var
  i: Integer;
begin
  inherited;
  _Settings.ReadStrings('Toolbar', FToolBarActionsList, 'Action');
  for i := FToolBarActionsList.Count - 1 downto 0 do begin
    // todo: Does this still apply?
    // Close just causes AVs, so we don't allow it
    if StrContains('FileClose', FToolBarActionsList[i]) then
      FToolBarActionsList.Delete(i);
  end;

  if (FToolBarActionsList.Count = 0) and (not IsStandAlone) then begin
    CreateDefaultActionList;
    InternalSaveSettings(_Settings);
  end;
end;

procedure TGX_IdeToolbarExpert.InternalSaveSettings(_Settings: IExpertSettings);
begin
  inherited;
  Assert(Assigned(_Settings));
  Assert(Assigned(FToolBarActionsList));

  _Settings.WriteStrings('Toolbar', FToolBarActionsList, 'Action');
end;

function TGX_IdeToolbarExpert.IsDefaultActive: Boolean;
begin
  Result := False;
end;

procedure TGX_IdeToolbarExpert.SetActive(New: Boolean);
begin
  if New <> Active then begin
    inherited SetActive(New);
    if New then begin
      CreateAndShowToolbar;
    end else begin
      FreeAndNil(FIdeToolbar);
    end;
  end;
end;

procedure TGX_IdeToolbarExpert.CreateAndShowToolbar;
var
  AppBuilder: TForm;
  ControlBar: TComponent;
begin
  if not Assigned(FIdeToolbar) then begin
    if not TScreen_TryFindFormByName('AppBuilder', AppBuilder) then
      Exit; //==>

    ControlBar := AppBuilder.FindComponent('ControlBar1');
    if not Assigned(ControlBar) then
      Exit; //==>

    FIdeToolbar := TGXIdeToolBarPanel.Create(ControlBar);
    FIdeToolbar.OnConfigure := Self.HandleOnConfigure;
    if not FIdeToolbar.RecreateToolBarButtons(FToolBarActionsList) then
      TTimedCallback.Create(DelayedRecreateToolbarButtons, 100, true);
  end;
end;

procedure TGX_IdeToolbarExpert.DelayedRecreateToolbarButtons(_Sender: TObject);
begin
  FIdeToolbar.RecreateToolBarButtons(FToolBarActionsList);
end;

procedure TGX_IdeToolbarExpert.CreateDefaultActionList;
const
  // Do not localize default action names.
  DefaultToolBarActions: array[0..20] of string =
    ('FileSaveCommand', 'FileSaveAllCommand', '-',
    'RunRunCommand', 'RunPauseCommand', 'RunResetCommand', '-',
    'AlignLeftDegesCommand', 'AlignRightEdgesCommand', 'AlignHorizCentersCommand',
    'AlignTopEdgesCommand', 'AlignBottomEdgesCommand', 'AlignVertCentersCommand',
    'AlignMakeSameHeight', 'AlignMakeSameWidth', '-',
    'AlignAlignToGridCommand', 'AlignSizeToGrid', '-',
    'BackCommand', 'ForwardCommand');
var
  i: Integer;
  ActionName: string;
begin
  for i := Low(DefaultToolBarActions) to High(DefaultToolBarActions) do begin
    ActionName := DefaultToolBarActions[i];
    if ActionName <> SeparatorMenuItemString then begin
{$IFOPT D+}
      if not Assigned(GxActionBroker.FindAction(ActionName)) then begin
        SendDebugError('Could not locate default IDE action named "' + ActionName + '"');
        ActionName := '';
      end;
{$ENDIF D+}
    end;
    if ActionName <> '' then
      FToolBarActionsList.Add(ActionName);
  end;
end;

//*********************************************************
// Purpose: Lets GExperts know about this expert
//*********************************************************
initialization
  RegisterGX_Expert(TGX_IdeToolbarExpert);
end.

