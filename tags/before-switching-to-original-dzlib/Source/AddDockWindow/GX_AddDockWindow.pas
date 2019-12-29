{***************************************************************
 * Unit Name: GX_AddDockWindow
 * Purpose  : Adds a new dock window to the IDE where any of the
 *          : dockable forms can be docked.
 * Authors  : twm
 ****************************************************************}

unit GX_AddDockWindow;

{$I GX_CondDefine.inc}

interface

uses
  GX_Experts,
  SysUtils,
  Classes,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  GX_BaseForm;

type
  TfmGxDockForm = class(TfmBaseForm)
    l_Warning: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDockDrop(Sender: TObject; Source: TDragDockObject; X, Y: Integer);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  end;

implementation

{$R *.dfm}

uses
{$IFOPT D+}GX_DbugIntf,
{$ENDIF}
  Registry,
  Menus,
  GX_GExperts,
  GX_ConfigurationInfo,
  Windows;

type
  TAddDockWindowExpert = class(TGX_Expert)
  private
  protected
    procedure SetActive(New: Boolean); override;
  public
    // optional, defauls to true
    function CanHaveShortCut: Boolean; override;
    constructor Create; override;
    destructor Destroy; override;
    function GetActionCaption: string; override;
    // optional, but recommended
    function GetHelpString: string; override;
    // optional, defaults to true
    function HasConfigOptions: Boolean; override;
    // optional if HasConfigOptions returns false
    procedure Configure; override;
    procedure InternalLoadSettings(_Settings: IExpertSettings); override;
    procedure InternalSaveSettings(_Settings: IExpertSettings); override;
    procedure Execute(Sender: TObject); override;
  end;

{ TAddDockWindowExpert }

procedure TAddDockWindowExpert.Execute(Sender: TObject);
begin
  TfmGxDockForm.Create(Application).Show;
end;

function TAddDockWindowExpert.CanHaveShortCut: Boolean;
begin
  Result := True;
end;

procedure TAddDockWindowExpert.Configure;
resourcestring
  SYouClickedConfigure = 'You clicked the Configuration button!';
begin
  MessageDlg(SYouClickedConfigure, mtInformation, [mbOK], 0);
end;

constructor TAddDockWindowExpert.Create;
begin
  inherited Create;
end;

destructor TAddDockWindowExpert.Destroy;
begin
  // Free any created objects here
  inherited Destroy;
end;

function TAddDockWindowExpert.GetActionCaption: string;
resourcestring
  SMenuCaption = 'Add Dock Window';
begin
  Result := SMenuCaption;
end;

function TAddDockWindowExpert.GetHelpString: string;
resourcestring
  SAddDockWindowHelp =
    'Provides a new dock window that can be used to dock any dockable form. ' +
    'This form is visible in the taskbar.';
begin
  Result := SAddDockWindowHelp;
end;

function TAddDockWindowExpert.HasConfigOptions: Boolean;
begin
  Result := False;
end;

procedure TAddDockWindowExpert.InternalLoadSettings(_Settings: IExpertSettings);
begin
  inherited;
//  FSomeData := _Settings.ReadString('TestSetting', FSomeData);
end;

procedure TAddDockWindowExpert.InternalSaveSettings(_Settings: IExpertSettings);
begin
  inherited;
//  _Settings.WriteString('TestSetting', FSomeData);
end;

procedure TAddDockWindowExpert.SetActive(New: Boolean);
begin
  if New <> Active then begin
    inherited SetActive(New);
    if New then
      // Initialize anything necessary here (generally empty for modal experts)
    else begin
      // If we had a non-modal form, it would be destroyed here
      //FreeAndNil(FSampleForm);
    end;
  end;
end;

{ TfmGxSampleExpertForm }

procedure TfmGxDockForm.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.ExStyle := Params.ExStyle or WS_EX_APPWINDOW;
  Params.WndParent := 0;
end;

procedure TfmGxDockForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  if DockClientCount > 0 then
    Action := caMinimize
  else
    Action := caFree;
end;

procedure TfmGxDockForm.FormDockDrop(Sender: TObject; Source: TDragDockObject; X, Y: Integer);
begin
  inherited;
  l_Warning.Visible := False;
end;

initialization
  RegisterGX_Expert(TAddDockWindowExpert);
end.
