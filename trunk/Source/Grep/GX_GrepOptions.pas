unit GX_GrepOptions;

interface

uses
  SysUtils,
  Classes,
  Controls,
  StdCtrls,
  Forms,
  Menus,
  ActnList,
  Actions,
  GX_BaseForm,
  GX_Experts;

const
  // do not translate
  GrepSearchName = 'GrepSearch';
  GrepResultsName = 'GrepResuls';
  GrepNextItemName = 'GrepNextItem';
  GrepPrevItemName = 'GrepPrevItem';

type
  TfmGrepOptions = class(TfmBaseForm)
    btnOK: TButton;
    btnCancel: TButton;
    chkUseCurrentIdent: TCheckBox;
    ed_ExternalEditor: TEdit;
    l_ExternalEditor: TLabel;
    l_Parameters: TLabel;
    b_Select: TButton;
    ed_Parameters: TEdit;
    b_Parameters: TButton;
    pm_Parameters: TPopupMenu;
    mi_File: TMenuItem;
    mi_Line: TMenuItem;
    mi_Column: TMenuItem;
    procedure b_SelectClick(Sender: TObject);
    procedure mi_FileClick(Sender: TObject);
    procedure mi_LineClick(Sender: TObject);
    procedure mi_ColumnClick(Sender: TObject);
  private
    procedure HandleDropFiles(_Sender: TObject; _Files: TStrings);
  public
    class function Execute(var _UseCurrentIdent: Boolean; var _Editor, _Params: string): Boolean;
    constructor Create(_Owner: TComponent); override;
  end;

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

{$R *.dfm}

uses
  ComCtrls,
  u_dzVclUtils,
  GX_GenericUtils, GX_IdeUtils, GX_GExperts, GX_ActionBroker;

{ TfmGrepOptions }

class function TfmGrepOptions.Execute(var _UseCurrentIdent: Boolean;
  var _Editor, _Params: string): Boolean;
var
  Dlg: TfmGrepOptions;
begin
  Dlg := TfmGrepOptions.Create(nil);
  try
    Dlg.chkUseCurrentIdent.Checked := _UseCurrentIdent;
    Dlg.ed_ExternalEditor.Text := _Editor;
    if _Params = '' then
      Dlg.ed_Parameters.Text := '{FILE}'
    else
      Dlg.ed_Parameters.Text := _Params;
    Result := (Dlg.ShowModal = mrOk);
    if Result then begin
      _UseCurrentIdent := Dlg.chkUseCurrentIdent.Checked;
      _Editor := Dlg.ed_ExternalEditor.Text;
      _Params := Dlg.ed_Parameters.Text;
    end;
  finally
    FreeAndNil(Dlg);
  end;
end;

procedure TfmGrepOptions.b_SelectClick(Sender: TObject);
var
  fn: string;
begin
  fn := ed_ExternalEditor.Text;
  if ShowOpenDialog('Select External Editor', '*.exe', fn) then
    ed_ExternalEditor.Text := fn;
end;

constructor TfmGrepOptions.Create(_Owner: TComponent);
begin
  inherited;
  TWinControl_ActivateDropFiles(Self, HandleDropFiles);

  TButton_AddDropdownMenu(b_Parameters, pm_Parameters);
end;

procedure TfmGrepOptions.HandleDropFiles(_Sender: TObject; _Files: TStrings);
begin
  if _Files.Count > 0 then
    ed_ExternalEditor.Text := _Files[0];
end;

procedure TfmGrepOptions.mi_ColumnClick(Sender: TObject);
begin
  ed_Parameters.Text := ed_Parameters.Text + '{COLUMN}';
end;

procedure TfmGrepOptions.mi_FileClick(Sender: TObject);
begin
  ed_Parameters.Text := ed_Parameters.Text + '{FILE}';
end;

procedure TfmGrepOptions.mi_LineClick(Sender: TObject);
begin
  ed_Parameters.Text := ed_Parameters.Text + '{LINE}';
end;

{ TGrepMenuEntryExpert }

constructor TGrepMenuEntryExpert.Create;
begin
  inherited;
  gblGrepMenuEntryExpert := Self;
end;

destructor TGrepMenuEntryExpert.Destroy;
begin
  gblGrepMenuEntryExpert := nil;
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
begin

end;

procedure TGrepMenuEntryExpert.ShowPopup(Sender: TObject);
begin

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
