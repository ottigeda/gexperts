unit GX_Goto;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  GX_BaseForm,
  GX_UnitPositions,
  ActnList,
  GX_ConfigurationInfo;

type
  Tf_Goto = class(TfmBaseForm)
    cmb_LineNumber: TComboBox;
    l_LineNumber: TLabel;
    lb_UnitPositions: TListBox;
    b_OK: TButton;
    b_Cancel: TButton;
    procedure lb_UnitPositionsClick(Sender: TObject);
    procedure lb_UnitPositionsDblClick(Sender: TObject);
    procedure cmb_LineNumberKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FUnitPositions: TUnitPositions;
    procedure SetData(_Row: Integer);
    procedure GetData(out _Row: Integer);
  public
    class function Execute(var _Row: Integer): boolean;
    constructor Create(_Owner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  ToolsAPI,
  GX_OtaUtils,
  GX_Experts,
  GX_IdeUtils,
  GX_GotoConfig;

const
  SEARCH_GOTO_COMMAND = 'SearchGotoCommand';

type
  TGotoExpert = class(TGX_Expert)
  private
    FIdeGotoActionEvent: TNotifyEvent;
    FOverrideSearchGoto: boolean;
    procedure HijackIdeActions;
    procedure ResetIdeActions;
  protected
    procedure SetActive(New: boolean); override;
    procedure Configure; override;
    procedure InternalLoadSettings(_Settings: IExpertSettings); override;
    procedure InternalSaveSettings(_Settings: IExpertSettings); override;
  public
    function GetActionCaption: string; override;
    class function GetName: string; override;
    destructor Destroy; override;
    procedure Execute(Sender: TObject); override;
    function HasConfigOptions: boolean; override;
    procedure AfterIDEInitialized; override;
  end;

{ Tf_Goto }

constructor Tf_Goto.Create(_Owner: TComponent);
var
  Items: TStrings;
  i: Integer;
begin
  inherited;
  Items := lb_UnitPositions.Items;
  Items.BeginUpdate;
  try
    FUnitPositions := TUnitPositions.Create(GxOtaGetCurrentSourceEditor);
    for i := 0 to FUnitPositions.Count - 1 do begin
      Items.Add(FUnitPositions.Positions[i].Name);
    end;
  finally
    Items.EndUpdate;
  end;
  lb_UnitPositions.ClientHeight := (FUnitPositions.Count + 1) * lb_UnitPositions.ItemHeight;
  b_OK.Top := lb_UnitPositions.Top + lb_UnitPositions.Height + 8;
  b_Cancel.Top := b_OK.Top;
  self.ClientHeight := b_OK.Top + b_OK.Height + 8;
end;

class function Tf_Goto.Execute(var _Row: Integer): boolean;
var
  frm: Tf_Goto;
begin
  frm := Tf_Goto.Create(nil);
  try
    frm.SetData(_Row);
    Result := (frm.ShowModal = mrOk);
    frm.GetData(_Row);
  finally
    FreeAndNil(frm);
  end;
end;

procedure Tf_Goto.GetData(out _Row: Integer);
begin
  _Row := StrToInt(cmb_LineNumber.Text);
end;

procedure Tf_Goto.SetData(_Row: Integer);
begin
  cmb_LineNumber.Text := IntToStr(_Row);
end;

procedure Tf_Goto.cmb_LineNumberKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if not cmb_LineNumber.DroppedDown then begin
    if (Key in [VK_UP, VK_DOWN]) and (Shift = []) then begin
      SendMessage(lb_UnitPositions.Handle, WM_KEYDOWN, Key, 0);
      Key := 0;
    end;
  end;
end;

procedure TCombobox_SetText(_cmb: TWinControl; const _Text: string);
begin
  _cmb.Perform(WM_SETTEXT, 0, LPARAM(PChar(_Text)));
  _cmb.Perform(CM_TEXTCHANGED, 0, 0);
end;

procedure TCombobox_SelectAll(_cmb: TWinControl);
begin
  SendMessage(_cmb.Handle, CB_SETEDITSEL, 0, LPARAM($FFFF0000));
end;

procedure Tf_Goto.lb_UnitPositionsClick(Sender: TObject);
var
  Idx: Integer;
  View: IOTAEditView;
  CharPos: TOTACharPos;
  CursorPos: TOTAEditPos;
begin
  Idx := lb_UnitPositions.ItemIndex;
  if Idx = -1 then
    Exit; //==>
  View := GxOtaGetTopMostEditView;
  CharPos := GxOtaGetCharPosFromPos(FUnitPositions.Positions[Idx].Position, View);
  View.ConvertPos(False, CursorPos, CharPos);
  TCombobox_SetText(cmb_LineNumber, IntToStr(CursorPos.Line));
  TCombobox_SelectAll(cmb_LineNumber);
end;

procedure Tf_Goto.lb_UnitPositionsDblClick(Sender: TObject);
begin
  lb_UnitPositionsClick(Sender);
  ModalResult := mrOk;
end;

{ TGotoExpert }

destructor TGotoExpert.Destroy;
begin
  ResetIDEAction(SEARCH_GOTO_COMMAND, FIdeGotoActionEvent);
  inherited;
end;

procedure TGotoExpert.Execute(Sender: TObject);
var
  View: IOTAEditView;
  Line: Integer;
  CursorPos: TOTAEditPos;
begin
//    SetFormIcon(fmAsciiChart);
  if not GxOtaTryGetTopMostEditView(View) then
    Exit; //==>

  CursorPos := View.CursorPos;
  Line := CursorPos.Line;

  if not Tf_Goto.Execute(Line) then
    Exit; //==>

  CursorPos.Line := Line;
  View.CursorPos := CursorPos;
  View.Center(Line, CursorPos.Col);
  IncCallCount;
end;

function TGotoExpert.GetActionCaption: string;
begin
  Result := '&Go to';
end;

class function TGotoExpert.GetName: string;
begin
  Result := 'GotoExpert';
end;

function TGotoExpert.HasConfigOptions: boolean;
begin
  Result := True;
end;

procedure TGotoExpert.Configure;
begin
  Tf_GotoConfig.Execute(FOverrideSearchGoto);

end;

procedure TGotoExpert.InternalLoadSettings(_Settings: IExpertSettings);
begin
  inherited;
  FOverrideSearchGoto := _Settings.ReadBool('OverrideSearchGoto', False);
end;

procedure TGotoExpert.InternalSaveSettings(_Settings: IExpertSettings);
begin
  inherited;
  _Settings.WriteBool('OverrideSearchGoto', FOverrideSearchGoto);
end;

procedure TGotoExpert.HijackIdeActions;
begin
  if FOverrideSearchGoto and not Assigned(FIdeGotoActionEvent) then
    HijackIDEAction(SEARCH_GOTO_COMMAND, FIdeGotoActionEvent, Execute);
end;

procedure TGotoExpert.ResetIdeActions;
begin
  if Assigned(FIdeGotoActionEvent) then
    ResetIDEAction(SEARCH_GOTO_COMMAND, FIdeGotoActionEvent);
end;

procedure TGotoExpert.SetActive(New: boolean);
begin
  inherited;
  if New then begin
    HijackIdeActions;
  end else begin
    ResetIdeActions;
  end;
end;

procedure TGotoExpert.AfterIDEInitialized;
begin
  inherited;
  HijackIdeActions;
end;

initialization
  RegisterGX_Expert(TGotoExpert);
end.
