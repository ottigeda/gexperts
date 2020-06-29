unit GX_CloseExceptionNotification;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils,
  Classes,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  Grids,
  Contnrs,
  GX_BaseForm;

type
  TfmGxCloseExceptionNotificationExpert = class(TfmBaseForm)
    sg_Exceptions: TStringGrid;
    b_OK: TButton;
    b_Cancel: TButton;
    l_Exceptions: TLabel;
    b_Add: TButton;
    b_Edit: TButton;
    b_Delete: TButton;
    procedure FormResize(Sender: TObject);
    procedure b_EditClick(Sender: TObject);
    procedure b_AddClick(Sender: TObject);
    procedure b_DeleteClick(Sender: TObject);
    procedure FormDblClick(Sender: TObject);
  protected
    procedure SetData(_Notifications: TObjectList);
    procedure GetData(_Notifications: TObjectList);
  public
    class function Execute(_Owner: TComponent; _Notifications: TObjectList): Boolean;
    constructor Create(_Owner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
{$IFOPT D+}GX_DbugIntf,
{$ENDIF}
  u_dzVclUtils,
  GX_Experts,
  GX_GExperts,
  GX_ConfigurationInfo,
  GX_EditExceptionNotification,
  GX_IdeDialogEnhancer,
  GX_TimedCallback,
  TypInfo,
  SynRegExpr;

type
  TExceptionNotification = class
  private
    FName: string;
    FMessageRE: string;
    FAction: TExceptionNotificationAction;
  public
    constructor Create(const _Name: string; const _MessageRE: string;
      _Action: TExceptionNotificationAction);
    function ActionStr: string; overload;
    class function ActionStr(_Action: TExceptionNotificationAction): string; overload;
    property Name: string read FName write FName;
    property MessageRE: string read FMessageRE write FMessageRE;
    property Action: TExceptionNotificationAction read FAction write FAction;
  end;

type
  TOnCheckException = procedure(_Sender: TObject; const _Message: string;
    var _Action: TExceptionNotificationAction) of object;

type
  TExceptionNotificationHandler = class(TIdeDialogEnhancer)
  private
    FButtonToPress: TButton;
    FOnCheckException: TOnCheckException;
    procedure HandleCallbackTimer(_Sender: TObject);
  protected
    constructor Create(_OnCheckException: TOnCheckException);
    function IsDesiredForm(_Form: TCustomForm): Boolean; override;
    procedure EnhanceForm(_Form: TForm); override;
  public
  end;

type
  TGxCloseExceptionNotificationExpert = class(TGX_Expert)
  private
    FNotifications: TObjectList;
    FHandler: TExceptionNotificationHandler;
  protected
    procedure SetActive(_Active: Boolean); override;
    procedure HandleCheckException(_Sender: TObject; const _Message: string;
      var _Action: TExceptionNotificationAction);
  public
    constructor Create; override;
    destructor Destroy; override;
    function HasMenuItem: Boolean; override;
    class function GetName: string; override;
    function GetDisplayName: string; override;
    function GetHelpString: string; override;
    procedure Configure; override;
    procedure InternalLoadSettings(_Settings: IExpertSettings); override;
    procedure InternalSaveSettings(_Settings: IExpertSettings); override;
    procedure Execute(Sender: TObject); override;
  end;

{ TGxCloseExceptionNotificationExpert }

procedure TGxCloseExceptionNotificationExpert.Execute(Sender: TObject);
begin
  // This doesn't have a menu item, so this should never be called, but in case I overlooked
  // something, call the configuration.
  Configure;
end;

procedure TGxCloseExceptionNotificationExpert.Configure;
begin
  if TfmGxCloseExceptionNotificationExpert.Execute(nil, FNotifications) then begin
    SaveSettings;
  end;
end;

constructor TGxCloseExceptionNotificationExpert.Create;
begin
  inherited Create;
  FNotifications := TObjectList.Create(True);
end;

destructor TGxCloseExceptionNotificationExpert.Destroy;
begin
  FreeAndNil(FNotifications);
  inherited Destroy;
end;

function TGxCloseExceptionNotificationExpert.GetDisplayName: string;
resourcestring
  SCloseExceptionNotification = 'Close Exception Notification';
begin
  Result := SCloseExceptionNotification;
end;

function TGxCloseExceptionNotificationExpert.GetHelpString: string;
resourcestring
  SCloseExceptionNotificationExpert =
    'Automatically close exception notifications based on exception class ' +
    'and message.';
begin
  Result := SCloseExceptionNotificationExpert;
end;

class function TGxCloseExceptionNotificationExpert.GetName: string;
begin
  Result := 'CloseExceptionNotification';
end;

function TGxCloseExceptionNotificationExpert.HasMenuItem: Boolean;
begin
  Result := False;
end;

procedure TGxCloseExceptionNotificationExpert.InternalLoadSettings(_Settings: IExpertSettings);
var
  SubKey: IExpertSettings;
  cnt: Integer;
  i: Integer;
  Item: string;
  ExceptionName: string;
  ExceptionMsg: string;
  ActionStr: string;
  Action: TExceptionNotificationAction;
begin
  inherited;
  FNotifications.Clear;
  SubKey := _Settings.SubKey('Items');
  cnt := SubKey.ReadInteger('Count', 0);
  for i := 0 to cnt - 1 do begin
    Item := 'Item' + IntToStr(i) + '.';
    ExceptionName := SubKey.ReadString(Item + 'Name', '');
    ExceptionMsg := SubKey.ReadString(Item + 'MessageRE', '');
    ActionStr := SubKey.ReadString(Item + 'Action', '');
    if (ExceptionName <> '') and (ExceptionMsg <> '') then begin
      if ActionStr = TExceptionNotification.ActionStr(enaIgnore) then
        Action := enaIgnore
      else if ActionStr = TExceptionNotification.ActionStr(enaBreak) then
        Action := enaBreak
      else
        Action := enaDisabled;
      FNotifications.Add(TExceptionNotification.Create(ExceptionName, ExceptionMsg, Action));
    end;
  end;
end;

procedure TGxCloseExceptionNotificationExpert.InternalSaveSettings(_Settings: IExpertSettings);
var
  SubKey: IExpertSettings;
  i: Integer;
  cnt: Integer;
  Item: string;
  Notification: TExceptionNotification;
begin
  inherited;
  cnt := FNotifications.Count;
  _Settings.EraseSection('Items');
  SubKey := _Settings.SubKey('Items');
  SubKey.WriteInteger('Count', cnt);
  for i := 0 to FNotifications.Count - 1 do begin
    Item := 'Item' + IntToStr(i) + '.';
    Notification := FNotifications[i] as TExceptionNotification;
    SubKey.WriteString(Item + 'Name', Notification.Name);
    SubKey.WriteString(Item + 'MessageRE', Notification.MessageRE);
    SubKey.WriteString(Item + 'Action', Notification.ActionStr);
  end;
end;

procedure TGxCloseExceptionNotificationExpert.SetActive(_Active: Boolean);
begin
  if _Active <> Active then
    inherited SetActive(_Active);

  if _Active then begin
    if not Assigned(FHandler) then
      FHandler := TExceptionNotificationHandler.Create(HandleCheckException);
  end else begin
    FreeAndNil(FHandler);
  end;
end;

procedure TGxCloseExceptionNotificationExpert.HandleCheckException(_Sender: TObject;
  const _Message: string; var _Action: TExceptionNotificationAction);
const
  EN_PROJECT_ = 'Project ';
  EN_RAISED_EXCEPTION_CLASS_ = ' raised exception class ';
  EN_WITH_MESSAGE_ = ' with message ';
var
  re: TRegExpr;
  Project: string;
  ExceptionName: string;
  ExceptionMsg: string;
  i: Integer;
  Notification: TExceptionNotification;
begin
  re := TRegExpr.Create;
  try
    re.Expression := EN_PROJECT_ + '(.*)' + EN_RAISED_EXCEPTION_CLASS_ + '(.*)' + EN_WITH_MESSAGE_ + #39 + '(.*)' + #39'\.';
    if not re.Exec(_Message) or (re.SubExprMatchCount <> 3) then
      Exit; //==>

    Project := re.Match[1];
    ExceptionName := re.Match[2];
    ExceptionMsg := re.Match[3];
    for i := 0 to FNotifications.Count - 1 do begin
      Notification := FNotifications[i] as TExceptionNotification;
      if SameText(ExceptionName, Notification.Name) then begin
        re.Expression := Notification.MessageRE;
        if re.Exec(ExceptionMsg) then begin
          _Action := Notification.Action;
          Exit; //==>
        end;
      end;
    end;
  finally
    FreeAndNil(re);
  end;
end;

{ TExceptionNotification }

constructor TExceptionNotification.Create(const _Name: string; const _MessageRE: string;
  _Action: TExceptionNotificationAction);
begin
  inherited Create;
  FName := _Name;
  FMessageRE := _MessageRE;
  FAction := _Action;
end;

class function TExceptionNotification.ActionStr(_Action: TExceptionNotificationAction): string;
begin
  case _Action of
    enaIgnore: Result := 'Ignore';
    enaBreak: Result := 'Break';
  else // enaDisabled
    Result := 'Disabled';
  end;
end;

function TExceptionNotification.ActionStr: string;
begin
  Result := ActionStr(FAction);
end;

{ TfmGxCloseExceptionNotificationExpert }

class function TfmGxCloseExceptionNotificationExpert.Execute(_Owner: TComponent;
  _Notifications: TObjectList): Boolean;
var
  frm: TfmGxCloseExceptionNotificationExpert;
begin
  frm := TfmGxCloseExceptionNotificationExpert.Create(_Owner);
  try
    frm.SetData(_Notifications);
    Result := (frm.ShowModal = mrOk);
    if Result then
      frm.GetData(_Notifications);
  finally
    FreeAndNil(frm);
  end;
end;

procedure TfmGxCloseExceptionNotificationExpert.FormDblClick(Sender: TObject);
begin
  raise exception.Create('Error Message');
end;

procedure TfmGxCloseExceptionNotificationExpert.FormResize(Sender: TObject);
begin
  inherited;
  TGrid_Resize(sg_Exceptions, [roUseGridWidth, roUseAllRows], [0, 2]);
end;

procedure TfmGxCloseExceptionNotificationExpert.b_AddClick(Sender: TObject);
var
  row: Integer;
  ExceptionName: string;
  MessageRE: string;
  Action: TExceptionNotificationAction;
begin
  ExceptionName := 'Exception';
  MessageRE := 'Some error message';
  Action := enaIgnore;
  if not TfmGxEditExceptionNotification.Execute(Self, ExceptionName, MessageRE, Action) then
    Exit; //==>

  row := TStringGrid_AppendRow(sg_Exceptions);
  sg_Exceptions.Cells[0, row] := ExceptionName;
  sg_Exceptions.Cells[1, row] := MessageRE;
  sg_Exceptions.Objects[2, row] := Pointer(Ord(Action));
  sg_Exceptions.Cells[2, row] := TExceptionNotification.ActionStr(Action);
end;

procedure TfmGxCloseExceptionNotificationExpert.b_DeleteClick(Sender: TObject);
var
  row: Integer;
  ExceptionName: string;
  MessageRE: string;
begin
  row := sg_Exceptions.row;
  ExceptionName := sg_Exceptions.Cells[0, row];
  MessageRE := sg_Exceptions.Cells[1, row];
  if mrYes <> MessageDlg('Do you really want to delete this entry?'#13#10
    + ExceptionName + #13#10
    + MessageRE, mtWarning, [mbYes, mbCancel], 0) then
    Exit; //==>
  TStringGrid_DeleteRow(sg_Exceptions, row);
end;

procedure TfmGxCloseExceptionNotificationExpert.b_EditClick(Sender: TObject);
var
  row: Integer;
  ExceptionName: string;
  MessageRE: string;
  Action: TExceptionNotificationAction;
begin
  row := sg_Exceptions.row;
  ExceptionName := sg_Exceptions.Cells[0, row];
  MessageRE := sg_Exceptions.Cells[1, row];
  Action := TExceptionNotificationAction(Integer(sg_Exceptions.Objects[2, row]));
  if not TfmGxEditExceptionNotification.Execute(Self, ExceptionName, MessageRE, Action) then
    Exit; //==>
  sg_Exceptions.Cells[0, row] := ExceptionName;
  sg_Exceptions.Cells[1, row] := MessageRE;
  sg_Exceptions.Objects[2, row] := Pointer(Ord(Action));
  sg_Exceptions.Cells[2, row] := TExceptionNotification.ActionStr(Action);
end;

constructor TfmGxCloseExceptionNotificationExpert.Create(_Owner: TComponent);
begin
  inherited;
  TControl_SetMinConstraints(Self);

  sg_Exceptions.ColWidths[0] := 100;
  sg_Exceptions.ColWidths[1] := 250;
  sg_Exceptions.ColWidths[2] := 50;
  sg_Exceptions.Cells[0, 0] := 'Exception';
  sg_Exceptions.Cells[1, 0] := 'Message (RegEx)';
  sg_Exceptions.Cells[2, 0] := 'Action';
end;

procedure TfmGxCloseExceptionNotificationExpert.GetData(_Notifications: TObjectList);
var
  i: Integer;
  s: string;
  re: string;
  Action: TExceptionNotificationAction;
begin
  _Notifications.Clear;
  for i := sg_Exceptions.FixedRows to sg_Exceptions.RowCount - 1 do begin
    s := sg_Exceptions.Cells[0, i];
    if s <> '' then begin
      re := sg_Exceptions.Cells[1, i];
      Action := TExceptionNotificationAction(Integer(sg_Exceptions.Objects[2, i]));
      _Notifications.Add(TExceptionNotification.Create(s, re, Action))
    end;
  end;
end;

procedure TfmGxCloseExceptionNotificationExpert.SetData(_Notifications: TObjectList);
var
  cnt: Integer;
  i: Integer;
  LineIdx: Integer;
  Notification: TExceptionNotification;
begin
  cnt := _Notifications.Count;
  TGrid_SetNonfixedRowCount(sg_Exceptions, cnt);
  for i := 0 to cnt - 1 do begin
    LineIdx := sg_Exceptions.FixedRows + i;
    Notification := _Notifications[i] as TExceptionNotification;
    sg_Exceptions.Cells[0, LineIdx] := Notification.Name;
    sg_Exceptions.Cells[1, LineIdx] := Notification.MessageRE;
    sg_Exceptions.Objects[2, LineIdx] := Pointer(Ord(Notification.Action));
    sg_Exceptions.Cells[2, LineIdx] := Notification.ActionStr;
  end;
  TGrid_Resize(sg_Exceptions, [roUseGridWidth, roUseAllRows], [0, 2]);
end;

{ TExceptionNotificationHandler }

constructor TExceptionNotificationHandler.Create(_OnCheckException: TOnCheckException);
begin
  inherited Create;
  FOnCheckException := _OnCheckException;
end;

procedure TExceptionNotificationHandler.EnhanceForm(_Form: TForm);
var
  lbl: TComponent;
  btn: TComponent;
  LblText: string;
  Action: TExceptionNotificationAction;
begin
  FButtonToPress := nil;

  if not Assigned(FOnCheckException) then
    Exit; //==>

  lbl := _Form.FindComponent('lbExceptionMessage');
  if not Assigned(lbl) then
    Exit; //==>

  LblText := GetStrProp(lbl, 'Caption');
  Action := enaDisabled;
  FOnCheckException(Self, LblText, Action);
  case Action of
    enaIgnore:
      btn := _Form.FindComponent('ContinueButton');
    enaBreak:
      btn := _Form.FindComponent('BreakButton');
  else // enaDisabled:
    btn := nil;
  end;
  if Assigned(btn) and (btn is TButton) then begin
    FButtonToPress := TButton(btn);
    TTimedCallback.Create(HandleCallbackTimer, 50, True);
  end;
end;

procedure TExceptionNotificationHandler.HandleCallbackTimer(_Sender: TObject);
begin
  if Assigned(FButtonToPress) then
    FButtonToPress.Click;
end;

function TExceptionNotificationHandler.IsDesiredForm(_Form: TCustomForm): Boolean;
const
  DIALOG_CLASS = 'TExceptionNotificationDlg';
  DIALOG_NAME = 'ExceptionNotificationDlg';
begin
  FButtonToPress := nil;
  Result := (_Form.ClassName = DIALOG_CLASS) and (_Form.Name = DIALOG_NAME);
end;

initialization
  RegisterGX_Expert(TGxCloseExceptionNotificationExpert);
end.
