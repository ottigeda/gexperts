unit GX_FilterExceptions;

{$I GX_CondDefine.inc}

{$IFNDEF GX_DELPHI2005_UP}
'This only works for Delphi 2005 and newer'
{$ENDIF}

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
  ToolsAPI,
  GX_BaseForm,
  GX_FilterExceptionsEdit;

type
  TfmGxFilterExceptionsExpert = class(TfmBaseForm)
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
    procedure sg_ExceptionsDblClick(Sender: TObject);
  private
    function TryGetCurrentEntry(out _Row: Integer; out _Project, _ExceptionName, _MessageRE: string;
      out _Action: TExceptionFilterAction): Boolean;
    procedure EditCurrentEntry;
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
  TypInfo,
  StrUtils,
  u_dzVclUtils,
  u_dzStringUtils,
  SynRegExpr,
  GX_Experts,
  GX_GExperts,
  GX_ConfigurationInfo,
  GX_OtaUtils,
  GX_FilterExceptionsNotification;

const
  COL_PROJECT = 0;
  COL_EXCEPTION = 1;
  COL_MESSAGE = 2;
  COL_ACTION = 3;

type
  TExceptionFilter = class
  private
    FProject: string;
    FExceptionClass: string;
    FMessageRE: string;
    FAction: TExceptionFilterAction;
  public
    constructor Create(const _Project, _ExceptionClass, _MessageRE: string;
      _Action: TExceptionFilterAction);
    function ActionStr: string; overload;
    class function ActionStr(_Action: TExceptionFilterAction): string; overload;
    property Project: string read FProject write FProject;
    property ExceptionClass: string read FExceptionClass write FExceptionClass;
    property MessageRE: string read FMessageRE write FMessageRE;
    property Action: TExceptionFilterAction read FAction write FAction;
  end;

type
  TOnCheckException = procedure(_Sender: TObject; const _Message: string;
    var _Action: TExceptionFilterAction) of object;

type
  TGxFilterExceptionsDebuggerNotifier = class(TBaseDebuggerNotifier)
  private
    FDestroyedCallback: TNotifyEvent;
  public
    constructor Create(_DestroyedCallback: TNotifyEvent);
    procedure ProcessDestroyed(const Process: IOTAProcess); override;
  end;

type
  TGxFilterExceptionsExpert = class(TGX_Expert)
  private
    FNotifications: TObjectList;
    FDebuggerNotifier: TGxFilterExceptionsDebuggerNotifier;
    procedure HandleCheckException(_Sender: TObject; const _Project, _ExceptionClass, _Message: string;
      var _Action: TExceptionFilterAction);
    procedure HandleAddException(_Sender: TObject; const _Project, _ExceptionClass, _Message: string;
      var _Action: TExceptionFilterAction);
    procedure HandleProcessDestroyed(_Sender: TObject);
  protected
    procedure SetActive(_Active: Boolean); override;
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

procedure TGxFilterExceptionsExpert.Execute(Sender: TObject);
begin
  // This expert does not have a menu item, so this should never be called, but in case I overlooked
  // something, call the configuration.
  Configure;
end;

procedure TGxFilterExceptionsExpert.Configure;
begin
  if TfmGxFilterExceptionsExpert.Execute(nil, FNotifications) then begin
    SaveSettings;
  end;
end;

constructor TGxFilterExceptionsExpert.Create;
begin
  inherited Create;
  FNotifications := TObjectList.Create(True);
  FDebuggerNotifier := TGxFilterExceptionsDebuggerNotifier.Create(HandleProcessDestroyed);
  FDebuggerNotifier.AddNotifierToIDE;
end;

destructor TGxFilterExceptionsExpert.Destroy;
begin
  if Assigned(FDebuggerNotifier) then
    FDebuggerNotifier.RemoveNotifierFromIDE;
  FDebuggerNotifier := nil;
  FreeAndNil(FNotifications);
  inherited Destroy;
end;

procedure TGxFilterExceptionsExpert.HandleProcessDestroyed(_Sender: TObject);
var
  i: Integer;
  Notification: TExceptionFilter;
begin
  for i := FNotifications.Count - 1 downto 0 do begin
    Notification := FNotifications[i] as TExceptionFilter;
    if Notification.Project = '' then
      FNotifications.Delete(i);
  end;
end;

function TGxFilterExceptionsExpert.GetDisplayName: string;
resourcestring
  SFilterExceptions = 'Filter Exceptions';
begin
  Result := SFilterExceptions;
end;

function TGxFilterExceptionsExpert.GetHelpString: string;
resourcestring
  SCloseExceptionNotificationExpert =
    'Filter exception notifications based on exception class and message.';
begin
  Result := SCloseExceptionNotificationExpert;
end;

class function TGxFilterExceptionsExpert.GetName: string;
begin
  Result := 'FilterExceptions';
end;

function TGxFilterExceptionsExpert.HasMenuItem: Boolean;
begin
  Result := False;
end;

procedure TGxFilterExceptionsExpert.InternalLoadSettings(_Settings: IExpertSettings);
var
  SubKey: IExpertSettings;
  cnt: Integer;
  i: Integer;
  Item: string;
  Project: string;
  ExceptionClass: string;
  ExceptionMsg: string;
  ActionStr: string;
  Action: TExceptionFilterAction;
begin
  inherited;
  FNotifications.Clear;
  SubKey := _Settings.SubKey('Items');
  cnt := SubKey.ReadInteger('Count', 0);
  for i := 0 to cnt - 1 do begin
    Item := 'Item' + IntToStr(i) + '.';
    Project := SubKey.ReadString(Item + 'Project', '.*');
    ExceptionClass := SubKey.ReadString(Item + 'Name', '');
    ExceptionMsg := SubKey.ReadString(Item + 'MessageRE', '');
    ActionStr := SubKey.ReadString(Item + 'Action', '');
    if (ExceptionClass <> '') and (ExceptionMsg <> '') then begin
      if ActionStr = TExceptionFilter.ActionStr(efaIgnore) then
        Action := efaIgnore
      else if ActionStr = TExceptionFilter.ActionStr(efaBreak) then
        Action := efaBreak
      else
        Action := efaDisabled;
      FNotifications.Add(TExceptionFilter.Create(Project, ExceptionClass, ExceptionMsg, Action));
    end;
  end;
end;

procedure TGxFilterExceptionsExpert.InternalSaveSettings(_Settings: IExpertSettings);
var
  SubKey: IExpertSettings;
  i: Integer;
  cnt: Integer;
  Item: string;
  Notification: TExceptionFilter;
begin
  inherited;
  cnt := FNotifications.Count;
  _Settings.EraseSection('Items');
  SubKey := _Settings.SubKey('Items');
  SubKey.WriteInteger('Count', cnt);
  for i := 0 to FNotifications.Count - 1 do begin
    Item := 'Item' + IntToStr(i) + '.';
    Notification := FNotifications[i] as TExceptionFilter;
    SubKey.WriteString(Item + 'Project', Notification.Project);
    SubKey.WriteString(Item + 'Name', Notification.ExceptionClass);
    SubKey.WriteString(Item + 'MessageRE', Notification.MessageRE);
    SubKey.WriteString(Item + 'Action', Notification.ActionStr);
  end;
end;

procedure TGxFilterExceptionsExpert.SetActive(_Active: Boolean);
begin
  if _Active <> Active then
    inherited SetActive(_Active);

  if _Active then begin
    GX_FilterExceptionsNotification.InstallHook(HandleCheckException, HandleAddException);
  end else begin
    GX_FilterExceptionsNotification.UninstallHook;
  end;
end;

function TryGetMessageParts(const _Message: string;
  out _Project, _ExceptionName, _ExceptionMsg: string): Boolean;

  function TryGetStringParts(const _Message: string;
    out _InProject, _RaisedClass, _WithMessage, _Tail: string): Boolean;
  const
    EN_PROJECT_ = 'Project ';
    EN_RAISED_EXCEPTION_CLASS_ = ' raised exception class ';
    EN_WITH_MESSAGE_ = ' with message '#39;
    EN_TAIL = #39'.';
    DE_PROJECT_ = 'Im Projekt ';
    DE_RAISED_EXCEPTION_CLASS_ = ' ist eine Exception der Klasse ';
    DE_WITH_MESSAGE_ = ' mit der Meldung '#39;
    DE_TAIL = #39' aufgetreten';
    FR_PROJECT_ = 'Le projet ';
    FR_RAISED_EXCEPTION_CLASS_ = ' a déclenché la classe d''exception ';
    FR_WITH_MESSAGE_ = ' avec le message '#39;
    FR_TAIL = #39'.';
// 'Im Projekt Project1.exe ist eine Exception der Klasse Exception mit der Meldung 'bla' aufgetreten.'
// 'Le projet Project1.exe a déclenché la classe d'exception Exception avec le message 'bla'.'
  begin
    Result := True;
    if StartsText(EN_PROJECT_, _Message) then begin
      _InProject := EN_PROJECT_;
      _RaisedClass := EN_RAISED_EXCEPTION_CLASS_;
      _WithMessage := EN_WITH_MESSAGE_;
      _Tail := EN_TAIL;
    end else if StartsText(DE_PROJECT_, _Message) then begin
      _InProject := DE_PROJECT_;
      _RaisedClass := DE_RAISED_EXCEPTION_CLASS_;
      _WithMessage := DE_WITH_MESSAGE_;
      _Tail := DE_TAIL;
    end else if StartsText(FR_PROJECT_, _Message) then begin
      _InProject := FR_PROJECT_;
      _RaisedClass := FR_RAISED_EXCEPTION_CLASS_;
      _WithMessage := FR_WITH_MESSAGE_;
      _Tail := FR_TAIL;
//    end else if StartsText(JP_PROJECT_, _Message) then begin
    end else
      Result := False;
  end;

var
  InProject: string;
  RaisedClass: string;
  WithMessage: string;
  Tail: string;
  re: TRegExpr;
begin
  Result := TryGetStringParts(_Message, InProject, RaisedClass, WithMessage, Tail);
  if not Result then
    Exit; //==>

  re := TRegExpr.Create;
  try
    re.Expression := InProject + '(.*)' + RaisedClass + '(.*)' + WithMessage + '(.*)' + Tail;
    if not re.Exec(_Message) or (re.SubExprMatchCount <> 3) then
      Exit; //==>

    _Project := re.Match[1];
    _ExceptionName := re.Match[2];
    _ExceptionMsg := re.Match[3];
  finally
    FreeAndNil(re);
  end;
end;

procedure TGxFilterExceptionsExpert.HandleAddException(_Sender: TObject;
  const _Project, _ExceptionClass, _Message: string; var _Action: TExceptionFilterAction);
var
  Project: string;
  ExceptionClass: string;
  MessageRE: string;
begin
  Project := _Project;
  ExceptionClass := _ExceptionClass;
  MessageRE := '';
  if TfmGxFilterExceptionsEdit.Execute(nil, _Message, Project, ExceptionClass, MessageRE, _Action) then
    FNotifications.Add(TExceptionFilter.Create(Project, ExceptionClass, MessageRE, _Action));
end;

procedure TGxFilterExceptionsExpert.HandleCheckException(_Sender: TObject;
  const _Project, _ExceptionClass, _Message: string; var _Action: TExceptionFilterAction);
var
  i: Integer;
  re: TRegExpr;
  Notification: TExceptionFilter;
begin
  re := TRegExpr.Create;
  try
    for i := 0 to FNotifications.Count - 1 do begin
      Notification := FNotifications[i] as TExceptionFilter;
      if SameText(_ExceptionClass, Notification.ExceptionClass) then begin
        re.Expression := Notification.Project;
        if (Notification.Project = '') or re.Exec(_Project) then begin
          re.Expression := Notification.MessageRE;
          if re.Exec(_Message) then begin
            _Action := Notification.Action;
            Exit; //==>
          end;
        end;
      end;
    end;
  finally
    FreeAndNil(re);
  end;
end;

{ TExceptionNotification }

constructor TExceptionFilter.Create(const _Project, _ExceptionClass, _MessageRE: string;
  _Action: TExceptionFilterAction);
begin
  inherited Create;
  FProject := _Project;
  FExceptionClass := _ExceptionClass;
  FMessageRE := _MessageRE;
  FAction := _Action;
end;

class function TExceptionFilter.ActionStr(_Action: TExceptionFilterAction): string;
begin
  case _Action of
    efaIgnore: Result := 'Ignore';
    efaBreak: Result := 'Break';
  else // enaDisabled
    Result := 'Disabled';
  end;
end;

function TExceptionFilter.ActionStr: string;
begin
  Result := ActionStr(FAction);
end;

{ TfmGxFilterExceptionsExpert }

class function TfmGxFilterExceptionsExpert.Execute(_Owner: TComponent;
  _Notifications: TObjectList): Boolean;
var
  frm: TfmGxFilterExceptionsExpert;
begin
  frm := TfmGxFilterExceptionsExpert.Create(_Owner);
  try
    frm.SetData(_Notifications);
    Result := (frm.ShowModal = mrOk);
    if Result then
      frm.GetData(_Notifications);
  finally
    FreeAndNil(frm);
  end;
end;

procedure TfmGxFilterExceptionsExpert.FormDblClick(Sender: TObject);
begin
  raise Exception.Create('Error Message');
end;

procedure TfmGxFilterExceptionsExpert.FormResize(Sender: TObject);
begin
  inherited;
  sg_Exceptions.ColWidths[COL_PROJECT] := 100;
  sg_Exceptions.ColWidths[COL_EXCEPTION] := 100;
  sg_Exceptions.ColWidths[COL_MESSAGE] := sg_Exceptions.ClientWidth - 250 - 3;
  sg_Exceptions.ColWidths[COL_ACTION] := 50;
end;

procedure TfmGxFilterExceptionsExpert.b_AddClick(Sender: TObject);
var
  row: Integer;
  ExceptionName: string;
  MessageRE: string;
  Action: TExceptionFilterAction;
  Project: string;
begin
  Project := '.*';
  ExceptionName := 'Exception';
  MessageRE := 'Some error message';
  Action := efaIgnore;
  if not TfmGxFilterExceptionsEdit.Execute(Self, '', Project, ExceptionName, MessageRE, Action) then
    Exit; //==>

  row := TStringGrid_AppendRow(sg_Exceptions, True);
  sg_Exceptions.Cells[COL_PROJECT, row] := Project;
  sg_Exceptions.Cells[COL_EXCEPTION, row] := ExceptionName;
  sg_Exceptions.Cells[COL_MESSAGE, row] := MessageRE;
  sg_Exceptions.Objects[COL_ACTION, row] := Pointer(Ord(Action));
  sg_Exceptions.Cells[COL_ACTION, row] := TExceptionFilter.ActionStr(Action);
end;

function TfmGxFilterExceptionsExpert.TryGetCurrentEntry(out _Row: Integer;
  out _Project, _ExceptionName, _MessageRE: string;
  out _Action: TExceptionFilterAction): Boolean;
begin
  _Row := sg_Exceptions.row;
  _Project := sg_Exceptions.Cells[COL_PROJECT, _Row];
  _ExceptionName := sg_Exceptions.Cells[COL_EXCEPTION, _Row];
  Result := (_ExceptionName <> '');
  if Result then begin
    _MessageRE := sg_Exceptions.Cells[COL_MESSAGE, _Row];
    _Action := TExceptionFilterAction(Integer(sg_Exceptions.Objects[COL_ACTION, _Row]));
  end;
end;

procedure TfmGxFilterExceptionsExpert.b_DeleteClick(Sender: TObject);
var
  row: Integer;
  Project: string;
  ExceptionClass: string;
  MessageRE: string;
  Action: TExceptionFilterAction;
begin
  if not TryGetCurrentEntry(row, Project, ExceptionClass, MessageRE, Action) then
    Exit; //==>

  if mrYes <> MessageDlg(Format('Do you really want to delete this filter?'#13#10
    + 'Project: "%s"'#13#10
    + 'Exception: "%s"'#13#10
    + 'Message: "%s"', [Project, ExceptionClass, MessageRE]),
    mtWarning, [mbYes, mbCancel], 0) then
    Exit; //==>

  TStringGrid_DeleteRow(sg_Exceptions, row);
end;

procedure TfmGxFilterExceptionsExpert.b_EditClick(Sender: TObject);
begin
  EditCurrentEntry;
end;

procedure TfmGxFilterExceptionsExpert.EditCurrentEntry;
var
  row: Integer;
  Project: string;
  ExceptionClass: string;
  MessageRE: string;
  Action: TExceptionFilterAction;
begin
  if not TryGetCurrentEntry(row, Project, ExceptionClass, MessageRE, Action) then
    Exit; //==>

  if not TfmGxFilterExceptionsEdit.Execute(Self, '', Project, ExceptionClass, MessageRE, Action) then
    Exit; //==>

  sg_Exceptions.Cells[COL_PROJECT, row] := Project;
  sg_Exceptions.Cells[COL_EXCEPTION, row] := ExceptionClass;
  sg_Exceptions.Cells[COL_MESSAGE, row] := MessageRE;
  sg_Exceptions.Objects[COL_ACTION, row] := Pointer(Ord(Action));
  sg_Exceptions.Cells[COL_ACTION, row] := TExceptionFilter.ActionStr(Action);
end;

constructor TfmGxFilterExceptionsExpert.Create(_Owner: TComponent);
begin
  inherited;
  TControl_SetMinConstraints(Self);

  sg_Exceptions.ColWidths[COL_PROJECT] := 100;
  sg_Exceptions.ColWidths[COL_EXCEPTION] := 100;
  sg_Exceptions.ColWidths[COL_MESSAGE] := 250;
  sg_Exceptions.ColWidths[COL_ACTION] := 50;
  sg_Exceptions.Cells[COL_PROJECT, 0] := 'Project';
  sg_Exceptions.Cells[COL_EXCEPTION, 0] := 'Exception';
  sg_Exceptions.Cells[COL_MESSAGE, 0] := 'Message (RegEx)';
  sg_Exceptions.Cells[COL_ACTION, 0] := 'Action';
end;

procedure TfmGxFilterExceptionsExpert.GetData(_Notifications: TObjectList);
var
  i: Integer;
  Project: string;
  ExceptionClass: string;
  re: string;
  Action: TExceptionFilterAction;
begin
  _Notifications.Clear;
  for i := sg_Exceptions.FixedRows to sg_Exceptions.RowCount - 1 do begin
    Project := sg_Exceptions.Cells[COL_PROJECT, i];
    ExceptionClass := sg_Exceptions.Cells[COL_EXCEPTION, i];
    if ExceptionClass <> '' then begin
      re := sg_Exceptions.Cells[COL_MESSAGE, i];
      Action := TExceptionFilterAction(Integer(sg_Exceptions.Objects[COL_ACTION, i]));
      _Notifications.Add(TExceptionFilter.Create(Project, ExceptionClass, re, Action))
    end;
  end;
end;

procedure TfmGxFilterExceptionsExpert.SetData(_Notifications: TObjectList);
var
  cnt: Integer;
  i: Integer;
  LineIdx: Integer;
  Notification: TExceptionFilter;
begin
  cnt := _Notifications.Count;
  TGrid_SetNonfixedRowCount(sg_Exceptions, cnt);
  for i := 0 to cnt - 1 do begin
    LineIdx := sg_Exceptions.FixedRows + i;
    Notification := _Notifications[i] as TExceptionFilter;
    sg_Exceptions.Cells[COL_PROJECT, LineIdx] := Notification.Project;
    sg_Exceptions.Cells[COL_EXCEPTION, LineIdx] := Notification.ExceptionClass;
    sg_Exceptions.Cells[COL_MESSAGE, LineIdx] := Notification.MessageRE;
    sg_Exceptions.Objects[COL_ACTION, LineIdx] := Pointer(Ord(Notification.Action));
    sg_Exceptions.Cells[COL_ACTION, LineIdx] := Notification.ActionStr;
  end;
  TGrid_Resize(sg_Exceptions, [roUseGridWidth, roUseAllRows], [COL_PROJECT, COL_EXCEPTION, COL_ACTION]);
end;

procedure TfmGxFilterExceptionsExpert.sg_ExceptionsDblClick(Sender: TObject);
begin
  EditCurrentEntry;
end;

{ TGxCloseExceptionDebuggerNotifier }

constructor TGxFilterExceptionsDebuggerNotifier.Create(_DestroyedCallback: TNotifyEvent);
begin
  inherited Create;
  FDestroyedCallback := _DestroyedCallback;
end;

procedure TGxFilterExceptionsDebuggerNotifier.ProcessDestroyed(const Process: IOTAProcess);
begin
  inherited;
  if Assigned(FDestroyedCallback) then
    FDestroyedCallback(nil);
end;

initialization
  RegisterGX_Expert(TGxFilterExceptionsExpert);
end.
