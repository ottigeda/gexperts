unit GX_AutoSwitchDesktop;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Controls,
  u_dzTypes,
  GX_ConfigurationInfo;

implementation

uses
  Menus,
  u_dzVclUtils,
  u_dzConvertUtils,
  GX_Experts,
{$IFOPT D+}GX_DbugIntf, {$ENDIF}
  GX_IdeUtils,
  GX_OtaUtils,
  GX_AutoSwitchDesktopConfig;

const
  WM_F12 = WM_USER;

type
  TAutoSwitchDesktopExpert = class(TGX_Expert)
  private
    FIsIdeInitialized: Boolean;
    FMsgHandlerHWND: HWND;
    FKBHook: HHOOK;
    FFormDesktop: string;
    FCodeDesktop: string;
    procedure HandleF12Press;
    procedure InstallKeyboardHook;
    procedure UninstallKeyboardHook;
    procedure WndMethod(var Msg: Messages.TMessage);
    procedure CallHandleF12Press;
  protected
    procedure SetActive(New: Boolean); override;
    // displays a dialog saying there are no configuration options
    // see also HasConfigOptions
    procedure Configure(_Owner: TWinControl); override;
    procedure InternalLoadSettings(_Settings: IExpertSettings); override;
    procedure InternalSaveSettings(_Settings: IExpertSettings); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    ///<summary>
    /// Describes the purpose of the expert. Displayed as hint if the user
    /// points the mouse on the expert's icon in the configuration dialog. </summary>
    function GetHelpString: string; override;
    ///<summary>
    /// Name to be displayed for the expert in the GExperts *configuration*
    /// dialog; this is a different entry than the action caption
    /// (GetActionCaption) but by default it calls GetActionCaption and
    /// removes any Hotkey characters and '...' This is probably OK for
    /// most experts. </summary>
    function GetDisplayName: string; override;
    // Defaults to True
    function HasMenuItem: Boolean; override;
    // Returns true, see also Configure
    function HasConfigOptions: Boolean; override;
    function CanHaveShortCut: Boolean; override;
    // Do any delayed setup after the IDE is done initializing
    procedure AfterIDEInitialized; override;
  end;

var
  TheExpert: TAutoSwitchDesktopExpert = nil;

{$IF not declared(HC_ACTION)}
const
  HC_Action = 0;
{$IFEND}
{$IF not declared(HC_NOREMOVE)}
const
  HC_NOREMOVE = 3;
{$IFEND}

function GetShiftState: TShiftState;
var
  LState: SmallInt;
begin
  Result := [];

  LState := GetAsyncKeyState(VK_CONTROL) and $8000;
  if LState <> 0 then Include(Result, ssCtrl);

  LState := GetAsyncKeyState(VK_SHIFT) and $8000;
  if LState <> 0 then Include(Result, ssShift);

  LState := GetAsyncKeyState(VK_MENU) and $8000;
  if LState <> 0 then Include(Result, ssAlt);
end;

function KeyboardHookProc(Code: Integer; wparam: wparam; lparam: lparam): LRESULT; stdcall;
begin
  if Assigned(TheExpert) and TheExpert.Active then begin
    if Code = 0 then begin
      if (wparam = VK_F12) and (GetShiftState = []) then begin
{$IFOPT D+}SendDebugFmt('F12: Code: %d WordParam: %d LongParam: %s', [Code, wparam, TBits32.Create(UInt32(lparam)).AsString(bgNibble)]); {$ENDIF}
        if UInt32(lparam) and UInt32(1 shl 31) <> 0 then begin
{$IFOPT D+}SendDebugFmt('F12 was released. Code: %d WordParam: %d LongParam: %d', [Code, wparam, lparam]); {$ENDIF}
          TheExpert.CallHandleF12Press;
        end;
      end;
    end;
  end;
  Result := CallNextHookEx(0, Code, wparam, lparam);
end;

{ TAutoSwitchDesktopExpert }

constructor TAutoSwitchDesktopExpert.Create;
begin
  inherited Create;
  FMsgHandlerHWND := AllocateHWnd(WndMethod);
  TheExpert := self;
end;

destructor TAutoSwitchDesktopExpert.Destroy;
begin
  TheExpert := nil;
  UninstallKeyboardHook;
  DeallocateHWnd(FMsgHandlerHWND);
  inherited;
end;

procedure TAutoSwitchDesktopExpert.AfterIDEInitialized;
begin
  inherited;
  FIsIdeInitialized := True;
  if Active then
    InstallKeyboardHook;
end;

procedure TAutoSwitchDesktopExpert.InstallKeyboardHook;
begin
  if FKBHook = 0 then
    FKBHook := SetWindowsHookEx(WH_KEYBOARD, TFNHookProc(@KeyboardHookProc), HInstance, GetCurrentThreadId());
end;

procedure TAutoSwitchDesktopExpert.UninstallKeyboardHook;
begin
  if FKBHook <> 0 then begin
    UnHookWindowsHookEx(FKBHook);
    FKBHook := 0;
  end;
end;

procedure TAutoSwitchDesktopExpert.CallHandleF12Press;
begin
  PostMessage(TheExpert.FMsgHandlerHWND, WM_F12, 0, 0);
end;

procedure TAutoSwitchDesktopExpert.WndMethod(var Msg: Messages.TMessage);
begin
  if Msg.Msg = WM_F12 then begin
    HandleF12Press;
    Msg.Result := 0;
  end else
    Msg.Result := DefWindowProc(FMsgHandlerHWND, Msg.Msg, Msg.wparam, Msg.lparam);
end;

procedure TAutoSwitchDesktopExpert.HandleF12Press;
var
  Desktops: TStrings;
  Idx: Integer;
  mi: TMenuItem;
  DesktopToSelect: string;
begin
  if Ota.CurrentlyEditingForm then
    DesktopToSelect := FFormDesktop
  else
    DesktopToSelect := FCodeDesktop;
  if DesktopToSelect = '' then
    Exit; //==>

  Desktops := GetViewDesktopEntries;
  try
    Idx := Desktops.IndexOf(DesktopToSelect);
    if Idx <> -1 then begin
      mi := TMenuItem(Desktops.Objects[Idx]);
      if Assigned(mi) then
        mi.Click;
    end;
  finally
    FreeAndNil(Desktops);
  end;
end;

function TAutoSwitchDesktopExpert.GetDisplayName: string;
begin
  Result := 'Auto Switch Desktop';
end;

function TAutoSwitchDesktopExpert.GetHelpString: string;
begin
  Result := 'This expert switches to a different desktop when the user presses F12 '
    + 'to switch between the code editor and the form designer. Note that this does '
    + 'not apply if the user switches using between these views using the mouse.';
end;

function TAutoSwitchDesktopExpert.CanHaveShortCut: Boolean;
begin
  Result := False;
end;

procedure TAutoSwitchDesktopExpert.Configure(_Owner: TWinControl);
begin
  if Tf_AutoSwitchDestkopConfig.Execute(_Owner, FFormDesktop, FCodeDesktop) then
    ;
end;

function TAutoSwitchDesktopExpert.HasConfigOptions: Boolean;
begin
  Result := True;
end;

function TAutoSwitchDesktopExpert.HasMenuItem: Boolean;
begin
  Result := False;
end;

procedure TAutoSwitchDesktopExpert.InternalLoadSettings(_Settings: IExpertSettings);
begin
  inherited;
  FFormDesktop := _Settings.ReadString('FormDestkop', '');
  FCodeDesktop := _Settings.ReadString('CodeDestkop', '');
end;

procedure TAutoSwitchDesktopExpert.InternalSaveSettings(_Settings: IExpertSettings);
begin
  inherited;
  _Settings.WriteString('FormDestkop', FFormDesktop);
  _Settings.WriteString('CodeDestkop', FCodeDesktop);
end;

procedure TAutoSwitchDesktopExpert.SetActive(New: Boolean);
begin
  inherited SetActive(New);
  if not FIsIdeInitialized then
    Exit; //==>

  if Active then
    InstallKeyboardHook
  else
    UninstallKeyboardHook;
end;

initialization
  RegisterGX_Expert(TAutoSwitchDesktopExpert);
end.

