unit GX_ReselectDesktop;

{$I GX_CondDefine.inc}

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Controls,
  Forms,
  GX_Experts;

type
  TReselectDesktopExpert = class(TGX_Expert)
  private
{$IFDEF GX_DESKTOP_SIZE_FIX}
    procedure ForceIdePosition(const _DesktopName: string);
    procedure ForceStartupDesktop(_Sender: TObject);
  public
    procedure AfterIDEInitialized; override;
{$ENDIF GX_DESKTOP_SIZE_FIX}
  public
    function GetActionCaption: string; override;
    class function GetName: string; override;
    procedure Execute(Sender: TObject); override;
    function HasConfigOptions: Boolean; override;
    function GetHelpString: string; override;
  end;

implementation

uses
  StdCtrls,
  Menus,
  IniFiles,
  SHFolder,
  u_dzClassUtils,
  u_dzVclUtils,
  GX_IdeUtils,
  GX_VerDepConst,
  GX_DbugIntf,
  GX_TimedCallback;

{ TReslectDesktopExpert }

type
  TComboBoxHack = class(TComboBox)
  end;

{$IFDEF GX_DESKTOP_SIZE_FIX}
procedure TReselectDesktopExpert.AfterIDEInitialized;
var
  StartupDesktopName: string;
  RegKey: string;
begin
  inherited;

  RegKey := GetIdeBaseRegistryKey + '\Session';
  StartupDesktopName := TRegistry_ReadString(RegKey, 'StartupDesktopName', '', HKEY_CURRENT_USER);
{$IFOPT D+}SendDebugFmt('Startup Desktop: "%s"', [StartupDesktopName]);{$ENDIF}
  if SameText(StartupDesktopName, '<None>') then begin
    // the user has set the startup desktop to '<none>' so we won't do anything
    // todo: What if the IDE was started with a project as parameter?
    //       In that case it switches immediately to the DefaultDesktop.
    //       -> That combination is probably a very rare case, so we ignore it for now.
{$IFOPT D+}SendDebug('Exiting because Startup Desktop is <None>');{$ENDIF}
  end else begin
    // wait for the IDE to set the desktop and then fix the position
{$IFOPT D+}SendDebug('Waiting for IDE to set desktop');{$ENDIF}
    TTimedCallback.Create(ForceStartupDesktop, 500, True);
  end;
end;

procedure TReselectDesktopExpert.ForceStartupDesktop(_Sender: TObject);
var
  frm: TForm;
  cmp: TComponent;
  miView: TMenuItem absolute cmp;
  mi: TMenuItem;
  DesktopName: string;
begin
  frm := Application.MainForm;
  cmp := frm.FindComponent('ViewDesktopsMenu');
  if not Assigned(cmp) then
    Exit; //==>

  if Assigned(miView.OnClick) then begin
    miView.OnClick(miView);
    if TMenuItem_FindSelectedRadioItem(miView, -1, mi) then begin
      DesktopName := StripHotkey(mi.Caption);
      if SameText(DesktopName, '<None>') then begin
        // it's too early in the startup process, try again in 500 ms
{$IFOPT D+}SendDebug('Still waiting for IDE to set desktop');{$ENDIF}
        TTimedCallback.Create(ForceStartupDesktop, 500, True);
        Exit; //==>
      end;
{$IFOPT D+}SendDebugFmt('Forcing desktop to "%s"', [DesktopName]);{$ENDIF}
      ForceIdePosition(DesktopName);
    end;
  end;
end;

procedure TReselectDesktopExpert.ForceIdePosition(const _DesktopName: string);
const
  SECTION = 'Main Window';
var
  s: string;
  frm: TForm;
  R: TRect;
  L: Integer;
  T: Integer;
  w: Integer;
  h: Integer;
  State: integer;
  DstFn: string;
  Ini: TMemIniFile;
begin
  s := GetHomePath + '\' + CompanyRegPrefix + ProductVersionSuffix + '\';
  DstFn := s + _DesktopName + '.dst';
  if not FileExists(DstFn) then
    Exit; //==>

  Ini := TMemIniFile.Create(DstFn);
  try
    State := Ini.ReadInteger(SECTION, 'State', 0);
    if State <> 0 then begin
      // The window is probably maximized. We don't touch that.
      Exit; //==>
    end;
    L := Ini.ReadInteger(SECTION, 'Left', 0);
    T := Ini.ReadInteger(SECTION, 'Top', 0);
    w := Ini.ReadInteger(SECTION, 'Width', 0);
    h := Ini.ReadInteger(SECTION, 'Height', 0);
  finally
    FreeAndNil(Ini);
  end;
  // The position is stored in "percentages" of the main display's work area,
  // where "percentage" means 1/10000.
  R := Screen.PrimaryMonitor.WorkareaRect;
  L := L * (R.Right - R.Left) div 10000;
  T := T * (R.Bottom - R.Top) div 10000;
  w := w * (R.Right - R.Left) div 10000;
  h := h * (R.Bottom - R.Top) div 10000;

  frm := Application.MainForm;
  if frm.WindowState <> wsNormal then
    frm.WindowState := wsNormal;
  frm.SetBounds(L, T, w, h);

  // Send the WM_SIZE message to process the resize
  PostMessage(frm.Handle, WM_SIZE, SIZE_RESTORED, MakeLParam(w, h));

  // and once that's done, select the desktop again
  TTimedCallback.Create(Execute, 500, True);
end;
{$ENDIF GX_DESKTOP_SIZE_FIX}

procedure TReselectDesktopExpert.Execute(Sender: TObject);
var
  cbDesktop: TCustomComboBox;
begin
  if not TryGetDesktopCombo(cbDesktop) then
    Exit; //==>

{$IFOPT D+}SendDebug('Reselecting current desktop');{$ENDIF}
  TComboBoxHack(cbDesktop).Click;

  IncCallCount;
end;

function TReselectDesktopExpert.GetActionCaption: string;
resourcestring
  SMenuCaption = 'Reselect Desktop';
begin
  Result := SMenuCaption;
end;

function TReselectDesktopExpert.GetHelpString: string;
resourcestring
  SReselectDesktopHelpString =
    '  This expert does the same as selecting the currently active desktop again'#13#10
    + '  so it saves one mouse click or several key strokes.'#13#10
    + '  It is meant to be put as an icon onto the Desktop toolbar.';
begin
  Result := SReselectDesktopHelpString;
end;

class function TReselectDesktopExpert.GetName: string;
begin
  Result := 'ReselectDesktop';
end;

function TReselectDesktopExpert.HasConfigOptions: Boolean;
begin
  Result := False;
end;

initialization
  RegisterGX_Expert(TReselectDesktopExpert);
end.
