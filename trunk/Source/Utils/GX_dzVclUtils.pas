unit GX_dzVclUtils;

interface

{$I GX_CondDefine.inc}

{$ifdef GX_VER200_up}
  {$DEFINE SUPPORTS_UNICODE_STRING}
{$endif}

{$WARN UNIT_PLATFORM OFF}
{$WARN SYMBOL_PLATFORM OFF}

uses
  Classes,
  Windows,
  SysUtils,
  Controls,
  StdCtrls;

type
  ///<summary> Ancestor to all exceptions raised in this unit. </summary>
  EdzVclUtils = class(Exception);

type
  TAutoCompleteSourceEnum = (acsFileSystem, acsUrlHistory, acsUrlMru);
  TAutoCompleteSourceEnumSet = set of TAutoCompleteSourceEnum;
type
  TAutoCompleteTypeEnum = (actSuggest, actAppend);
  TAutoCompleteTypeEnumSet = set of TAutoCompleteTypeEnum;

///<summary>
/// Enables autocompletion for an edit control using a call to SHAutoComplete
/// NOTE that this will only work until the control handle is recreated which the VCL does
///      quite often. See TEdit_AutoComplete which handles this problem.
/// @param ed is a TCustomEdit control for which autocompletion should be enabled
/// @param Source is a set of TAutoCompleteSourceEnum that determines from which source to
///               autocomplate, any combination of acsFileSystem, acsUrlHistory, acsUrlMru
///               is allowed.
/// @param Type is a set fo TAutoCompleteEnumSet that determines the type of autocompletion
///             actAppend means that the first match will be appended to the existing text and
///                       selected, so that typing anything will automatically overwrite it.
///             actSuggest means that a list of matches will be displayed as a dropdown list
///                        from which the user can then select using the arrow keys or the mouse.
///             is is possible to pass an empty set, in which case the registry setting.
///             (Unfortunately MSDN doesn't say where in the registry this setting is located.)
/// @returns true, if autocomplete could be activated </summary>
function TEdit_SetAutocomplete(_ed: TCustomEdit; _Source: TAutoCompleteSourceEnumSet = [acsFileSystem];
  _Type: TAutoCompleteTypeEnumSet = []): Boolean;

///<summary>
/// Enables autocompletion for an edit control using a call to SHAutoComplete
/// @param ed is a TCustomEdit control for which autocompletion should be enabled
/// @param Source is a set of TAutoCompleteSourceEnum that determines from which source to
///               autocomplate, any combination of acsFileSystem, acsUrlHistory, acsUrlMru
///               is allowed.
/// @param Type is a set fo TAutoCompleteEnumSet that determines the type of autocompletion
///             actAppend means that the first match will be appended to the existing text and
///                       selected, so that typing anything will automatically overwrite it.
///             actSuggest means that a list of matches will be displayed as a dropdown list
///                        from which the user can then select using the arrow keys or the mouse.
///             is is possible to pass an empty set, in which case the registry setting.
///             (Unfortunately MSDN doesn't say where in the registry this setting is located.)
/// @returns the TAutoCompleteActivator instance created.
///          NOTE: You do not need to free this object! It will automatically be freed when the
///                TEdit is destroyed. </summary>
function TEdit_AutoComplete(_ed: TCustomEdit; _Source: TAutoCompleteSourceEnumSet = [acsFileSystem];
  _Type: TAutoCompleteTypeEnumSet = []): TObject;

type
  ///<summary>
  /// Event used in TWinControl_ActivateDropFiles.
  /// @param Sender is the control onto which the files were dropped
  /// @param Files is a TStrings containg the full names of all files dropped.
  ///              Use Files.DelimitedText to get a semicolon separated list of all files. </summary>
  TOnFilesDropped = procedure(_Sender: TObject; _Files: TStrings) of object;

///<summary>
/// Enables dropping of files (folders) from explorer to the given WinControl
/// @param WinCtrl is a TWinControl for which dropping should be enabled
/// @param Callback is a TOnFilesDropped event that is called when files are dropped on the form.
/// @returns the TAutoCompleteActivator instance created.
///          NOTE: You do not need to free this object! It will automatically be freed when the
///                TEdit is destroyed. </summary>
function TWinControl_ActivateDropFiles(_WinCtrl: TWinControl; _Callback: TOnFilesDropped): TObject;

///<summary>
/// @returns true, if the shift key is currently pressed </summary>
function IsShiftDown: Boolean;

implementation

uses
  Consts,
  Messages,
{$IFDEF SUPPORTS_UNICODE_STRING}
  AnsiStrings,
{$ENDIF SUPPORTS_UNICODE_STRING}
  ShellApi,
  Types,
  FileCtrl;

type
  TWindowProcHook = class(TComponent)
  private
    FCtrl: TWinControl;
    FOldWindowProc: TWndMethod;
  protected
    procedure WmNcCreate; virtual;
    procedure WmNcDestroy; virtual;
    procedure NewWindowProc(var _Msg: TMessage); virtual;
  public
    constructor Create(_WinControl: TWinControl); reintroduce;
    destructor Destroy; override;
  end;

{ TWindowProcHook }

constructor TWindowProcHook.Create(_WinControl: TWinControl);
begin
  inherited Create(_WinControl);
  FCtrl := _WinControl;

  FOldWindowProc := FCtrl.WindowProc;
  FCtrl.WindowProc := NewWindowProc;
end;

destructor TWindowProcHook.Destroy;
begin
  if Assigned(FCtrl) and Assigned(FOldWindowProc) then begin
    FCtrl.WindowProc := FOldWindowProc;
  end;
  inherited;
end;

procedure TWindowProcHook.WmNcCreate;
begin
  // do nothing
end;

procedure TWindowProcHook.WmNcDestroy;
begin
  // do nothing
end;

procedure TWindowProcHook.NewWindowProc(var _Msg: TMessage);
begin
  if _Msg.Msg = WM_NCCREATE then
    WmNcCreate
  else if _Msg.Msg = WM_NCDESTROY then
    WmNcDestroy;
  // call original WindowProc method to handle all other messages
  FOldWindowProc(_Msg);
end;

type
  TDropFilesActivator = class(TWindowProcHook)
  private
    FCallback: TOnFilesDropped;
    procedure WmDropFiles(var _Msg: TMessage);
    procedure doCallback(_st: TStrings);
  protected
    procedure WmNcCreate; override;
    procedure WmNcDestroy; override;
    procedure NewWindowProc(var _Msg: TMessage); override;
  public
    constructor Create(_WinControl: TWinControl; _Callback: TOnFilesDropped);
    destructor Destroy; override;
  end;

function IsShiftDown: Boolean;
var
  State: TKeyboardState;
begin
  GetKeyboardState(State);
  Result := ((State[vk_Shift] and 128) <> 0);
end;

{ TDropFilesActivator }

constructor TDropFilesActivator.Create(_WinControl: TWinControl; _Callback: TOnFilesDropped);
begin
  inherited Create(_WinControl);
  FCallback := _Callback;
  DragAcceptFiles(FCtrl.Handle, True);
end;

destructor TDropFilesActivator.Destroy;
begin
  if Assigned(FCtrl) and (FCtrl.HandleAllocated) then
    DragAcceptFiles(FCtrl.Handle, False);
  inherited;
end;

procedure TDropFilesActivator.doCallback(_st: TStrings);
begin
  if Assigned(FCallback) then
    FCallback(FCtrl, _st);
end;

procedure TDropFilesActivator.WmNcCreate;
begin
  inherited;
  DragAcceptFiles(FCtrl.Handle, True);
end;

procedure TDropFilesActivator.WmNcDestroy;
begin
  inherited;
  DragAcceptFiles(FCtrl.Handle, False);
end;

procedure TDropFilesActivator.NewWindowProc(var _Msg: TMessage);
begin
  if _Msg.Msg = WM_DROPFILES then
    WmDropFiles(_Msg);

  inherited NewWindowProc(_Msg);
end;

procedure TDropFilesActivator.WmDropFiles(var _Msg: TMessage);
var
  arr: array[0..255] of Char;
  fn: string;
  i: Integer;
  sl: TStringList;
  cnt: Cardinal;
begin
  sl := TStringList.Create;
  try
    sl.Delimiter := ';';
    cnt := DragQueryFile(_Msg.wParam, $FFFFFFFF, nil, 255);
    for i := 0 to cnt - 1 do begin
      DragQueryFile(_Msg.wParam, i, PChar(@arr), SizeOf(arr));
      fn := PChar(@arr);
      sl.Add(fn);
    end;
    DragFinish(_Msg.wParam);
    doCallback(sl);
  finally
    FreeAndNil(sl);
  end;
end;

function TForm_ActivateDropFiles(_WinCtrl: TWinControl; _Callback: TOnFilesDropped): TObject;
begin
  Result := TWinControl_ActivateDropFiles(_WinCtrl, _Callback);
end;

function TWinControl_ActivateDropFiles(_WinCtrl: TWinControl; _Callback: TOnFilesDropped): TObject;
begin
  Result := TDropFilesActivator.Create(_WinCtrl, _Callback);
end;

const
  // constants and descriptions from MSDN
  // http://msdn.microsoft.com/en-us/library/windows/desktop/bb759862(v=vs.85).aspx

  // Ignore the registry default and force the AutoAppend feature off.
  // This flag must be used in combination with one or more of the
  // SHACF_FILESYS* or SHACF_URL* flags.
  SHACF_AUTOAPPEND_FORCE_OFF = $80000000;

  // Ignore the registry value and force the AutoAppend feature on. The completed string will be
  // displayed in the edit box with the added characters highlighted.
  // This flag must be used in combination with one or more of the
  // SHACF_FILESYS* or SHACF_URL* flags.
  SHACF_AUTOAPPEND_FORCE_ON = $40000000;

  // Ignore the registry default and force the AutoSuggest feature off.
  // This flag must be used in combination with one or more of the
  // SHACF_FILESYS* or SHACF_URL* flags.
  SHACF_AUTOSUGGEST_FORCE_OFF = $20000000;

  // Ignore the registry value and force the AutoSuggest feature on.
  // A selection of possible completed strings will be displayed as a
  // drop-down list, below the edit box. This flag must be used in
  // combination with one or more of the
  // SHACF_FILESYS* or SHACF_URL* flags.
  SHACF_AUTOSUGGEST_FORCE_ON = $10000000;

  // The default setting, equivalent to
  // SHACF_FILESYSTEM | SHACF_URLALL.
  // SHACF_DEFAULT cannot be combined with any other flags.
  SHACF_DEFAULT = $00000000;

  // Include the file system only.
  SHACF_FILESYS_ONLY = $00000010;

  // Include the file system and directories, UNC servers, and UNC server shares.
  SHACF_FILESYS_DIRS = $00000020;

  // Include the file system and the rest of the Shell (Desktop, Computer, and Control Panel, for example).
  SHACF_FILESYSTEM = $00000001;

  // Include the URLs in the user's History list.
  SHACF_URLHISTORY = $00000002;

  // Include the URLs in the user's Recently Used list.
  SHACF_URLMRU = $00000004;

  // Include the URLs in the users History and Recently Used lists. Equivalent to
  // SHACF_URLHISTORY | SHACF_URLMRU.
  SHACF_URLALL = SHACF_URLHISTORY or SHACF_URLMRU;

  // Allow the user to select from the autosuggest list by pressing the TAB key.
  // If this flag is not set, pressing the TAB key will shift focus to the next
  // control and close the autosuggest list.
  // If SHACF_USETAB is set, pressing the TAB key will select the first item
  // in the list. Pressing TAB again will select the next item in the list,
  // and so on. When the user reaches the end of the list, the next TAB key
  // press will cycle the focus back to the edit control.
  // This flag must be used in combination with one or more of the
  // SHACF_FILESYS* or SHACF_URL*
  // flags
  SHACF_USETAB = $00000008;

  SHACF_VIRTUAL_NAMESPACE = $00000040;

function SHAutoComplete(hwndEdit: HWnd; dwFlags: DWORD): HResult; stdcall; external 'Shlwapi.dll';

function TEdit_SetAutocomplete(_ed: TCustomEdit; _Source: TAutoCompleteSourceEnumSet = [acsFileSystem];
  _Type: TAutoCompleteTypeEnumSet = []): Boolean;
var
  Options: DWORD;
  Res: HResult;
begin
  Options := 0;
  if acsFileSystem in _Source then
    Options := Options or SHACF_FILESYSTEM;
  if acsUrlHistory in _Source then
    Options := Options or SHACF_URLHISTORY;
  if acsUrlMru in _Source then
    Options := Options or SHACF_URLMRU;
  if actSuggest in _Type then
    Options := Options or SHACF_AUTOSUGGEST_FORCE_ON;
  if actAppend in _Type then
    Options := Options or SHACF_AUTOAPPEND_FORCE_ON;

  Res := SHAutoComplete(_ed.Handle, Options);
  Result := (Res = S_OK);
end;

type
  TAutoCompleteActivator = class(TWindowProcHook)
  private
    FSource: TAutoCompleteSourceEnumSet;
    FType: TAutoCompleteTypeEnumSet;
  protected
    procedure WmNcCreate; override;
    procedure SetAutoComplete;
    procedure NewWindowProc(var _Msg: TMessage); override;
  public
    constructor Create(_ed: TCustomEdit; _Source: TAutoCompleteSourceEnumSet = [acsFileSystem];
      _Type: TAutoCompleteTypeEnumSet = []);
  end;

{ TAutoCompleteActivator }

constructor TAutoCompleteActivator.Create(_ed: TCustomEdit;
  _Source: TAutoCompleteSourceEnumSet = [acsFileSystem]; _Type: TAutoCompleteTypeEnumSet = []);
begin
  inherited Create(_ed);
  FSource := _Source;
  FType := _Type;
  SetAutoComplete;
end;

procedure TAutoCompleteActivator.WmNcCreate;
begin
  inherited;
  SetAutoComplete;
end;

procedure TAutoCompleteActivator.SetAutoComplete;
begin
  TEdit_SetAutocomplete(FCtrl as TCustomEdit, FSource, FType);
end;

// This and the calling function IsAutoSuggstionDropdownVisible are taken from mghie's answer on
// http://stackoverflow.com/a/9228954/49925

function EnumThreadWindowsProc(AWnd: HWnd; AParam: LParam): BOOL; stdcall;
var
  WndClassName: string;
  FoundAndVisiblePtr: PInteger;
begin
  SetLength(WndClassName, 1024);
  GetClassName(AWnd, PChar(WndClassName), Length(WndClassName));
  WndClassName := PChar(WndClassName);
  if WndClassName = 'Auto-Suggest Dropdown' then begin // do not translate
    FoundAndVisiblePtr := PInteger(AParam);
    FoundAndVisiblePtr^ := Ord(IsWindowVisible(AWnd));
    Result := False;
  end else
    Result := True;
end;

function IsAutoSuggestDropdownVisible: Boolean;
var
  FoundAndVisible: Integer;
begin
  FoundAndVisible := 0;
  EnumThreadWindows(GetCurrentThreadId, @EnumThreadWindowsProc,
    LParam(@FoundAndVisible));
  Result := FoundAndVisible > 0;
end;

procedure TAutoCompleteActivator.NewWindowProc(var _Msg: TMessage);
begin
  if (_Msg.Msg = CM_WANTSPECIALKEY) then begin
    if (_Msg.wParam = VK_RETURN) or (_Msg.wParam = VK_ESCAPE) then begin
      if IsAutoSuggestDropdownVisible then begin
        _Msg.Result := 1;
        Exit; //==>
      end;
    end;
  end;
  inherited NewWindowProc(_Msg);
end;

function TEdit_AutoComplete(_ed: TCustomEdit; _Source: TAutoCompleteSourceEnumSet = [acsFileSystem];
  _Type: TAutoCompleteTypeEnumSet = []): TObject;
begin
  Result := TAutoCompleteActivator.Create(_ed, _Source, _Type);
end;

end.
