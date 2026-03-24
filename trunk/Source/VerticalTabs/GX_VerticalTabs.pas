unit GX_VerticalTabs;

{$I GX_CondDefine.inc}

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, Menus, ActnList, ComCtrls,
  StdCtrls, ExtCtrls, ToolWin, ToolsAPI,
  GX_IdeDock, GX_Experts, GX_OtaUtils, GX_ConfigurationInfo, GX_SharedImages;

type
  TfmVerticalTabs = class(TfmIdeDockForm)
    lbFiles: TListBox;
    ToolBar: TToolBar;
    tbnRefresh: TToolButton;
    tbnSep1: TToolButton;
    tbnSortAlpha: TToolButton;
    StatusBar: TStatusBar;
    Popup: TPopupMenu;
    mitGotoFile: TMenuItem;
    mitSep1: TMenuItem;
    mitCloseFile: TMenuItem;
    mitCloseAllOther: TMenuItem;
    mitSep2: TMenuItem;
    mitCopyPath: TMenuItem;
    mitOpenFolder: TMenuItem;
    Actions: TActionList;
    actRefresh: TAction;
    actSortAlpha: TAction;
    actGotoFile: TAction;
    actCloseFile: TAction;
    actCloseAllOther: TAction;
    actCopyPath: TAction;
    actOpenFolder: TAction;
    procedure lbFilesClick(Sender: TObject);
    procedure lbFilesMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure actRefreshExecute(Sender: TObject);
    procedure actSortAlphaExecute(Sender: TObject);
    procedure actGotoFileExecute(Sender: TObject);
    procedure actCloseFileExecute(Sender: TObject);
    procedure actCloseAllOtherExecute(Sender: TObject);
    procedure actCopyPathExecute(Sender: TObject);
    procedure actOpenFolderExecute(Sender: TObject);
  private
    FFilePaths: TStringList;
    FIdeNotifier: TBaseIdeNotifier;
    FSortAlphabetical: Boolean;
    FUpdatingSelection: Boolean;
{$IFDEF GX_VER160_up}
    FEditNotifierIndex: Integer;
{$ELSE}
    FTimer: TTimer;
{$ENDIF}
    function GetSelectedFilePath: string;
{$IFNDEF GX_VER160_up}
    procedure TimerTick(Sender: TObject);
{$ENDIF}
    procedure RefreshFileList;
    procedure AddFile(const _FileName: string);
    procedure RemoveFile(const _FileName: string);
    procedure UpdateSelection(const _FileName: string);
    procedure UpdateStatusBar;
    procedure SortFileList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TVerticalTabsExpert = class(TGX_Expert)
  protected
    procedure SetActive(New: Boolean); override;
    procedure InternalLoadSettings(_Settings: IExpertSettings); override;
    procedure InternalSaveSettings(_Settings: IExpertSettings); override;
  public
    function GetActionCaption: string; override;
    class function GetName: string; override;
    procedure Execute(Sender: TObject); override;
    function HasConfigOptions: Boolean; override;
  end;

var
  fmVerticalTabs: TfmVerticalTabs;
  VerticalTabsExpert: TVerticalTabsExpert;

implementation

{$R *.dfm}

uses
  Clipbrd, ShellAPI,
  u_dzVclUtils,
  GX_GenericUtils,
{$IFDEF GX_VER160_up}
  DockForm,
  GX_NTAEditServiceNotifier,
{$ENDIF}
  GX_GExperts;

type
  TVerticalTabsIdeNotifier = class(TBaseIdeNotifier)
  private
    FForm: TfmVerticalTabs;
  public
    constructor Create(_Form: TfmVerticalTabs);
    procedure FileNotification(NotifyCode: TOTAFileNotification;
      const FileName: string; var Cancel: Boolean); override;
  end;

{$IFDEF GX_VER160_up}
type
  TVerticalTabsEditNotifier = class(TGxNTAEditServiceNotifier, INTAEditServicesNotifier)
  private
    FForm: TfmVerticalTabs;
  protected
    procedure EditorViewActivated(const EditWindow: INTAEditWindow;
      const EditView: IOTAEditView); override;
  public
    constructor Create(_Form: TfmVerticalTabs);
  end;
{$ENDIF}

{ TVerticalTabsIdeNotifier }

constructor TVerticalTabsIdeNotifier.Create(_Form: TfmVerticalTabs);
begin
  inherited Create;
  FForm := _Form;
end;

procedure TVerticalTabsIdeNotifier.FileNotification(NotifyCode: TOTAFileNotification;
  const FileName: string; var Cancel: Boolean);
begin
  if not Assigned(FForm) then
    Exit;
  case NotifyCode of
    ofnFileOpened:
      FForm.AddFile(FileName);
    ofnFileClosing:
      FForm.RemoveFile(FileName);
  end;
end;

{$IFDEF GX_VER160_up}

{ TVerticalTabsEditNotifier }

constructor TVerticalTabsEditNotifier.Create(_Form: TfmVerticalTabs);
begin
  FForm := _Form;
  inherited Create;
end;

procedure TVerticalTabsEditNotifier.EditorViewActivated(
  const EditWindow: INTAEditWindow; const EditView: IOTAEditView);
var
  fn: string;
begin
  if not Assigned(FForm) then
    Exit;
  if not Assigned(EditView) then
    Exit;
  try
    fn := EditView.Buffer.FileName;
    if fn <> '' then
      FForm.UpdateSelection(fn);
  except
    // Ignore OTA exceptions during IDE shutdown
  end;
end;
{$ENDIF}

{ TfmVerticalTabs }

constructor TfmVerticalTabs.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetToolbarGradient(ToolBar);

  if Assigned(VerticalTabsExpert) then
    VerticalTabsExpert.SetFormIcon(Self);

  FFilePaths := TStringList.Create;
  lbFiles.ShowHint := True;

  FIdeNotifier := TVerticalTabsIdeNotifier.Create(Self);
  FIdeNotifier.AddNotifierToIDE;

{$IFDEF GX_VER160_up}
  FEditNotifierIndex := Ota.GetEditorServices.AddNotifier(
    TVerticalTabsEditNotifier.Create(Self));
{$ELSE}
  FTimer := TTimer.Create(Self);
  FTimer.Interval := 500;
  FTimer.OnTimer := TimerTick;
  FTimer.Enabled := True;
{$ENDIF}

  RefreshFileList;
end;

destructor TfmVerticalTabs.Destroy;
begin
{$IFDEF GX_VER160_up}
  if FEditNotifierIndex >= 0 then begin
    Ota.GetEditorServices.RemoveNotifier(FEditNotifierIndex);
    FEditNotifierIndex := -1;
  end;
{$ELSE}
  FreeAndNil(FTimer);
{$ENDIF}

  if Assigned(FIdeNotifier) then begin
    FIdeNotifier.RemoveNotifierFromIDE;
    FIdeNotifier := nil;
  end;

  FreeAndNil(FFilePaths);

  inherited Destroy;

  fmVerticalTabs := nil;
end;

function TfmVerticalTabs.GetSelectedFilePath: string;
var
  Idx: Integer;
begin
  Result := '';
  Idx := lbFiles.ItemIndex;
  if (Idx >= 0) and (Idx < FFilePaths.Count) then
    Result := FFilePaths[Idx];
end;

procedure TfmVerticalTabs.RefreshFileList;
var
  ModuleServices: IOTAModuleServices;
  Module: IOTAModule;
  SourceEditor: IOTASourceEditor;
  i: Integer;
  fn: string;
  SelectedFile: string;
begin
  SelectedFile := GetSelectedFilePath;

  lbFiles.Items.BeginUpdate;
  try
    lbFiles.Items.Clear;
    FFilePaths.Clear;

    ModuleServices := BorlandIDEServices as IOTAModuleServices;
    if not Assigned(ModuleServices) then
      Exit;

    for i := 0 to ModuleServices.ModuleCount - 1 do begin
      Module := ModuleServices.Modules[i];
      if not Assigned(Module) then
        Continue;
      SourceEditor := Ota.GetSourceEditorFromModule(Module);
      if not Assigned(SourceEditor) then
        Continue;
      fn := SourceEditor.FileName;
      if fn = '' then
        Continue;
      if FFilePaths.IndexOf(fn) >= 0 then
        Continue;
      FFilePaths.Add(fn);
      lbFiles.Items.Add(ExtractFileName(fn));
    end;

    SortFileList;
  finally
    lbFiles.Items.EndUpdate;
  end;

  UpdateSelection(SelectedFile);
  UpdateStatusBar;
end;

procedure TfmVerticalTabs.AddFile(const _FileName: string);
var
  SourceEditor: IOTASourceEditor;
begin
  if _FileName = '' then
    Exit;

  // Only add files that have a source editor
  if not Ota.TryGetSourceEditor(_FileName, SourceEditor) then
    Exit;

  if FFilePaths.IndexOf(_FileName) >= 0 then
    Exit;

  FFilePaths.Add(_FileName);
  lbFiles.Items.Add(ExtractFileName(_FileName));

  if FSortAlphabetical then
    SortFileList;

  UpdateStatusBar;
end;

procedure TfmVerticalTabs.RemoveFile(const _FileName: string);
var
  Idx: Integer;
begin
  Idx := FFilePaths.IndexOf(_FileName);
  if Idx < 0 then
    Exit;

  FFilePaths.Delete(Idx);
  lbFiles.Items.Delete(Idx);

  UpdateStatusBar;
end;

procedure TfmVerticalTabs.UpdateSelection(const _FileName: string);
var
  i: Integer;
begin
  if FUpdatingSelection then
    Exit;
  FUpdatingSelection := True;
  try
    for i := 0 to FFilePaths.Count - 1 do begin
      if SameFileName(FFilePaths[i], _FileName) then begin
        if lbFiles.ItemIndex <> i then
          lbFiles.ItemIndex := i;
        Exit;
      end;
    end;
  finally
    FUpdatingSelection := False;
  end;
end;

procedure TfmVerticalTabs.UpdateStatusBar;
begin
  StatusBar.SimpleText := Format(' %d files', [FFilePaths.Count]);
end;

procedure TfmVerticalTabs.SortFileList;
var
  i, j: Integer;
  TempName: string;
  TempPath: string;
  SelectedFile: string;
begin
  if not FSortAlphabetical then
    Exit;

  SelectedFile := GetSelectedFilePath;

  // Simple insertion sort by display name
  for i := 1 to lbFiles.Items.Count - 1 do begin
    j := i;
    while (j > 0) and (AnsiCompareText(lbFiles.Items[j - 1], lbFiles.Items[j]) > 0) do begin
      TempName := lbFiles.Items[j];
      lbFiles.Items[j] := lbFiles.Items[j - 1];
      lbFiles.Items[j - 1] := TempName;

      TempPath := FFilePaths[j];
      FFilePaths[j] := FFilePaths[j - 1];
      FFilePaths[j - 1] := TempPath;

      Dec(j);
    end;
  end;

  UpdateSelection(SelectedFile);
end;

procedure TfmVerticalTabs.lbFilesClick(Sender: TObject);
var
  fn: string;
begin
  if FUpdatingSelection then
    Exit;
  fn := GetSelectedFilePath;
  if fn <> '' then
    Ota.MakeSourceVisible(fn);
end;

procedure TfmVerticalTabs.lbFilesMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  Idx: Integer;
begin
  Idx := lbFiles.ItemAtPos(Point(X, Y), True);
  if (Idx >= 0) and (Idx < FFilePaths.Count) then
    lbFiles.Hint := FFilePaths[Idx]
  else
    lbFiles.Hint := '';
end;

procedure TfmVerticalTabs.actRefreshExecute(Sender: TObject);
begin
  RefreshFileList;
end;

procedure TfmVerticalTabs.actSortAlphaExecute(Sender: TObject);
begin
  FSortAlphabetical := not FSortAlphabetical;
  tbnSortAlpha.Down := FSortAlphabetical;
  RefreshFileList;
end;

procedure TfmVerticalTabs.actGotoFileExecute(Sender: TObject);
var
  fn: string;
begin
  fn := GetSelectedFilePath;
  if fn <> '' then
    Ota.MakeSourceVisible(fn);
end;

procedure TfmVerticalTabs.actCloseFileExecute(Sender: TObject);
var
  fn: string;
  Module: IOTAModule;
begin
  fn := GetSelectedFilePath;
  if fn = '' then
    Exit;
  Module := Ota.GetModule(fn);
  if Assigned(Module) then
    Module.CloseModule(True);
end;

procedure TfmVerticalTabs.actCloseAllOtherExecute(Sender: TObject);
var
  SelectedFile: string;
  Module: IOTAModule;
  i: Integer;
  FilesToClose: TStringList;
begin
  SelectedFile := GetSelectedFilePath;
  if SelectedFile = '' then
    Exit;

  FilesToClose := TStringList.Create;
  try
    // Collect files to close first, then close them
    for i := 0 to FFilePaths.Count - 1 do begin
      if not SameFileName(FFilePaths[i], SelectedFile) then
        FilesToClose.Add(FFilePaths[i]);
    end;

    for i := 0 to FilesToClose.Count - 1 do begin
      Module := Ota.GetModule(FilesToClose[i]);
      if Assigned(Module) then
        Module.CloseModule(True);
    end;
  finally
    FreeAndNil(FilesToClose);
  end;
end;

procedure TfmVerticalTabs.actCopyPathExecute(Sender: TObject);
var
  fn: string;
begin
  fn := GetSelectedFilePath;
  if fn <> '' then
    Clipboard.AsText := fn;
end;

procedure TfmVerticalTabs.actOpenFolderExecute(Sender: TObject);
var
  fn: string;
begin
  fn := GetSelectedFilePath;
  if fn <> '' then
    ShellExecute(0, 'open', 'explorer.exe', PChar('/select,' + fn), nil, SW_SHOWNORMAL);
end;

{$IFNDEF GX_VER160_up}
procedure TfmVerticalTabs.TimerTick(Sender: TObject);
var
  fn: string;
begin
  fn := Ota.GetCurrentSourceFile;
  if fn <> '' then
    UpdateSelection(fn);
end;
{$ENDIF}

{ TVerticalTabsExpert }

function TVerticalTabsExpert.GetActionCaption: string;
begin
  Result := '&Vertical Tabs';
end;

class function TVerticalTabsExpert.GetName: string;
begin
  Result := 'VerticalTabs';
end;

function TVerticalTabsExpert.HasConfigOptions: Boolean;
begin
  Result := False;
end;

procedure TVerticalTabsExpert.Execute(Sender: TObject);
begin
  if fmVerticalTabs = nil then
    fmVerticalTabs := TfmVerticalTabs.Create(nil);
  IdeDockManager.ShowForm(fmVerticalTabs);
  IncCallCount;
end;

procedure TVerticalTabsExpert.SetActive(New: Boolean);
begin
  if New <> Active then begin
    inherited SetActive(New);
    if not New then
      FreeAndNil(fmVerticalTabs);
  end;
end;

procedure TVerticalTabsExpert.InternalLoadSettings(_Settings: IExpertSettings);
begin
  inherited;
  if Assigned(fmVerticalTabs) then begin
    fmVerticalTabs.FSortAlphabetical := _Settings.ReadBool('SortAlphabetical', False);
    fmVerticalTabs.tbnSortAlpha.Down := fmVerticalTabs.FSortAlphabetical;
  end;
end;

procedure TVerticalTabsExpert.InternalSaveSettings(_Settings: IExpertSettings);
begin
  inherited;
  if Assigned(fmVerticalTabs) then
    _Settings.WriteBool('SortAlphabetical', fmVerticalTabs.FSortAlphabetical);
end;

initialization
  RegisterGX_Expert(TVerticalTabsExpert);
end.
