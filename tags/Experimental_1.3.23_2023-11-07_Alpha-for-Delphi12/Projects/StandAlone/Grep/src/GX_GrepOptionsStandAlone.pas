unit GX_GrepOptionsStandAlone;

{$I GX_CondDefine.inc}

interface

uses
  Windows,
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

type
  TfmGrepGrepOptionsStandAlone = class(TfmBaseForm)
    btnOK: TButton;
    btnCancel: TButton;
    pm_Parameters: TPopupMenu;
    mi_File: TMenuItem;
    mi_Line: TMenuItem;
    mi_Column: TMenuItem;
    grp_ExternalEditor: TGroupBox;
    l_ExternalEditorParameters: TLabel;
    l_ExternalEditorExe: TLabel;
    b_ExternalEditorParameters: TButton;
    ed_ExternalEditorParameters: TEdit;
    ed_ExternalEditorExe: TEdit;
    b_ExternalEditorExe: TButton;
    grp_WindowsExplorer: TGroupBox;
    chk_ExplorerMenuBackground: TCheckBox;
    chk_ExplorerMenuFolder: TCheckBox;
    grp_Presets: TGroupBox;
    b_Notepad: TButton;
    b_NotepadPP: TButton;
    b_Notepad2: TButton;
    b_Associated: TButton;
    b_ProgrammersNotepad: TButton;
    procedure b_ExternalEditorExeClick(Sender: TObject);
    procedure mi_FileClick(Sender: TObject);
    procedure mi_LineClick(Sender: TObject);
    procedure mi_ColumnClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure b_NotepadPPClick(Sender: TObject);
    procedure b_Notepad2Click(Sender: TObject);
    procedure b_NotepadClick(Sender: TObject);
    procedure b_ProgrammersNotepadClick(Sender: TObject);
  private
    procedure HandleDropFiles(_Sender: TObject; _Files: TStrings);
    procedure SetData(const _Editor, _Params: string);
    procedure GetData(out _Editor, _Params: string);
    procedure GetExplorerIntegrationOptions(out _AddToBackground, _AddToFolders: Boolean);
    procedure ApplyExplorerIntegration(_AddToBackground, _AddToFolders: Boolean);
    procedure SetExplorerIntegration(const _BaseKey: string; _IsEnabled: Boolean);
    procedure GetExplorerIntegration(const _BaseKey: string; out _IsEnabled: Boolean);
    function FindNotepadPP(out _ExePath: string): Boolean;
    function FindNotepad2(out _ExePath: string): Boolean;
  protected
{$IFDEF GX_IDE_IS_HIDPI_AWARE}
    procedure ArrangeControls; override;
{$ENDIF}
  public
    class function Execute(_Owner: TWinControl; var _Editor, _Params: string): Boolean;
    constructor Create(_Owner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  ComCtrls, Types, Registry,
  u_dzVclUtils, u_dzOsUtils, u_dzClassUtils, u_dzFileUtils,
  GX_GenericUtils, GX_IdeUtils, GX_GExperts, GX_ActionBroker, GX_OtaUtils;

{ TfmGrepGrepOptionsStandAlone }

class function TfmGrepGrepOptionsStandAlone.Execute(_Owner: TWinControl;
  var _Editor, _Params: string): Boolean;
var
  frm: TfmGrepGrepOptionsStandAlone;
begin
  frm := TfmGrepGrepOptionsStandAlone.Create(_Owner);
  try
    frm.SetData(_Editor, _Params);
    Result := (frm.ShowModal = mrOk);
    if Result then begin
      frm.GetData(_Editor, _Params);
    end;
  finally
    FreeAndNil(frm);
  end;
end;

function TfmGrepGrepOptionsStandAlone.FindNotepadPP(out _ExePath: string): Boolean;
begin
  Result := TRegistry_TryReadString('SOFTWARE\Microsoft\Windows\CurrentVersion\App Paths\notepad++.exe',
    '', _ExePath, HKEY_LOCAL_MACHINE, True)
end;

function TfmGrepGrepOptionsStandAlone.FindNotepad2(out _ExePath: string): Boolean;
begin
  Result := TRegistry_TryReadString('SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\Notepad2',
    'InstallLocation', _ExePath, HKEY_LOCAL_MACHINE);
  if Result then
    _ExePath := itpd(_ExePath) + 'Notepad2.exe'
end;


procedure TfmGrepGrepOptionsStandAlone.btnOKClick(Sender: TObject);
begin
  ApplyExplorerIntegration(chk_ExplorerMenuBackground.Checked, chk_ExplorerMenuFolder.Checked);
end;

procedure TfmGrepGrepOptionsStandAlone.b_ExternalEditorExeClick(Sender: TObject);
var
  fn: string;
begin
  fn := ed_ExternalEditorExe.Text;
  if ShowOpenDialog('Select External Editor', 'exe', fn) then
    ed_ExternalEditorExe.Text := fn;
end;

procedure TfmGrepGrepOptionsStandAlone.b_Notepad2Click(Sender: TObject);
var
  Executable: string;
begin
  if not FindNotepad2(Executable) then
    Executable := 'path\to\Notepad2.exe';
  ed_ExternalEditorExe.Text := Executable;
  ed_ExternalEditorParameters.Text := '/g{LINE},{COLUMN} {FILE}';
end;

procedure TfmGrepGrepOptionsStandAlone.b_NotepadClick(Sender: TObject);
begin
  ed_ExternalEditorExe.text := 'c:\windows\notepad.exe';
  ed_ExternalEditorParameters.Text := '{FILE}';
end;

procedure TfmGrepGrepOptionsStandAlone.b_NotepadPPClick(Sender: TObject);
var
  Executable: string;
begin
  if not FindNotepadPP(Executable) then
    Executable := 'path\to\Notepad++.exe';
  ed_ExternalEditorExe.Text := Executable;
  ed_ExternalEditorParameters.Text := '-n{LINE} -c{COLUMN} {FILE}';
end;

procedure TfmGrepGrepOptionsStandAlone.b_ProgrammersNotepadClick(Sender: TObject);
begin
  ed_ExternalEditorExe.Text := 'C:\Program Files (x86)\Programmer''s Notepad\pn.exe';
  ed_ExternalEditorParameters.Text := '--line {LINE} --col {COLUMN} {FILE}';
end;

constructor TfmGrepGrepOptionsStandAlone.Create(_Owner: TComponent);
var
  AddToBackground: Boolean;
  AddToFolders: Boolean;
begin
  inherited;

  TWinControl_ActivateDropFiles(Self, HandleDropFiles);
  TEdit_ActivateAutoComplete(ed_ExternalEditorExe);

  TButton_AddDropdownMenu(b_ExternalEditorParameters, pm_Parameters);

  InitDpiScaler;

  GetExplorerIntegrationOptions(AddToBackground, AddToFolders);
  chk_ExplorerMenuBackground.Checked := AddToBackground;
  chk_ExplorerMenuFolder.Checked := AddToFolders;
end;

{$IFDEF GX_IDE_IS_HIDPI_AWARE}
procedure TfmGrepOptions.ArrangeControls;
var
  t: Integer;
begin
  t := TEdit_AlignBelowLabel(ed_ExternalEditorExe, l_ExternalEditorExe);
  TButton_AlignVerticallyTo(b_ExternalEditorExe, ed_ExternalEditorExe);
  l_ExternalEditorParameters.Top := t + 8;
  t := TEdit_AlignBelowLabel(ed_ExternalEditorParameters, l_ExternalEditorParameters);
  Inc(t, 8);
  b_ExternalEditorParameters.Top := t;
  grp_ExternalEditor.ClientHeight := t + b_ExternalEditorParameters.Height + 8;
  grp_WindowsExplorer.Top := grp_ExternalEditor.Top + grp_ExternalEditor.Height + 16;
  t := grp_StandAlone.Top + grp_StandAlone.Height + 16;
  btnOK.Top := t;
  btnCancel.Top := t;
end;
{$ENDIF}

procedure TfmGrepGrepOptionsStandAlone.HandleDropFiles(_Sender: TObject; _Files: TStrings);
begin
  if _Files.Count > 0 then
    ed_ExternalEditorExe.Text := _Files[0];
end;

procedure TfmGrepGrepOptionsStandAlone.mi_ColumnClick(Sender: TObject);
begin
  ed_ExternalEditorParameters.Text := ed_ExternalEditorParameters.Text + '{COLUMN}';
end;

procedure TfmGrepGrepOptionsStandAlone.mi_FileClick(Sender: TObject);
begin
  ed_ExternalEditorParameters.Text := ed_ExternalEditorParameters.Text + '{FILE}';
end;

procedure TfmGrepGrepOptionsStandAlone.mi_LineClick(Sender: TObject);
begin
  ed_ExternalEditorParameters.Text := ed_ExternalEditorParameters.Text + '{LINE}';
end;

procedure TfmGrepGrepOptionsStandAlone.GetData(out _Editor, _Params: string);
begin
  _Editor := ed_ExternalEditorExe.Text;
  _Params := ed_ExternalEditorParameters.Text;
end;

procedure TfmGrepGrepOptionsStandAlone.SetData(const _Editor, _Params: string);
begin
  ed_ExternalEditorExe.Text := _Editor;
  if _Params = '' then
    ed_ExternalEditorParameters.Text := '{FILE}'
  else
    ed_ExternalEditorParameters.Text := _Params;
end;

procedure TfmGrepGrepOptionsStandAlone.GetExplorerIntegration(const _BaseKey: string; out _IsEnabled: boolean);
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    _IsEnabled := Reg.KeyExists(_BaseKey + '\Shell\GExperts Grep\command');
  finally
    FreeAndNil(Reg);
  end;
end;

procedure TfmGrepGrepOptionsStandAlone.GetExplorerIntegrationOptions(
  out _AddToBackground, _AddToFolders: Boolean);
begin
  GetExplorerIntegration('SOFTWARE\Classes\Directory\Background', _AddToBackground);
  GetExplorerIntegration('SOFTWARE\Classes\Directory', _AddToFolders);
end;

procedure TfmGrepGrepOptionsStandAlone.SetExplorerIntegration(const _BaseKey: string; _IsEnabled: boolean);
var
  Reg: TRegistry;
  GrepExe: string;
begin
  GrepExe := AddSlash(ExtractFileDir(GetModuleFilename)) + 'GExpertsGrep.exe';

  Reg := TRegistry.Create;
  try
  Reg.RootKey := HKEY_CURRENT_USER;
  if _IsEnabled then begin
      Reg.OpenKey(_BaseKey + '\Shell\GExperts Grep\command', True);
      try
        Reg.WriteString('', '"' + GrepExe + '" "%V"');
      finally
        Reg.CloseKey;
      end;
      Reg.OpenKey(_BaseKey + '\Shell\GExperts Grep', False);
      try
        Reg.WriteString('Icon', '"' + GrepExe + '"');
      finally
        Reg.CloseKey;
      end;
    end else begin
      Reg.DeleteKey(_BaseKey + '\Shell\GExperts Grep');
    end;
  finally
    FreeAndNil(Reg);
  end;
end;

procedure TfmGrepGrepOptionsStandAlone.ApplyExplorerIntegration(_AddToBackground, _AddToFolders: Boolean);
begin
  SetExplorerIntegration('SOFTWARE\Classes\Directory\Background', _AddToBackground);
  SetExplorerIntegration('SOFTWARE\Classes\Directory', _AddToFolders);
end;

end.
