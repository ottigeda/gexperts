unit GX_GrepOptions;

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
  TfmGrepOptions = class(TfmBaseForm)
    btnOK: TButton;
    btnCancel: TButton;
    chkUseCurrentIdent: TCheckBox;
    grp_StandAlone: TGroupBox;
    grp_ExternalEditor: TGroupBox;
    ed_ExternalEditorExe: TEdit;
    l_ExternalEditorExe: TLabel;
    l_ExternalEditorParameters: TLabel;
    b_ExternalEditorExe: TButton;
    ed_ExternalEditorParameters: TEdit;
    b_ExternalEditorParameters: TButton;
    pm_Parameters: TPopupMenu;
    mi_File: TMenuItem;
    mi_Line: TMenuItem;
    mi_Column: TMenuItem;
    grp_WindowsExplorer: TGroupBox;
    chk_ExplorerMenuBackground: TCheckBox;
    chk_ExplorerMenuFolder: TCheckBox;
    procedure b_ExternalEditorExeClick(Sender: TObject);
    procedure mi_FileClick(Sender: TObject);
    procedure mi_LineClick(Sender: TObject);
    procedure mi_ColumnClick(Sender: TObject);
  private
    procedure HandleDropFiles(_Sender: TObject; _Files: TStrings);
    procedure SetData(_UseCurrentIdent: Boolean;
      const _Editor, _Params: string;
      _AddToBackground, _AddToFolders: Boolean);
    procedure GetData(out _UseCurrentIdent: Boolean;
      out _Editor, _Params: string;
      out _AddToBackground, _AddToFolders: Boolean);
    class procedure ApplyExplorerIntegration(_AddToBackground, _AddToFolders: Boolean);
    class procedure SetExplorerIntegration(const _BaseKey: string; _IsEnabled: boolean);
  protected
{$IFDEF GX_IDE_IS_HIDPI_AWARE}
    procedure ArrangeControls; override;
{$ENDIF}
  public
    class function Execute(_Owner: TWinControl;
      var _UseCurrentIdent: Boolean;
      var _Editor, _Params: string;
      var _AddToBackground, _AddToFolders: Boolean): Boolean;
    constructor Create(_Owner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  ComCtrls, Types, Registry,
  u_dzVclUtils, u_dzOsUtils,
  GX_GenericUtils, GX_IdeUtils, GX_GExperts, GX_ActionBroker, GX_OtaUtils;

{ TfmGrepOptions }

class function TfmGrepOptions.Execute(_Owner: TWinControl;
  var _UseCurrentIdent: Boolean;
  var _Editor, _Params: string;
  var _AddToBackground, _AddToFolders: Boolean): Boolean;
var
  frm: TfmGrepOptions;
begin
  frm := TfmGrepOptions.Create(_Owner);
  try
    frm.SetData(_UseCurrentIdent,_Editor, _Params, _AddToBackground, _AddToFolders);
    Result := (frm.ShowModal = mrOk);
    if Result then begin
      frm.GetData(_UseCurrentIdent,_Editor, _Params, _AddToBackground, _AddToFolders);
      ApplyExplorerIntegration(_AddToBackground, _AddToFolders);
    end;
  finally
    FreeAndNil(frm);
  end;
end;

procedure TfmGrepOptions.b_ExternalEditorExeClick(Sender: TObject);
var
  fn: string;
begin
  fn := ed_ExternalEditorExe.Text;
  if ShowOpenDialog('Select External Editor', 'exe', fn) then
    ed_ExternalEditorExe.Text := fn;
end;

constructor TfmGrepOptions.Create(_Owner: TComponent);
begin
  inherited;

  TWinControl_ActivateDropFiles(Self, HandleDropFiles);

  TButton_AddDropdownMenu(b_ExternalEditorParameters, pm_Parameters);

  InitDpiScaler;
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

procedure TfmGrepOptions.HandleDropFiles(_Sender: TObject; _Files: TStrings);
begin
  if _Files.Count > 0 then
    ed_ExternalEditorExe.Text := _Files[0];
end;

procedure TfmGrepOptions.mi_ColumnClick(Sender: TObject);
begin
  ed_ExternalEditorParameters.Text := ed_ExternalEditorParameters.Text + '{COLUMN}';
end;

procedure TfmGrepOptions.mi_FileClick(Sender: TObject);
begin
  ed_ExternalEditorParameters.Text := ed_ExternalEditorParameters.Text + '{FILE}';
end;

procedure TfmGrepOptions.mi_LineClick(Sender: TObject);
begin
  ed_ExternalEditorParameters.Text := ed_ExternalEditorParameters.Text + '{LINE}';
end;

procedure TfmGrepOptions.GetData(out _UseCurrentIdent: Boolean; out _Editor, _Params: string;
  out _AddToBackground, _AddToFolders: Boolean);
begin
  _UseCurrentIdent := chkUseCurrentIdent.Checked;

  _Editor := ed_ExternalEditorExe.Text;
  _Params := ed_ExternalEditorParameters.Text;

  _AddToBackground := chk_ExplorerMenuBackground.Checked;
  _AddToFolders := chk_ExplorerMenuFolder.Checked;
end;

procedure TfmGrepOptions.SetData(_UseCurrentIdent: Boolean; const _Editor, _Params: string;
  _AddToBackground, _AddToFolders: Boolean);
begin
  chkUseCurrentIdent.Checked := _UseCurrentIdent;

  ed_ExternalEditorExe.Text := _Editor;
  if _Params = '' then
    ed_ExternalEditorParameters.Text := '{FILE}'
  else
    ed_ExternalEditorParameters.Text := _Params;

  chk_ExplorerMenuBackground.Checked := _AddToBackground;
  chk_ExplorerMenuFolder.Checked := _AddToFolders;
end;

class procedure TfmGrepOptions.SetExplorerIntegration(const _BaseKey: string; _IsEnabled: boolean);
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

class procedure TfmGrepOptions.ApplyExplorerIntegration(_AddToBackground, _AddToFolders: Boolean);
begin
  SetExplorerIntegration('SOFTWARE\Classes\Directory\Background',_AddToBackground);
  SetExplorerIntegration('SOFTWARE\Classes\Directory',_AddToFolders);
end;

end.
