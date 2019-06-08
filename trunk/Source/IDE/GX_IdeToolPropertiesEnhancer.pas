unit GX_IdeToolPropertiesEnhancer;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils,
  Classes,
  IniFiles;

type
  TGxIdeToolPropertiesEnhancer = class
  public
    class function GetEnabled: Boolean; // static;
    class procedure SetEnabled(const Value: Boolean); //static;
  end;

implementation

uses
  Controls,
  StdCtrls,
  Forms,
  Messages,
  Menus,
  GX_IdeFormEnhancer,
  GX_dzVclUtils,
  GX_dzClassUtils,
  GX_IdeDialogEnhancer,
  GX_GenericUtils,
  GX_dzFileUtils;

const
  DelphiToolMenuEntry = 'Delphi Tool Menu Entry';
  DtmeExtension = '.dtme';

type
  TToolsEntry = record
    Title: string;
    Path: string;
    WorkingDir: string;
    Params: string;
  end;

type
  TToolPropertiesEnhancer = class(TIdeDialogEnhancer)
  private
    edTitle: TEdit;
    edProgram: TEdit;
    edWorkingDir: TEdit;
    edParameters: TEdit;
    FDirectory: string;
    procedure HandleFilesDropped(_Sender: TObject; _Files: TStrings);
    procedure b_ExportClick(_Sender: TObject);
    procedure b_ImportClick(_Sender: TObject);
    procedure LoadEntry(const _fn: string; out _Entry: TToolsEntry);
    procedure SaveEntry(const _fn: string; const _Entry: TToolsEntry);
  protected
    function IsDesiredForm(_Form: TCustomForm): Boolean; override;
    procedure EnhanceForm(_Form: TForm); override;
  public
    constructor Create;
  end;

var
  TheToolPropertiesEnhancer: TToolPropertiesEnhancer = nil;

{ TGxIdeToolPropertiesEnhancer }

class function TGxIdeToolPropertiesEnhancer.GetEnabled: Boolean;
begin
  Result := Assigned(TheToolPropertiesEnhancer);
end;

class procedure TGxIdeToolPropertiesEnhancer.SetEnabled(const Value: Boolean);
begin
  if Value then begin
    if not Assigned(TheToolPropertiesEnhancer) then
      TheToolPropertiesEnhancer := TToolPropertiesEnhancer.Create
  end else
    FreeAndNil(TheToolPropertiesEnhancer);
end;

{ TToolPropertiesEnhancer }

constructor TToolPropertiesEnhancer.Create;
begin
  inherited;
  // todo: Initialize this sensibly somehow. Maybe the user's documents directory?
  FDirectory := 'c:\';
end;

procedure TToolPropertiesEnhancer.HandleFilesDropped(_Sender: TObject; _Files: TStrings);
var
  frm: TCustomForm;
  ed: TEdit;
begin
  frm := Screen.ActiveCustomForm;
  if not IsDesiredForm(frm) then
    Exit;
  ed := _Sender as TEdit;
  ed.Text := _Files[0];
  if ed.Name = 'edProgram' then
    if TComponent_FindComponent(frm, 'edWorkingDir', True, TComponent(ed), TEdit) then
      ed.Text := ExtractFileDir(_Files[0]);
end;

function TToolPropertiesEnhancer.IsDesiredForm(_Form: TCustomForm): Boolean;
begin
  Result := _Form.ClassNameIs('TTransEditDlg')
    and SameText(_Form.Name, 'TransEditDlg');
end;

procedure TToolPropertiesEnhancer.EnhanceForm(_Form: TForm);
var
  b_Export: TButton;
  HelpButton: TButton;
  CancelButton: TButton;
  b_Import: TButton;
begin
// Drop files only works in Delphi 6 and 7 while autocomplete works in all versions.
// The "new" IDE apparently does something to TEdits that prevent them to receive WM_DROPFILES
// messages.
// I tried to use this for re-registering the drop files handler but it did not help:
//  FControlChangedHandle := TIDEFormEnhancements.RegisterControlChangeCallback(HandleControlChanged);

  if not TComponent_FindComponent(_Form, 'edTitle', True, TComponent(edTitle), TEdit) then
    Exit; //==>
  if not TComponent_FindComponent(_Form, 'edProgram', True, TComponent(edProgram), TEdit) then
    Exit; //==>
  TWinControl_ActivateDropFiles(edProgram, HandleFilesDropped);
  TEdit_ActivateAutoComplete(edProgram, [acsFileSystem], [actSuggest]);

  if not TComponent_FindComponent(_Form, 'edWorkingDir', True, TComponent(edWorkingDir), TEdit) then
    Exit; //==>
  TWinControl_ActivateDropFiles(edWorkingDir, HandleFilesDropped);
  TEdit_ActivateAutoComplete(edWorkingDir, [acsFileSystem], [actSuggest]);

  if not TComponent_FindComponent(_Form, 'edParameters', True, TComponent(edParameters), TEdit) then
    Exit; //==>
  TWinControl_ActivateDropFiles(edParameters, HandleFilesDropped);
  TEdit_ActivateAutoComplete(edParameters, [acsFileSystem], [actSuggest]);

  CancelButton := _Form.FindComponent('CancelButton') as TButton;
  HelpButton := _Form.FindComponent('HelpButton') as TButton;

  b_Export := TButton.Create(_Form);
  b_Export.Name := 'GxExportButton';
  b_Export.Parent := _Form;
  b_Export.Caption := 'Export ...';
  b_Export.Top := HelpButton.Top + (HelpButton.Top - CancelButton.Top);
  b_Export.Left := HelpButton.Left;
  b_Export.Width := HelpButton.Width;
  b_Export.OnClick := b_ExportClick;

  b_Import := TButton.Create(_Form);
  b_Import.Name := 'GxImportButton';
  b_Import.Parent := _Form;
  b_Import.Caption := 'Import ...';
  b_Import.Top := b_Export.Top + b_Export.Height + 4;
  b_Import.Left := HelpButton.Left;
  b_Import.Width := HelpButton.Width;
  b_Import.OnClick := b_ImportClick;
end;

//procedure TToolPropertiesEnhancer.HandleControlChanged(_Sender: TObject; _Form: TCustomForm; _Control: TWinControl);
//var
//  i: Integer;
//begin
//  if not IsToolPropertiesForm(_Form) then begin
//    FIsAutocompleteEnabled := False;
//    Exit;
//  end;
//  if not (_Control is TEdit) then
//    Exit;
//  for i := _Control.ComponentCount-1 downto 0 do begin
//    if _Control.Components[i].ClassNameIs('TDropFilesActivator') then begin
//      _Control.Components[i].Free;
//      TWinControl_ActivateDropFiles2(_Control, HandleFilesDropped);
//    end;
//  end;
//end;

procedure TToolPropertiesEnhancer.b_ExportClick(_Sender: TObject);
var
  fn: string;
  Entry: TToolsEntry;
begin
  Entry.Title := edTitle.Text;
  Entry.Path := edProgram.Text;
  Entry.WorkingDir := edWorkingDir.Text;
  Entry.Params := edParameters.Text;

  fn := StripHotkey(Entry.Title);
  fn := TFileSystem.MakeValidFilename(fn);
  fn := FDirectory + fn + DtmeExtension;

  if not ShowSaveDialog('Export tools configuration entry', '.dtme', fn,
    DelphiToolMenuEntry + '(*' + DtmeExtension + ')|*' + DtmeExtension) then
    Exit; //==>

  SaveEntry(fn, Entry);
  FDirectory := ExtractFileDir(fn)
end;

procedure TToolPropertiesEnhancer.b_ImportClick(_Sender: TObject);
var
  fn: string;
  Entry: TToolsEntry;
begin
  if not ShowOpenDialog('Select tools configuration to import', '.dtme', fn,
    DelphiToolMenuEntry + '(*' + DtmeExtension + ')|*' + DtmeExtension) then
    Exit; //==>

  LoadEntry(fn, Entry);

  edTitle.Text := Entry.Title;
  edProgram.Text := Entry.Path;
  edWorkingDir.Text := Entry.WorkingDir;
  edParameters.Text := Entry.Params;
end;

procedure TToolPropertiesEnhancer.LoadEntry(const _fn: string; out _Entry: TToolsEntry);
var
  ini: TMemIniFile;
begin
  ini := TMemIniFile.Create(_fn);
  try
    _Entry.Title := ini.ReadString(DelphiToolMenuEntry, 'Title', '');
    _Entry.Path := ini.ReadString(DelphiToolMenuEntry, 'Path', '');
    _Entry.WorkingDir := ini.ReadString(DelphiToolMenuEntry, 'WorkingDir', '');
    _Entry.Params := ini.ReadString(DelphiToolMenuEntry, 'Params', '');
  finally
    FreeAndNil(ini);
  end;
end;

procedure TToolPropertiesEnhancer.SaveEntry(const _fn: string; const _Entry: TToolsEntry);
var
  ini: TMemIniFile;
begin
  ini := TMemIniFile.Create(_fn);
  try
    ini.WriteString(DelphiToolMenuEntry, 'Title', _Entry.Title);
    ini.WriteString(DelphiToolMenuEntry, 'Path', _Entry.Path);
    ini.WriteString(DelphiToolMenuEntry, 'WorkingDir', _Entry.WorkingDir);
    ini.WriteString(DelphiToolMenuEntry, 'Params', _Entry.Params);
    ini.UpdateFile;
  finally
    FreeAndNil(ini);
  end;
end;

initialization
finalization
  TGxIdeToolPropertiesEnhancer.SetEnabled(False);
end.
