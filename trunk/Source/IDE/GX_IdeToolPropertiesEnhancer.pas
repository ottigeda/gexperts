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
  u_dzVclUtils,
  u_dzClassUtils,
  u_dzFileUtils,
  GX_IdeDialogEnhancer,
  GX_GenericUtils,
  GX_CustomClipboard;

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
  TClipFormatToolsEntry = class(TGXCustomClipboard)
  public
    constructor Create;
    function TryReadFromClipboard(out _ToolsEntry: TToolsEntry): Boolean;
    procedure WriteToClipboard(const _ToolsEntry: TToolsEntry);
  end;

type
  TToolPropertiesEnhancer = class(TIdeDialogEnhancer)
  private
    FClipFormat: TClipFormatToolsEntry;
    edTitle: TEdit;
    edProgram: TEdit;
    edWorkingDir: TEdit;
    edParameters: TEdit;
    FDirectory: string;
    FOrigMacroOnClick: TNotifyEvent;
    FForm: TForm;
    FOrigFormHeight: Integer;
    FOrigMacrolistHeight: Integer;
    procedure HandleFilesDropped(_Sender: TObject; _Files: TStrings);
    procedure b_ExportClick(_Sender: TObject);
    procedure b_ImportClick(_Sender: TObject);
    procedure LoadEntry(const _fn: string; out _Entry: TToolsEntry);
    procedure SaveEntry(const _fn: string; const _Entry: TToolsEntry);
    procedure HandleCopyToClipboard(_Sender: TObject);
    procedure HandlePasteFromClipboard(_Sender: TObject);
    procedure GetEntry(out _Entry: TToolsEntry);
    procedure SetEntry(const _Entry: TToolsEntry);
    procedure HandleMacroButtonClick(_Sender: TObject);
  protected
    function IsDesiredForm(_Form: TCustomForm): Boolean; override;
    procedure EnhanceForm(_Form: TForm); override;
  public
    constructor Create;
    destructor Destroy; override;
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
  FClipFormat := TClipFormatToolsEntry.Create;
end;

destructor TToolPropertiesEnhancer.Destroy;
begin
  FreeAndNil(FClipFormat);
  inherited;
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

type
  TCustomButtonHack = class(TCustomButton)
  end;

procedure TToolPropertiesEnhancer.EnhanceForm(_Form: TForm);
var
  b_Export: TButton;
  HelpButton: TButton;
  CancelButton: TButton;
  b_Import: TButton;
  pm: TPopupMenu;
  // The macro button is a button in Delphi 2007 and a TBitBtn in 10.3, so a TCustomButton should work for all
  MacroButton: TCustomButton;
  MacroList: TListBox;
begin
// Drop files only works in Delphi 6 and 7 while autocomplete works in all versions.
// The "new" IDE apparently does something to TEdits that prevent them to receive WM_DROPFILES
// messages.
// I tried to use this for re-registering the drop files handler but it did not help:
//  FControlChangedHandle := TIDEFormEnhancements.RegisterControlChangeCallback(HandleControlChanged);

  if _Form.FindComponent('GxExportButton') <> nil then begin
    // Form has already been enhanced
    Exit; //==>
  end;

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
  MacroButton := _Form.FindComponent('MacroButton') as TCustomButton;
  FOrigMacroOnClick := TCustomButtonHack(MacroButton).OnClick;
  TCustomButtonHack(MacroButton).OnClick := Self.HandleMacroButtonClick;

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

  pm := TPopupMenu.Create(_Form);
  TPopupMenu_AppendMenuItem(pm, 'Copy entry to clipboard', HandleCopyToClipboard);
  TPopupMenu_AppendMenuItem(pm, 'Paste entry from clipboard', HandlePasteFromClipboard);
  _Form.PopupMenu := pm;

  FForm := _Form;
  FOrigFormHeight := FForm.Height;
  MacroList := FForm.FindComponent('MacroList') as TListBox;
  FOrigMacrolistHeight := MacroList.Height;
end;

procedure TToolPropertiesEnhancer.HandleMacroButtonClick(_Sender: TObject);
var
  MacroList: TListBox;
  Diff: Integer;
  Monitor: TMonitor;
  WorkArea: TRectLTWH;
  DesiredHeight: Integer;
begin
  MacroList := FForm.FindComponent('MacroList') as TListBox;
  FOrigMacroOnClick(_Sender);
  if FForm.Height > FOrigFormHeight then begin
    DesiredHeight := 540;
    Monitor := TForm_GetMonitor(FForm);
    if not Assigned(Monitor) then
      Exit; //==>
    TRectLTWH_Assign(WorkArea, Monitor.WorkareaRect);
    if DesiredHeight > WorkArea.Height then
      DesiredHeight := WorkArea.Height;
    Diff := DesiredHeight - FForm.Height;
    FForm.Height := DesiredHeight;
    MacroList.Height := MacroList.Height + Diff;
    TMonitor_MakeFullyVisible(Monitor, FForm);
  end else begin
    MacroList.Height := FOrigMacrolistHeight;
  end;
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

procedure TToolPropertiesEnhancer.GetEntry(out _Entry: TToolsEntry);
begin
  _Entry.Title := edTitle.Text;
  _Entry.Path := edProgram.Text;
  _Entry.WorkingDir := edWorkingDir.Text;
  _Entry.Params := edParameters.Text;
end;

procedure TToolPropertiesEnhancer.SetEntry(const _Entry: TToolsEntry);
begin
  edTitle.Text := _Entry.Title;
  edProgram.Text := _Entry.Path;
  edWorkingDir.Text := _Entry.WorkingDir;
  edParameters.Text := _Entry.Params;
end;

procedure TToolPropertiesEnhancer.b_ExportClick(_Sender: TObject);
var
  fn: string;
  Entry: TToolsEntry;
begin
  GetEntry(Entry);

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

  SetEntry(Entry);
end;

procedure TToolPropertiesEnhancer.HandleCopyToClipboard(_Sender: TObject);
var
  Entry: TToolsEntry;
begin
  GetEntry(Entry);
  FClipFormat.WriteToClipboard(Entry);
end;

procedure TToolPropertiesEnhancer.HandlePasteFromClipboard(_Sender: TObject);
var
  Entry: TToolsEntry;
begin
  if FClipFormat.TryReadFromClipboard(Entry) then
    SetEntry(Entry);
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

{ TClipFormatToolsEntry }

constructor TClipFormatToolsEntry.Create;
begin
  inherited Create('Delphi.ToolsEntry');
end;

function TClipFormatToolsEntry.TryReadFromClipboard(out _ToolsEntry: TToolsEntry): Boolean;
var
  WideCharArr: array of WideChar;
  Idx: Integer;
  Size: Integer;

  function Pull: WideString;
  var
    i: Integer;
  begin
    Result := '';
    i := 0;
    while (WideCharArr[Idx + i] <> #0) and (Idx + i < Size) do begin
      Result := Result + WideCharArr[Idx + i];
      Inc(i);
    end;
    Inc(Idx, i + 1);
  end;

var
  Buffer: TByteDynArray;
begin
  Result := doTryReadFromClipboard(Buffer);
  if not Result then begin
    // clipboard does not contain that format
    Exit; //==>
  end;
  Size := Length(Buffer);
  if Odd(Size) then begin
    // Buffer length cannot be odd
    Exit; //==>
  end;
  Size := Size div 2;
  SetLength(WideCharArr, Size);
  Move(Buffer[0], WideCharArr[0], Size * 2);

  Idx := 0;

  _ToolsEntry.Title := Pull();
  _ToolsEntry.Path := Pull();
  _ToolsEntry.WorkingDir := Pull();
  _ToolsEntry.Params := Pull();
end;

procedure TClipFormatToolsEntry.WriteToClipboard(const _ToolsEntry: TToolsEntry);
var
  WideCharArr: array of WideChar;
  Idx: Integer;
  Size: Integer;

  procedure Push(const _ws: WideString);
  var
    Len: Integer;
    i: Integer;
  begin
    Len := Length(_ws);
    for i := 1 to Len do begin
      Assert(Idx + i - 1 < Size);
      WideCharArr[Idx + i - 1] := _ws[i];
    end;
    Inc(Idx, Len);
    WideCharArr[Idx] := #0;
    Inc(Idx);
  end;

var
  ws: WideString;
  Buffer: TByteDynArray;
begin
  Size := Length(_ToolsEntry.Title) + 1
    + Length(_ToolsEntry.Path) + 1
    + Length(_ToolsEntry.WorkingDir) + 1
    + Length(_ToolsEntry.Params) + 1;
  SetLength(WideCharArr, Size);

  Idx := 0;
  ws := _ToolsEntry.Title;
  Push(ws);
  ws := _ToolsEntry.Path;
  Push(ws);
  ws := _ToolsEntry.WorkingDir;
  Push(ws);
  ws := _ToolsEntry.Params;
  Push(ws);
  SetLength(Buffer, Size * 2);
  Move(WideCharArr[0], Buffer[0], Size * 2);
  doWriteToClipboard(Buffer);
end;

initialization
finalization
  TGxIdeToolPropertiesEnhancer.SetEnabled(False);
end.

