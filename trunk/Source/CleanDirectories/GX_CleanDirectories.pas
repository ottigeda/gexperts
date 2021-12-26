unit GX_CleanDirectories;

{$I GX_CondDefine.inc}

{$WARN UNIT_PLATFORM OFF}
{$WARN SYMBOL_PLATFORM OFF}

// TODO 3 -cFeature -oAnyone: Add save/restore of recursive directory checkboxes and custom-added directories?

interface

uses
  Classes, Controls, Forms, StdCtrls, CheckLst, ExtCtrls, ActnList, Menus, Actions, UITypes,
  GX_Experts, GX_ConfigurationInfo, GX_BaseForm;

type
  TCleanDirectoriesExpert = class;

  TfmCleanDirectories = class(TfmBaseForm)
    ActionList: TActionList;
    actDirsCheckAll: TAction;
    actDirsUncheckAll: TAction;
    actExtsCheckAll: TAction;
    actExtsUncheckAll: TAction;
    pmuDirs: TPopupMenu;
    mitDirsCheckAll: TMenuItem;
    mitDirsUncheckAll: TMenuItem;
    pmuExts: TPopupMenu;
    mitExtsCheckAll: TMenuItem;
    mitExtsUncheckAll: TMenuItem;
    actDirsInvert: TAction;
    actExtsInvert: TAction;
    mitDirsInvertChecked: TMenuItem;
    mitExtsInvertChecked: TMenuItem;
    pnlButtons: TPanel;
    pnlButtonsRight: TPanel;
    pnlDirs: TPanel;
    pnlDirectories: TPanel;
    gbxDirectories: TGroupBox;
    btnHelp: TButton;
    btnClean: TButton;
    btnCancel: TButton;
    chkReportErrors: TCheckBox;
    pnlBottom: TPanel;
    gbxExtensions: TGroupBox;
    pnlExtButtons: TPanel;
    btnAddExt: TButton;
    btnRemoveExt: TButton;
    pnlExtensions: TPanel;
    clbExtensions: TCheckListBox;
    pnlMessage: TPanel;
    lCleaning: TLabel;
    laStatus: TLabel;
    pnlDirList: TPanel;
    pnlDirButtons: TPanel;
    btnAdd: TButton;
    btnRemove: TButton;
    pnlDirMessage: TPanel;
    lblDirMessage: TLabel;
    clbDirs: TCheckListBox;
    procedure FormDestroy(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnCleanClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure clbDirsClick(Sender: TObject);
    procedure btnAddExtClick(Sender: TObject);
    procedure btnRemoveExtClick(Sender: TObject);
    procedure clbDirsKeyPress(Sender: TObject; var Key: Char);
    procedure clbExtensionsClick(Sender: TObject);
    procedure clbExtensionsKeyPress(Sender: TObject; var Key: Char);
    procedure FormResize(Sender: TObject);
    procedure CheckActionExecute(Sender: TObject);
  private
    CleanExpert: TCleanDirectoriesExpert;
    CleanExtList: TStringList;
    FTotalBytesCleaned: Integer;
    FTotalFilesCleaned: Integer;
    procedure InitializeForm;
    procedure FillProjectDirectoriesList;
    procedure AddHorizontalScrollbar;
    procedure PerformCleaning;
    procedure CleanDirectory(const Directory: string; const Recursing: Boolean);
    procedure DeleteFoundFile(const FileName: string);
    procedure SaveSettings;
    procedure LoadSettings;
    procedure UpdateCleanExtList;
    procedure clbDirsOnFilesDropped(_Sender: TObject; _Files: TStrings);
    procedure clbExtensionsOnFilesDropped(_Sender: TObject; _Files: TStrings);
  public
    constructor CreateParametrized(OwningExpert: TCleanDirectoriesExpert);
  end;

  TCleanDirectoriesExpert = class(TGX_Expert)
  private
    FReportErrors: Boolean;
    FExtensionList: TStrings;
    FCleanList: TStrings;
    FIncludeBinaryDirs: Boolean;
  protected
    procedure InternalLoadSettings(_Settings: IExpertSettings); override;
    procedure InternalSaveSettings(_Settings: IExpertSettings); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetActionCaption: string; override;
    class function GetName: string; override;
    procedure Execute(Sender: TObject); override;
    function HasConfigOptions: Boolean; override;
    procedure Configure; override;
    property ExtensionList: TStrings read FExtensionList;
    property CleanList: TStrings read FCleanList;
  end;

implementation

{$R *.dfm}

uses
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  Variants, SysUtils, FileCtrl, Dialogs, Math,
  ToolsAPI, GX_GxUtils, GX_GenericUtils, GX_OtaUtils, u_dzVclUtils;

resourcestring
  SCouldNotDelete = 'Could not delete %s' + sLineBreak +
                    sLineBreak +
                    'Continue showing errors?';

{ TfmCleanDirectories }

procedure TfmCleanDirectories.btnAddClick(Sender: TObject);
var
  Temp: string;
begin
  Temp := '';
  if GetDirectory(Temp) then
  begin
    Temp := AddSlash(Temp);
    clbDirs.Items.Add(Temp);
    AddHorizontalScrollbar;
  end;
end;

procedure TfmCleanDirectories.btnAddExtClick(Sender: TObject);
resourcestring
  SAddNewExtension = 'Add file extension';
  SAddNewText = 'Enter the file extension to be cleaned:';
  SStarNotAllowed = '.* is not an allowable clean extension';
var
  NewExt: string;
  Idx: Integer;
begin
  if InputQuery(SAddNewExtension, SAddNewText, NewExt) then
  begin
    if NewExt[1] = '*' then
      Delete(NewExt, 1, 1);
    if not (NewExt[1] = '.') then
      NewExt := '.' + NewExt;
    NewExt := Trim(NewExt);
    if NewExt = '.*' then
      raise Exception.Create(SStarNotAllowed);
    Idx := clbExtensions.Items.Add(NewExt);
    clbExtensions.Checked[Idx] := True;
  end;
end;

procedure TfmCleanDirectories.btnCleanClick(Sender: TObject);
begin
  btnClean.Enabled := False;
  btnCancel.Enabled := False;
  try
    PerformCleaning;
  finally
    btnClean.Enabled := True;
    btnCancel.Enabled := True;
    CleanExpert.SaveSettings;
    SaveSettings;

    ModalResult := mrOk;
  end;
end;

procedure TfmCleanDirectories.btnHelpClick(Sender: TObject);
begin
  GxContextHelp(Self, 14);
end;

procedure TfmCleanDirectories.btnRemoveClick(Sender: TObject);
var
  i: Integer;
  OldIndex: Integer;
begin
  i := 0;
  OldIndex := clbDirs.ItemIndex;
  // MultiSelect isn't published/implemented in Delphi 5
  while i <= clbDirs.Items.Count - 1 do
  begin
    if clbDirs.Selected[i] then
      clbDirs.Items.Delete(i)
    else
      Inc(i);
  end;
  if (OldIndex > -1) and (clbDirs.Items.Count > 0) then
    clbDirs.ItemIndex := Min(OldIndex, clbDirs.Items.Count - 1);
  btnRemove.Enabled := (clbDirs.ItemIndex > -1) and (clbDirs.Items.Count > 0);
end;

procedure TfmCleanDirectories.btnRemoveExtClick(Sender: TObject);
var
  i: Integer;
begin
  i := clbExtensions.ItemIndex;
  if i < 0 then
    Exit;

  clbExtensions.Checked[i] := False;
  clbExtensions.Items.Delete(i);

  btnRemoveExt.Enabled := (clbExtensions.ItemIndex > -1) and (clbExtensions.Items.Count > 0);
end;

procedure TfmCleanDirectories.clbDirsClick(Sender: TObject);
begin
  btnRemove.Enabled := (clbDirs.ItemIndex > -1);
end;

procedure TfmCleanDirectories.clbDirsKeyPress(Sender: TObject; var Key: Char);
begin
  btnRemove.Enabled := (clbDirs.ItemIndex > -1);
end;

procedure TfmCleanDirectories.clbExtensionsClick(Sender: TObject);
begin
  btnRemoveExt.Enabled := (clbExtensions.ItemIndex > -1);
end;

procedure TfmCleanDirectories.clbExtensionsKeyPress(Sender: TObject; var Key: Char);
begin
  btnRemoveExt.Enabled := (clbExtensions.ItemIndex > -1);
end;

procedure TfmCleanDirectories.CleanDirectory(const Directory: string; const Recursing: Boolean);
{$IFNDEF GX_VER150_up}
// Delphi 6 does not have this constant
const
  faSymLink   = $00000040;
{$endif}
var
  SearchRec: TSearchRec;
  SearchAttr: Integer;
  FindResult: Integer;
  i: Integer;
begin
  // We explicitly do not search for r/o files since we cannot
  // delete them anyway; alternatively do search r/o files and have
  // an error reported?
  SearchAttr := faHidden or faSysFile or faArchive;

  if Recursing then
    SearchAttr := SearchAttr or faDirectory;

  FindResult := FindFirst(Directory + AllFilesWildCard, SearchAttr, SearchRec);
  try
    laStatus.Caption := MinimizeName(Directory, laStatus.Canvas, laStatus.Width);
    laStatus.Repaint;
    while FindResult = 0 do
    begin
      // Do not localize strings in the following expression.
      // Note: this test includes the "recursing" test as we
      // will only find faDirectory if we are recursing.
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
      begin
        if ((SearchRec.Attr and faDirectory) <> 0) then
        begin
          // Skip Junctions
          if ((SearchRec.Attr and faSymLink) = 0) then begin
            // Recurse into sub-directories.
            SearchRec.Name := AddSlash(SearchRec.Name);
            CleanDirectory(Directory + SearchRec.Name, Recursing);
          end;
        end
        else
        begin
          // Delete files with matching extension(s).
          for i := 0 to CleanExtList.Count - 1 do
          begin
            //if SameFileName(FoundFileExt, CleanExtList.Strings[i]) then
            if WildcardCompare('*'+CleanExtList.Strings[i], SearchRec.Name, True) then
            begin
              DeleteFoundFile(Directory + SearchRec.Name);
              Break;
            end;
          end;
        end;
      end;

      FindResult := FindNext(SearchRec);
    end;
  finally
    FindClose(SearchRec);
  end;
end;

constructor TfmCleanDirectories.CreateParametrized(OwningExpert: TCleanDirectoriesExpert);
begin
  CleanExpert := OwningExpert;

  inherited Create(nil);

  TControl_SetMinConstraints(Self);
  TWinControl_ActivateDropFiles(clbDirs, clbDirsOnFilesDropped);
  TWinControl_ActivateDropFiles(clbExtensions, clbExtensionsOnFilesDropped);

  InitDpiScaler;

  InitializeForm;
end;

procedure TfmCleanDirectories.clbDirsOnFilesDropped(_Sender: TObject; _Files: TStrings);
var
  i: Integer;
  fn: string;
  LastIdxAdded: Integer;
begin
  for i := 0 to _Files.Count - 1 do begin
    fn := _Files[i];
    if SysUtils.DirectoryExists(fn) then
      LastIdxAdded := clbDirs.Items.Add(AddSlash(fn))
    else
      LastIdxAdded := clbDirs.Items.Add(AddSlash(ExtractFileDir(fn)));
    clbDirs.ItemIndex := LastIdxAdded;
  end;
  AddHorizontalScrollbar;
end;

procedure TfmCleanDirectories.clbExtensionsOnFilesDropped(_Sender: TObject; _Files: TStrings);
var
  i: Integer;
  fn: string;
  LastIdxAdded: Integer;
  Ext: string;
begin
  LastIdxAdded := clbDirs.ItemIndex;
  for i := 0 to _Files.Count - 1 do begin
    fn := _Files[i];
    if not SysUtils.DirectoryExists(fn) then begin
      Ext := ExtractFileExt(fn);
      LastIdxAdded := clbExtensions.Items.Add('*' + Ext);
    end;
    clbExtensions.ItemIndex := LastIdxAdded;
  end;
end;

procedure TfmCleanDirectories.DeleteFoundFile(const FileName: string);
{.$DEFINE SimulateDeleting}
{$IFNDEF SimulateDeleting}
var
  TempFileSize: Integer;
{$ENDIF SimulateDeleting}
begin
  {$IFOPT D+}SendDebug('Deleting file: ' + FileName);{$ENDIF}
{$IFNDEF SimulateDeleting}
  TempFileSize := GetFileSize(FileName);
  if DeleteFile(FileName) then
  begin
    Inc(FTotalFilesCleaned);
    FTotalBytesCleaned := FTotalBytesCleaned + TempFileSize;
  end
  else
  begin
    if chkReportErrors.Checked then
    begin
      chkReportErrors.Checked := (MessageDlg(Format(SCouldNotDelete,
                                             [FileName]), mtError, [mbYes, mbNo], 0) = mrYes);
    end;
  end;
{$ENDIF SimulateDeleting}
end;

procedure TfmCleanDirectories.FillProjectDirectoriesList;
var
  Strings: TStrings;

  procedure AddPathToStrings(const Path: string);
  begin
    if Trim(Path) = '' then
      Exit;
    EnsureStringInList(Strings, Path);
  end;


  procedure AddProjectDir(const OptionName: string);
{$IFNDEF GX_VER200_up} // Delphi 2009
  // Delphi < 2009 does not know this constant
  const
    varUString  = $0102; { Unicode string 258 } {not OLE compatible }
{$ENDIF}
  var
    DirectoryVariant: Variant;
    Directory: string;
    ProjectDir: string;
  begin
    if GxOtaTryGetProjectOption(OptionName, DirectoryVariant) then
        case VarType(DirectoryVariant) of
          varString, varOleStr, varUString: begin
          Directory := DirectoryVariant;
          if Trim(Directory) <> '' then
          begin
            if IsPathAbsolute(Directory) then
            begin
              if not SysUtils.DirectoryExists(Directory) then
                Exit;
            end
            else
            begin
              ProjectDir := ExtractFileDir(GxOtaGetCurrentProjectFileName);
              if ProjectDir <> '' then
              begin
                Directory := AddSlash(ProjectDir) + Directory;
                if not SysUtils.DirectoryExists(Directory) then
                  Exit;
              end
              else
                Exit;
            end;
            AddPathToStrings(Directory);
          end;
        end;
      end;
  end;

var
  i: Integer;
  Project: IOTAProject;
  ModuleInfo: IOTAModuleInfo;
  TempPathString: string;
begin
  Project := GxOtaGetCurrentProject;
  if not Assigned(Project) then
    Exit;

  Strings := clbDirs.Items;
  Strings.BeginUpdate;
  try
    AddPathToStrings(ExtractFilePath(Project.FileName));
    for i := 0 to Project.GetModuleCount - 1 do
    begin
      ModuleInfo := Project.GetModule(i);
      Assert(Assigned(ModuleInfo));
      if IsExecutable(ModuleInfo.FileName) then
        Continue;
      TempPathString := ExtractFilePath(ModuleInfo.FileName);
      AddPathToStrings(TempPathString);
    end;
    if CleanExpert.FIncludeBinaryDirs then
    begin
      AddProjectDir('OutputDir');
      AddProjectDir('UnitOutputDir');
      AddProjectDir('PkgDcpDir');
    end;
  finally
    Strings.EndUpdate;
  end;
  AddHorizontalScrollbar;
end;

procedure TfmCleanDirectories.InitializeForm;
const // We will never localize these strings.
  SDefaultCleanExts =
    '.~bpg'+ sLineBreak + '.~cpp'+ sLineBreak + '.~dfm'+ sLineBreak + '.~dpk'+ sLineBreak +
    '.~dsk'+ sLineBreak + '.~h'  + sLineBreak + '.~hpp'+ sLineBreak + '.~pas'+ sLineBreak +
    '.bak' + sLineBreak + '.cfg' + sLineBreak + '.csm' + sLineBreak + '.dcu' + sLineBreak +
    '.dof' + sLineBreak + '.dsk' + sLineBreak + '.fts' + sLineBreak + '.gid' + sLineBreak +
    '.il*' + sLineBreak + '.kwf' + sLineBreak + '.md'  + sLineBreak + '.obj' + sLineBreak +
    '.tds' + sLineBreak + '.tmp' + sLineBreak + '.$*'  + sLineBreak + '.~*'  + sLineBreak +
    '.#??' + sLineBreak + '.ddp' + sLineBreak + '.rsm' + sLineBreak + '.map' + sLineBreak +
    '.pdb' + sLineBreak + '.gex' + sLineBreak +'.~xfm' + sLineBreak + '.~nfm'+ sLineBreak +
    '.~bdsproj'+ sLineBreak + '.~dproj'+ sLineBreak + '.~bdsgroup' + sLineBreak +
    '.~groupproj' + sLineBreak + '.identcache' + sLineBreak + '.dcuil' + sLineBreak + '.dcpil';
var
  i, j: Integer;
begin
  laStatus.Caption := '';
  LoadSettings;

  if CleanExpert.ExtensionList.Count > 0 then
    clbExtensions.Items.Assign(CleanExpert.ExtensionList)
  else
    clbExtensions.Items.Text := SDefaultCleanExts;

  CleanExtList := TStringList.Create;
  CleanExtList.Assign(CleanExpert.CleanList);

  for i := CleanExtList.Count - 1 downto 0 do
  begin
    j := clbExtensions.Items.IndexOf(CleanExtList[i]);
    if j >= 0 then
      clbExtensions.Checked[j] := True
    else
      CleanExtList.Delete(i);
  end;

  FillProjectDirectoriesList;
  CenterForm(Self);
  EnsureFormVisible(Self);
end;

procedure TfmCleanDirectories.FormDestroy(Sender: TObject);
begin
  UpdateCleanExtList;
  CleanExpert.CleanList.Assign(CleanExtList);
  CleanExpert.ExtensionList.Assign(clbExtensions.Items);

  FreeAndNil(CleanExtList);
end;

procedure TfmCleanDirectories.PerformCleaning;
resourcestring
  SCleaningComplete = 'Cleaning complete. %d files were deleted.' + sLineBreak +
                      '%s bytes of storage space were recovered.';
  SOneCleaningComplete = 'Cleaning complete. %d file was deleted.' + sLineBreak +
                      '%s bytes of storage space were recovered.';
var
  i: Integer;
  ConfirmMessage: string;
  Cursor: IInterface;
begin
  UpdateCleanExtList;
  FTotalBytesCleaned := 0;
  FTotalFilesCleaned := 0;

  lCleaning.Visible := True;
  lCleaning.Repaint;

  Cursor := TCursor_TempHourglass;

  try
    for i := 0 to clbDirs.Items.Count - 1 do
    begin
      // Ascertain that we have a trailing slash
      // for each directory item.
      clbDirs.Items[i] := AddSlash(clbDirs.Items[i]);

      laStatus.Caption := MinimizeName(clbDirs.Items[i], laStatus.Canvas, laStatus.Width);
      laStatus.Repaint;

      // If a directory is checked, then the user wants
      // to recurse into that directory and clean all
      // items there, too.
      CleanDirectory(clbDirs.Items[i], clbDirs.Checked[i]);
    end;
  finally
    if FTotalFilesCleaned = 1 then
      ConfirmMessage := SOneCleaningComplete
    else
      ConfirmMessage := SCleaningComplete;
    // Prevent the status dialog from becoming hidden behind this window
    Self.Hide;
    Cursor := nil;
    MessageDlg(Format(ConfirmMessage,
                      [FTotalFilesCleaned,
                       FormatFloat('#,;;0', FTotalBytesCleaned)]),
               mtInformation, [mbOK], 0);

    lCleaning.Visible := False;

    // This saving is also done on destruction, but duplicating it here
    // preserves changes even if Delphi crashes before we are destroyed.
    CleanExpert.CleanList.Assign(CleanExtList);
    CleanExpert.ExtensionList.Assign(clbExtensions.Items);
  end;
end;

procedure TfmCleanDirectories.AddHorizontalScrollbar;
begin
  ListboxHorizontalScrollbar(clbDirs);
end;

procedure TfmCleanDirectories.FormResize(Sender: TObject);
begin
  // Fixes a D5 paint bug when a horizontal scroll bar is visible
  clbDirs.Repaint;
end;

procedure TfmCleanDirectories.LoadSettings;
var
  Settings: IExpertSettings;
begin
  Settings := TCleanDirectoriesExpert.GetSettings;
  // Do not localize any of the below strings.
  Settings.LoadForm('Window', Self, [fsSize]);
end;

procedure TfmCleanDirectories.SaveSettings;
var
  Settings: IExpertSettings;
begin
  Settings := TCleanDirectoriesExpert.GetSettings;
  // Do not localize any of the below strings.
  Settings.SaveForm('Window', Self, [fsSize]);
end;

procedure TfmCleanDirectories.CheckActionExecute(Sender: TObject);
begin
  if Sender = actDirsCheckAll then
    SetListBoxChecked(clbDirs, chAll)
  else if Sender = actDirsUncheckAll then
    SetListBoxChecked(clbDirs, chNone)
  else if Sender = actDirsInvert then
    SetListBoxChecked(clbDirs, chInvert)
  else if Sender = actExtsCheckAll then
    SetListBoxChecked(clbExtensions, chAll)
  else if Sender = actExtsUncheckAll then
    SetListBoxChecked(clbExtensions, chNone)
  else if Sender = actExtsInvert then
    SetListBoxChecked(clbExtensions, chInvert);
  UpdateCleanExtList;
end;

procedure TfmCleanDirectories.UpdateCleanExtList;
var
  i: Integer;
begin
  Assert(Assigned(CleanExtList));
  CleanExtList.Clear;
  for i := 0 to clbExtensions.Items.Count - 1 do
    if clbExtensions.Checked[i] then
      CleanExtList.Add(clbExtensions.Items[i]);
end;

{ TCleanDirectoriesExpert }

constructor TCleanDirectoriesExpert.Create;
begin
  inherited Create;

  FCleanList := TStringList.Create;
  FExtensionList := TStringList.Create;
end;

destructor TCleanDirectoriesExpert.Destroy;
begin
  SaveSettings;

  FreeAndNil(FCleanList);
  FreeAndNil(FExtensionList);

  inherited Destroy;
end;

function TCleanDirectoriesExpert.GetActionCaption: string;
resourcestring
  SMenuCaption = 'Clea&n Directories...';
begin
  Result := SMenuCaption;
end;

class function TCleanDirectoriesExpert.GetName: string;
begin
  Result := 'CleanDirectories';
end;

procedure TCleanDirectoriesExpert.Execute(Sender: TObject);
var
  frm: TfmCleanDirectories;
begin
  frm := TfmCleanDirectories.CreateParametrized(Self);
  try
    SetFormIcon(frm);
    frm.chkReportErrors.Checked := FReportErrors;
    if frm.ShowModal = mrOk then
      IncCallCount;
    FReportErrors := frm.chkReportErrors.Checked;
  finally
    FreeAndNil(frm);
  end;
end;

procedure TCleanDirectoriesExpert.InternalLoadSettings(_Settings: IExpertSettings);
begin
  inherited InternalLoadSettings(_Settings);
  // Do not localize.
  _Settings.ReadStrings('Delete', CleanList, 'CleanExt');
  _Settings.ReadStrings('Extensions', ExtensionList, 'AvailableExt');
  FReportErrors := _Settings.ReadBool('ReportError', True);
  FIncludeBinaryDirs := _Settings.ReadBool('IncludeBinaryDirs', False)
end;

procedure TCleanDirectoriesExpert.InternalSaveSettings(_Settings: IExpertSettings);
begin
  inherited InternalSaveSettings(_Settings);
  // Do not localize.
  _Settings.WriteBool('ReportError', FReportErrors);
  _Settings.WriteBool('IncludeBinaryDirs', FIncludeBinaryDirs);
  _Settings.WriteStrings('Extensions', ExtensionList, 'AvailableExt');
  _Settings.WriteStrings('Delete', CleanList, 'CleanExt');
end;

function TCleanDirectoriesExpert.HasConfigOptions: Boolean;
begin
  Result := True;
end;

procedure TCleanDirectoriesExpert.Configure;
resourcestring
  SAddExeDcuDirsToCleanList =
    'Would you like the project''s unit output and executable' + sLineBreak +
    'output directories included in the default cleanable' + sLineBreak +
    'directory list?';
begin
  FIncludeBinaryDirs := MessageDlg(SAddExeDcuDirsToCleanList,
      mtConfirmation, [mbYes, mbNo], 0) = mrYes;
end;

initialization
  RegisterGX_Expert(TCleanDirectoriesExpert);
end.

