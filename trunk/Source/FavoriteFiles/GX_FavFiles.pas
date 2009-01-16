unit GX_FavFiles;

{$I GX_CondDefine.inc}

interface

uses
  GX_FavUtil, DropTarget, FileView, Windows, SysUtils, Classes, Controls, Forms,
  ComCtrls, Menus, ExtCtrls, ImgList, ActnList, ToolWin, Dialogs, GX_BaseForm;

type
  TfmFavFiles = class(TfmBaseForm)
    MainMenu: TMainMenu;
    mitFile: TMenuItem;
    mitNewFile: TMenuItem;
    mitNewFolder: TMenuItem;
    mitFileDelete: TMenuItem;
    mitFileSep2: TMenuItem;
    mitFileExit: TMenuItem;
    mitOptions: TMenuItem;
    mitHelp: TMenuItem;
    mitHelpHelp: TMenuItem;
    mitHelpContents: TMenuItem;
    mitHelpSep1: TMenuItem;
    mitHelpAbout: TMenuItem;
    tvFolders: TTreeView;
    splTreeView: TSplitter;
    StatusBar: TStatusBar;
    ilFolders: TImageList;
    dlgGetFiles: TOpenDialog;
    mitFileSep1: TMenuItem;
    mitFileProperties: TMenuItem;
    pmuFolders: TPopupMenu;
    mitTreeNewFolder: TMenuItem;
    mitTreeDeleteFolder: TMenuItem;
    mitTreeSep1: TMenuItem;
    mitTreeProperties: TMenuItem;
    pmuFiles: TPopupMenu;
    mitFNewFile: TMenuItem;
    mitFDelete: TMenuItem;
    mitFExecute: TMenuItem;
    mitCSep2: TMenuItem;
    mitFProperties: TMenuItem;
    ilSystem: TImageList;
    mitOptionsOptions: TMenuItem;
    ilSysLarge: TImageList;
    mitFView: TMenuItem;
    mitViewLarge: TMenuItem;
    mitViewSmall: TMenuItem;
    mitViewList: TMenuItem;
    mitViewDetails: TMenuItem;
    mitCSep1: TMenuItem;
    pnlFiles: TPanel;
    ListView: TListView;
    splFileView: TSplitter;
    pnlFileView: TPanel;
    Actions: TActionList;
    actOptionsOptions: TAction;
    actFileExit: TAction;
    actFileProperties: TAction;
    actFileDelete: TAction;
    actFileNewFile: TAction;
    actFileNewFolder: TAction;
    actHelpHelp: TAction;
    actHelpContents: TAction;
    actHelpAbout: TAction;
    ToolBar: TToolBar;
    tbnFileNewFile: TToolButton;
    tbnFileDelete: TToolButton;
    tbnSep1: TToolButton;
    tbnFileProperties: TToolButton;
    tbnSep2: TToolButton;
    tbnNavLevelUp: TToolButton;
    tbnSep3: TToolButton;
    tbnNavExpand: TToolButton;
    tbnNavContract: TToolButton;
    tbnSep4: TToolButton;
    tbnHelpHelp: TToolButton;
    actNavExpand: TAction;
    actNavContract: TAction;
    actNavLevelUp: TAction;
    actViewLargeIcons: TAction;
    actViewSmallIcons: TAction;
    actViewList: TAction;
    actViewDetails: TAction;
    actFileExecute: TAction;
    mitCSep0: TMenuItem;
    tbnFileNewFolder: TToolButton;
    actFileRename: TAction;
    mitFRename: TMenuItem;
    procedure tvFoldersChange(Sender: TObject; Node: TTreeNode);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure tvFoldersKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ListViewChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure ListViewEdited(Sender: TObject; Item: TListItem; var S: string);
    procedure tvFoldersEdited(Sender: TObject; Node: TTreeNode; var S: string);
    procedure ListViewDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure tvFoldersDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure tvFoldersDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure tvFoldersChanging(Sender: TObject; Node: TTreeNode; var AllowChange: Boolean);
    procedure tvFoldersEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure actFileNewFolderExecute(Sender: TObject);
    procedure actFileExitExecute(Sender: TObject);
    procedure actFileNewFileExecute(Sender: TObject);
    procedure actNavLevelUpExecute(Sender: TObject);
    procedure actOptionsOptionsExecute(Sender: TObject);
    procedure actNavContractExecute(Sender: TObject);
    procedure actNavExpandExecute(Sender: TObject);
    procedure actHelpHelpExecute(Sender: TObject);
    procedure actHelpContentsExecute(Sender: TObject);
    procedure actFileDeleteExecute(Sender: TObject);
    procedure actFilePropertiesExecute(Sender: TObject);
    procedure actFileExecuteExecute(Sender: TObject);
    procedure actViewLargeIconsExecute(Sender: TObject);
    procedure actViewSmallIconsExecute(Sender: TObject);
    procedure actViewListExecute(Sender: TObject);
    procedure actViewDetailsExecute(Sender: TObject);
    procedure actHelpAboutExecute(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure mitFViewClick(Sender: TObject);
    procedure actFileRenameExecute(Sender: TObject);
    procedure ActionsUpdate(Action: TBasicAction; var Handled: Boolean);
  private
    FFileViewer: TFileViewer;
    FEntryFile: string;
    FFolderDelete: Boolean;
    FExpandAll: Boolean;
    FExecHide: Boolean;
    FModified: Boolean;
    FFileDrop: TDropFileTarget;
    function GetFolder(const FolderNode: TTreeNode): TGXFolder;
    function GetFile(const FileItem: TListItem): TGXFile;
    procedure FileToListItem(const AFile: TGXFile; const AListItem: TListItem);
    procedure SetupSystemImageLists;
    function AddFolder(const Text: string; FType: TFolderType): TTreeNode;
    procedure DeleteSelectedFiles;
    procedure DeleteCurrentFolder;
    procedure LogAndShowLoadError(const E: Exception);
    procedure SaveEntries;
    procedure LoadEntries;
    procedure LoadSettings;
    procedure SaveSettings;
    procedure CreateFolders(Folder: TGXFolder; Node: TTreeNode);
    procedure DropFiles(Sender: TObject; ShiftState: TShiftState; Point: TPoint; var Effect: Longint);
    procedure AddFilesToCurrentFolder(Files: TStrings);
    function HaveSelectedItemInActiveControl: Boolean;
    procedure CreateNewFolder;
    procedure CreateNewFile;
    procedure ExecuteSelectedFiles;
    procedure EditFolder;
    procedure EditFile;
    function GetShowPreview: Boolean;
    procedure SetShowPreview(Value: Boolean);
    property ShowPreview: Boolean read GetShowPreview write SetShowPreview;
    function CreateEmptyRootNode: TTreeNode;
    function ConfigurationKey: string;
    function ExecuteFile(AListItem: TListItem): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AssignIconImage(Image: TImage; const ContainerFileName: string);
    procedure SetFilter;
  end;

implementation

{$R *.dfm}

uses
  Messages, ShellAPI, DropSource,
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  ToolsAPI,
  GX_FavNewFolder, GX_FavFolderProp, GX_FavFileProp, GX_FavOptions,
  {$IFNDEF STANDALONE}
  GX_ConfigurationInfo, GX_Experts, GX_GExperts,
  {$ENDIF STANDALONE}
  GX_GxUtils, GX_GenericUtils, GX_OtaUtils, GX_SharedImages, OmniXML,
  GX_XmlUtils, GX_IdeUtils, Math;

type
  EFavFiles = class(Exception);

  TFilesExpert = class(TGX_Expert)
  private
    FFavoriteFiles: TfmFavFiles;
  protected
    procedure SetActive(New: Boolean); override;
  public
    function GetActionCaption: string; override;
    class function GetName: string; override;
    procedure Click(Sender: TObject); override;
    function HasConfigOptions: Boolean; override;
  end;

const // Do not localize any of the strings in this const section:
  XmlAttributeFolderName = 'Name';
  XmlAttributeFolderType = 'FolderType';
  XmlAttributeFileName = 'Name';
  XmlAttributeFileFileName = 'FileName';
  XmlAttributeFileDescription = 'Description';
  XmlAttributeFileExecType = 'ExecType';
  XmlAttributeFileExecProg = 'ExecProg';

  XmlNodeRoot = 'FavoriteFiles';
  XmlNodeFolder = 'Folder';
  XmlNodeFile = 'File';

  XmlExecTypeNames: array[TExecType] of string = ('LoadInIDE', 'ShellExecute', 'Custom', 'Project');
  XmlFolderTypeNames: array[TFolderType] of string = ('Normal', 'Source', 'Bitmaps', 'Glyphs', 'Documentation');

function XmlStringToExecType(const ExecTypeString: string): TExecType;
var
  ExecType: TExecType;
begin
  for ExecType := Low(TExecType) to High(TExecType) do
    if XmlExecTypeNames[ExecType] = ExecTypeString then
    begin
      Result := ExecType;
      Exit;
    end;
  raise EConvertError.CreateFmt('Unknown ExecType value "%s"', [ExecTypeString]);
end;

function XmlStringToFolderType(const FolderTypeString: string): TFolderType;
var
  FolderType: TFolderType;
begin
  for FolderType := Low(TFolderType) to High(TFolderType) do
    if XmlFolderTypeNames[FolderType] = FolderTypeString then
    begin
      Result := FolderType;
      Exit;
    end;
  raise EConvertError.CreateFmt('Unknown FolderType value "%s"', [FolderTypeString]);
end;

resourcestring
  SFavorites = 'Favorites';

procedure TfmFavFiles.AssignIconImage(Image: TImage; const ContainerFileName: string);
var
  Icon: HIcon;
  ID: Word;
begin
  if not FileExists(ContainerFileName) then
    Exit;

  Icon := ExtractAssociatedIcon(HInstance, PChar(ContainerFileName), ID);
  if Icon <> 0 then
  begin
    Image.Picture.Icon.Handle := Icon;
    Image.Visible := True;
    Image.Refresh;
  end
  else
    Image.Visible := False;
end;

procedure TfmFavFiles.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caHide;
end;

function TfmFavFiles.AddFolder(const Text: string; FType: TFolderType): TTreeNode;
var
  Node: TTreeNode;
  Folder: TGXFolder;
begin
  Folder := nil;
  try
    if tvFolders.Selected = nil then
    begin
      Folder := TGXFolder.Create(Root);
      Node := tvFolders.Items.AddObject(nil, Text, Folder);
    end
    else
    begin
      Folder := TGXFolder.Create(GetFolder(tvFolders.Selected));
      Node := tvFolders.Items.AddChildObject(tvFolders.Selected, Text, Folder);
    end;
    FModified := True;
  except
    on E: Exception do
    begin
      FreeAndNil(Folder);
      raise;
    end;
  end;
  Folder.FolderType := FType;
  Node.ImageIndex := Ord(FType) * 2;
  Node.SelectedIndex := Node.ImageIndex + 1;
  Folder.FolderName := Text;
  Result := Node;
end;

procedure TfmFavFiles.tvFoldersChange(Sender: TObject; Node: TTreeNode);
resourcestring
  SItems = '%d favorite(s)';
var
  Folder: TGXFolder;
  mFile: TGXFile;
  i: Integer;
  LItem: TListItem;
  Cursor: IInterface;
begin
  if (csDestroying in ComponentState) then
    Exit;
  if tvFolders.Selected = nil then
    Exit;

  Cursor := TempHourGlassCursor;
  ListView.Items.BeginUpdate;
  try
    ListView.Items.Clear;
    ListView.SortType := stNone;
    Folder := GetFolder(tvFolders.Selected);
    for i := 0 to Folder.FileCount - 1 do
    begin
      mFile := Folder.Files[i];
      LItem := ListView.Items.Add;
      FileToListItem(mFile, LItem);
    end;
    StatusBar.SimpleText := Format(SItems, [ListView.Items.Count]);
  finally
    ListView.SortType := stText;
    ListView.Items.EndUpdate;
    tvFolders.Selected := tvFolders.Selected;
  end;
end;

procedure TfmFavFiles.DeleteSelectedFiles;
var
  i: Integer;
begin
  if ListView.Selected = nil then Exit;
  i := 0;
  while i <= ListView.Items.Count - 1 do
  begin
    if ListView.Items[i].Selected then
    begin
      GetFile(ListView.Items[i]).Free;
      ListView.Items[i].Delete;
    end
    else
      Inc(i);
  end;
  ListView.Arrange(arDefault);
  FModified := True;
end;

procedure TfmFavFiles.SaveEntries;

  procedure SetFolderAttributes(const Folder: TGXFolder; const FolderNode: IXMLElement);
  begin
    FolderNode.SetAttribute(XmlAttributeFolderName, Folder.FolderName);
    FolderNode.SetAttribute(XmlAttributeFolderType, XmlFolderTypeNames[Folder.FolderType]);
  end;

  procedure SetFileAttributes(const AFile: TGXFile; const FileNode: IXMLElement);
  begin
    FileNode.SetAttribute(XmlAttributeFileName, AFile.DName);
    FileNode.SetAttribute(XmlAttributeFileFileName, AFile.FileName);
    if AFile.Description <> '' then
      FileNode.SetAttribute(XmlAttributeFileDescription, AFile.Description);
    FileNode.SetAttribute(XmlAttributeFileExecType, XmlExecTypeNames[AFile.ExecType]);
    if AFile.ExecType = etCustom then
      FileNode.SetAttribute(XmlAttributeFileExecProg, AFile.ExecProg);
  end;

  procedure SaveFolder(const Folder: TGXFolder; const Doc: IXMLDocument;
    const ParentNode: IXMLElement);
  var
    FolderNode, FileNode: IXMLElement;
    i: Integer;
  begin
    FolderNode := Doc.CreateElement(XmlNodeFolder);
    SetFolderAttributes(TGXFolder(Folder), FolderNode);
    ParentNode.AppendChild(FolderNode);

    for i := 0 to Folder.FolderCount - 1 do
      SaveFolder(Folder.Folders[i], Doc, FolderNode);

    for i := 0 to Folder.FileCount - 1 do
    begin
      FileNode := Doc.CreateElement(XmlNodeFile);
      SetFileAttributes(Folder.Files[i], FileNode);
      FolderNode.AppendChild(FileNode);
    end;
  end;

resourcestring
  SSaveError = 'Your favorite files data could not be saved to:' + sLineBreak +
    '  %s' + sLineBreak +
    'Please verify that the path exists and the file can be written to.';
var
  Doc: IXMLDocument;
  RootNode: IXMLElement;
begin
  if not FModified then
    Exit;

  try
    {$IFOPT D+} SendDebug('Saving favorites'); {$ENDIF}
    Doc := CreateXMLDoc;
    AddXMLHeader(Doc);

    RootNode := Doc.CreateElement(XmlNodeRoot);
    Doc.DocumentElement := RootNode;
    SaveFolder(Root, Doc, RootNode);

    Doc.Save(FEntryFile, ofIndent);
  except
    on E: Exception do
    begin
      {$IFOPT D+} SendDebugError('Saving favorite entries: ' + E.Message); {$ENDIF}
      MessageDlg(Format(SSaveError, [E.Message]), mtError, [mbOK], 0);
    end;
  end;
end;

procedure TfmFavFiles.CreateFolders(Folder: TGXFolder; Node: TTreeNode);
var
  SubFolder: TGXFolder;
  CNode: TTreeNode;
  i: Integer;
begin
  for i := 0 to Folder.FolderCount - 1 do
  begin
    SubFolder := TGXFolder(Folder.Folders[i]);
    CNode := tvFolders.Items.AddChildObject(Node, SubFolder.FolderName, SubFolder);
    CNode.ImageIndex := Ord(SubFolder.FolderType) * 2;
    CNode.SelectedIndex := (Ord(SubFolder.FolderType) * 2) + 1;
    CreateFolders(SubFolder, CNode);
  end;
end;

procedure TfmFavFiles.LogAndShowLoadError(const E: Exception);
resourcestring
  SLoadError = 'Error while loading %s' + sLineBreak;
begin
  GxLogAndShowException(E, Format(SLoadError, [FEntryFile]));
end;

function GetStringAttribute(const Node: IXMLNode; const AttributeName: string): string;
var
  AttrNode: IXMLNode;
begin
  AttrNode := Node.Attributes.GetNamedItem(AttributeName);
  if Assigned(AttrNode) then
    Result := AttrNode.NodeValue
  else
    Result := '';
end;

function GetRequiredStringAttribute(const Node: IXMLNode; const AttributeName: string): string;
var
  AttrNode: IXMLNode;
begin
  AttrNode := Node.Attributes.GetNamedItem(AttributeName);
  if Assigned(AttrNode) then
    Result := AttrNode.NodeValue
  else
    raise EFavFiles.CreateFmt('Missing %s Attribute', [AttributeName]);
end;

function GetExecTypeAttribute(const Node: IXMLNode): TExecType;
var
  AttrNode: IXMLNode;
begin
  AttrNode := Node.Attributes.GetNamedItem(XmlAttributeFileExecType);
  if Assigned(AttrNode) then
    Result := XmlStringToExecType(AttrNode.NodeValue)
  else
    raise EFavFiles.Create('Missing ExecType Attribute');
end;

function GetFolderTypeAttribute(const Node: IXMLNode): TFolderType;
var
  AttrNode: IXMLNode;
begin
  AttrNode := Node.Attributes.GetNamedItem(XmlAttributeFolderType);
  if Assigned(AttrNode) then
    Result := XmlStringToFolderType(AttrNode.NodeValue)
  else
    raise EFavFiles.Create('Missing FolderType Attribute');
end;

procedure TfmFavFiles.LoadEntries;

  procedure LoadFolder(const Folder: TGXFolder; const FolderNode: IXMLNode);
  var
    ChildNode: IXMLNode;
    i: Integer;
    SubFolder: TGXFolder;
    mFile: TGXFile;
  begin
    Folder.FolderName := GetRequiredStringAttribute(FolderNode, XmlAttributeFolderName);
    Folder.FolderType := GetFolderTypeAttribute(FolderNode);

    for i := 0 to FolderNode.ChildNodes.Length - 1 do
    begin
      ChildNode := FolderNode.ChildNodes.Item[i];
      try
        if ChildNode.NodeName = XmlNodeFolder then
        begin
          SubFolder := TGXFolder.Create(Folder);
          LoadFolder(SubFolder, ChildNode);
        end
        else
          if ChildNode.NodeName = XmlNodeFile then
          begin
            mFile := TGXFile.Create(Folder);
            mFile.DName := GetRequiredStringAttribute(ChildNode, XmlAttributeFileName);
            mFile.FileName := GetRequiredStringAttribute(ChildNode, XmlAttributeFileFileName);
            mFile.ExecType := GetExecTypeAttribute(ChildNode);
            mFile.Description := GetStringAttribute(ChildNode, XmlAttributeFileDescription);
            mFile.ExecProg := GetStringAttribute(ChildNode, XmlAttributeFileExecProg);
          end;
      except
        on e: EFavFiles do
          LogAndShowLoadError(e);
        on e: EConvertError do
          LogAndShowLoadError(e);
      end;
    end;
  end;

resourcestring
  SSaveWarning = 'The storage directory defined in the GExperts configuration ' +
    'dialog is not valid.  Make sure that you have selected a valid folder ' +
    'or your favorite files will not be saved.';
  SMissingRootFolder =
    'Error: Missing root folder in file "%s"!' + sLineBreak + sLineBreak +
    'Overwrite existing file with a new empty favorites file?';
var
  Doc: IXMLDocument;
  RootFolderNode: IXMLNode;
  Node: TTreeNode;
  ErrorMsg: string;
begin
  FModified := False;
  Node := nil;
  if not FileExists(FEntryFile) then
  begin
    Node := CreateEmptyRootNode;
    if not DirectoryExists(ExtractFilePath(FEntryFile)) then
      MessageDlg(SSaveWarning, mtWarning, [mbOK], 0);
  end
  else begin
    Doc := CreateXMLDoc;
    Doc.Load(FEntryFile);
    RootFolderNode := Doc.DocumentElement.SelectSingleNode(XmlNodeFolder);
    if RootFolderNode = nil then
    begin
      {$IFOPT D+}
      ErrorMsg := Format('Loading favorites: Missing root folder in file "%s".', [FEntryFile]);
      SendDebugError(ErrorMsg);
      {$ENDIF}
      ErrorMsg := Format(SMissingRootFolder, [FEntryFile]);
      if MessageDlg(ErrorMsg, mtError, [mbYes, mbNo], 0) = mrYes then
        Node := CreateEmptyRootNode
      else
        Abort;
    end
    else begin
      LoadFolder(Root, RootFolderNode);
      Node := tvFolders.Items.AddObject(nil, Root.FolderName, Root);
    end;
  end;

  Node.ImageIndex := 0;
  Node.SelectedIndex := 1;
  CreateFolders(Root, Node);
  Node.Expand(FExpandAll);
end;

procedure TfmFavFiles.EditFolder;
var
  Dlg: TfmFavFolderProperties;
  Folder: TGXFolder;
begin
  if tvFolders.Selected = nil then
    Exit;

  Folder := GetFolder(tvFolders.Selected);
  Dlg := TfmFavFolderProperties.Create(nil);
  try
    Dlg.FavoriteFilesForm := Self;
    Dlg.edtFolderName.Text := Folder.FolderName;
    Dlg.cbxFolderType.ItemIndex := Ord(Folder.FolderType);
    if Dlg.ShowModal = mrOk then
    begin
      FModified := True;
      Folder.FolderName := Dlg.edtFolderName.Text;
      Folder.FolderType := TFolderType(Dlg.cbxFolderType.ItemIndex);
      tvFolders.Selected.Text := Folder.FolderName;
      tvFolders.Selected.ImageIndex := Ord(Folder.FolderType) * 2;
      tvFolders.Selected.SelectedIndex := (Ord(Folder.FolderType) * 2) + 1;
    end;
  finally
    FreeAndNil(Dlg);
  end;
end;

procedure TfmFavFiles.EditFile;
var
  mFile: TGXFile;
  Dlg: TfmFavFileProp;
begin
  mFile := GetFile(ListView.Selected);
  if mFile = nil then
    Exit;

  Dlg := TfmFavFileProp.Create(nil);
  try
    with Dlg do
    begin
      FavoriteFilesForm := Self;
      edtFilename.Text := mFile.FileName;
      edtName.Text := mFile.DName;
      edtDescription.Text := mFile.Description;
      cbxExecuteType.ItemIndex := Ord(mFile.ExecType);
      edtExecuteUsing.Text := mFile.ExecProg;
      AssignIconImage(imgFileIcon, mFile.FileName);
      if ShowModal = mrOk then
      begin
        FModified := True;
        mFile.FileName := edtFilename.Text;
        mFile.Description := edtDescription.Text;
        mFile.DName := edtName.Text;
        mFile.ExecType := TExecType(cbxExecuteType.ItemIndex);
        mFile.ExecProg := edtExecuteUsing.Text;
        FileToListItem(mFile, ListView.Selected);
      end;
    end;
  finally
    FreeAndNil(Dlg);
  end;
end;

procedure TfmFavFiles.DeleteCurrentFolder;
resourcestring
  SConfirmDeleteFolder = 'Do you want to delete the current folder %s?';
  SCannotDeleteRoot = 'You cannot delete the root folder. Please select a different folder.';
var
  SelectedItem: TTreeNode;
begin
  SelectedItem := tvFolders.Selected;
  if SelectedItem = nil then Exit;
  if SelectedItem.Level = 0 then
  begin
    MessageDlg(SCannotDeleteRoot, mtError, [mbOK], 0);
    Exit;
  end;
  if FFolderDelete then
    if MessageDlg(Format(SConfirmDeleteFolder, [SelectedItem.Text]), mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
      Exit;
  GetFolder(SelectedItem).Free;
  ListView.Items.Clear; { GXFiles freed by folder being deleted }
  SelectedItem.Delete;
  FModified := True;
end;

procedure TfmFavFiles.tvFoldersKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if tvFolders.IsEditing then Exit;
  case Key of
    VK_DELETE:
      begin
        DeleteCurrentFolder;
        Key := 0;
      end;
    VK_INSERT:
      begin
        CreateNewFolder;
        Key := 0;
      end;
  end;
end;

procedure TfmFavFiles.SaveSettings;
var
  Settings: TGExpertsSettings;
  Key: string;
begin
  // Do not localize.
  Settings := TGExpertsSettings.Create;
  try
    Settings.WriteInteger(ConfigurationKey, 'Left', Left);
    Settings.WriteInteger(ConfigurationKey, 'Top', Top);
    Settings.WriteInteger(ConfigurationKey, 'Width', Width);
    Settings.WriteInteger(ConfigurationKey, 'Height', Height);
    Settings.WriteInteger(ConfigurationKey, 'Splitter', Max(tvFolders.Width, 30));
    Settings.WriteInteger(ConfigurationKey, 'Splitter2', Max(FFileViewer.Height, 30));
    Settings.WriteBool(ConfigurationKey, 'FolderDelete', FFolderDelete);
    Settings.WriteBool(ConfigurationKey, 'ExpandAll', FExpandAll);
    Settings.WriteBool(ConfigurationKey, 'ExecHide', FExecHide);
    Settings.WriteBool(ConfigurationKey, 'ShowPreview', ShowPreview);
    Settings.WriteInteger(ConfigurationKey, 'ListView', Ord(ListView.ViewStyle));
  finally
    FreeAndNil(Settings);
  end;

  Key := AddSlash(ConfigInfo.GExpertsIdeRootRegistryKey) + ConfigurationKey;
  SaveTreeSettings(tvFolders, TreeSaveAll, Key);
end;

procedure TfmFavFiles.LoadSettings;
var
  Settings: TGExpertsSettings;
  Key: string;
begin
  // Do not localize.
  Settings := TGExpertsSettings.Create;
  try
    Left := Settings.ReadInteger(ConfigurationKey, 'Left', Left);
    Top := Settings.ReadInteger(ConfigurationKey, 'Top', Top);
    Width := Settings.ReadInteger(ConfigurationKey, 'Width', Width);
    Height := Settings.ReadInteger(ConfigurationKey, 'Height', Height);
    tvFolders.Width := Settings.ReadInteger(ConfigurationKey, 'Splitter', tvFolders.Width);
    FFileViewer.Height := Settings.ReadInteger(ConfigurationKey, 'Splitter2', FFileViewer.Height);
    FFolderDelete := Settings.ReadBool(ConfigurationKey, 'FolderDelete', FFolderDelete);
    FExpandAll := Settings.ReadBool(ConfigurationKey, 'ExpandAll', FExpandAll);
    FExecHide := Settings.ReadBool(ConfigurationKey, 'ExecHide', FExecHide);
    ShowPreview := Settings.ReadBool(ConfigurationKey, 'ShowPreview', ShowPreview);
    ListView.ViewStyle := TViewStyle(Settings.ReadInteger(ConfigurationKey, 'ListView', Ord(ListView.ViewStyle)));
    Assert(ListView.ViewStyle in [Low(TViewStyle)..High(TViewStyle)])
  finally
    FreeAndNil(Settings);
  end;

  Key := AddSlash(ConfigInfo.GExpertsIdeRootRegistryKey) + ConfigurationKey;
  LoadTreeSettings(tvFolders, TreeSaveAll, Key);
end;

procedure TfmFavFiles.SetupSystemImageLists;
var
  AHandle: DWord;
  FileInfo: TSHFileInfo;
begin
  // Who is responsible for freeing that returned handle?
  // We do not do it, since both imagelists share their images
  // The Win32 API docs do not mention anything.
  AHandle := SHGetFileInfo('', 0, FileInfo, SizeOf(FileInfo), SHGFI_SMALLICON or SHGFI_SYSICONINDEX);
  if AHandle <> 0 then
  begin
    ilSystem.Handle := AHandle;
    ilSystem.ShareImages := True;
  end;

  AHandle := SHGetFileInfo('', 0, FileInfo, SizeOf(FileInfo), SHGFI_ICON or SHGFI_SYSICONINDEX);
  if AHandle <> 0 then
  begin
    ilSysLarge.Handle := AHandle;
    ilSysLarge.ShareImages := True;
  end;
end;

procedure TfmFavFiles.SetFilter;
var
  Folder: TGXFolder;
begin
  if tvFolders.Selected = nil then
    Exit;

  Folder := GetFolder(tvFolders.Selected);
  case Folder.FolderType of
    ftNormal: dlgGetFiles.FilterIndex := 10;
    ftSource: dlgGetFiles.FilterIndex := 1;
    ftBitmap: dlgGetFiles.FilterIndex := 5;
    ftGlyph:  dlgGetFiles.FilterIndex := 5;
    ftDocs:   dlgGetFiles.FilterIndex := 4;
    else      dlgGetFiles.FilterIndex := 10;
  end;
end;

procedure TfmFavFiles.ListViewChange(Sender: TObject; Item: TListItem; Change: TItemChange);
resourcestring
  SFilesSelected = '%d files selected';
  SFileMissingString = '%s (missing)';
var
  LoadFile: string;
begin
  if (csDestroying in ComponentState) then Exit;

  if (ListView.Selected <> nil) and ShowPreview then
  begin
    if ListView.SelCount = 1 then
    begin
      LoadFile := GetFile(ListView.Selected).FileName;
      if FileExists(LoadFile) or DirectoryExists(LoadFile) then
      begin
        StatusBar.SimpleText := LoadFile;

        if not (FFileViewer.LoadedFile = LoadFile) then
          FFileViewer.LoadFromFile(LoadFile);
      end
      else
        StatusBar.SimpleText := Format(SFileMissingString, [LoadFile]);
    end
    else
      StatusBar.SimpleText := Format(SFilesSelected, [ListView.SelCount]);
  end
  else
  begin
    StatusBar.SimpleText := '';
    FFileViewer.Clear;
  end;
end;

procedure TfmFavFiles.ListViewEdited(Sender: TObject; Item: TListItem; var S: string);
begin
  GetFile(Item).DName := S;
  FModified := True;
end;

procedure TfmFavFiles.tvFoldersEdited(Sender: TObject; Node: TTreeNode; var S: string);
begin
  GetFolder(Node).FolderName := S;
  FModified := True;
end;

procedure TfmFavFiles.ListViewDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := False;
end;

procedure TfmFavFiles.tvFoldersDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  Node: TTreeNode;
begin
  Accept := (Source is TTreeview) or (Source is TListView);
  if Source is TTreeView then
  begin
    Node := tvFolders.GetNodeAt(X, Y);
    if Node = nil then
      Accept := False
    else
      if tvFolders.Selected.Level = 0 then
        Accept := False
  end;
end;

procedure TfmFavFiles.tvFoldersDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  Node: TTreeNode;
  i: Integer;
  mFile: TGXFile;
  Folder: TGXFolder;
  Cursor: IInterface;
begin
  try
    Node := tvFolders.GetNodeAt(X, Y);
    if Node = nil then Exit;
    if Source = tvFolders then
    begin
      if (tvFolders.Selected = nil) or (tvFolders.Selected = Node) or Node.HasAsParent(tvFolders.Selected) then
        Exit;
      Cursor := TempHourGlassCursor;
      tvFolders.Items.BeginUpdate;
      try
        tvFolders.Selected.MoveTo(Node, naAddChild);
        Folder := GetFolder(tvFolders.Selected);
        Folder.Owner := GetFolder(Node);
        FModified := True;
        (* TODO 3 -cCleanup -oAnyone: Is this code still necessary?
        tvFolders.Selected.DeleteChildren;
        CreateFolders(Folder, tvFolders.Selected);
        *)
      finally
        tvFolders.Items.EndUpdate;
        Screen.Cursor := crDefault;
      end;
    end
    else if Source = ListView then
    begin
      i := 0;
      while i <= ListView.Items.Count - 1 do
      begin
        if ListView.Items[i].Selected then
        begin
          mFile := GetFile(ListView.Items[i]);
          mFile.Owner := GetFolder(Node);
          ListView.Items[i].Delete;
          FModified := True;
        end
        else
          Inc(i);
      end;
    end;
  except
    on E: Exception do
    begin
      GxLogAndShowException(E);
      tvFolders.EndDrag(False);
    end;
  end;
end;

procedure TfmFavFiles.tvFoldersChanging(Sender: TObject; Node: TTreeNode; var AllowChange: Boolean);
begin
  AllowChange := True;
end;

procedure TfmFavFiles.tvFoldersEndDrag(Sender, Target: TObject; X, Y: Integer);
begin
  tvFolders.EndDrag(False);
end;

procedure TfmFavFiles.FormShow(Sender: TObject);
begin
  //LoadSettings;
end;

procedure TfmFavFiles.FormHide(Sender: TObject);
begin
  SaveEntries;
  SaveSettings;
end;

procedure TfmFavFiles.DropFiles(Sender: TObject; ShiftState: TShiftState; Point: TPoint; var Effect: Longint);
begin
  AddFilesToCurrentFolder(FFileDrop.Files);
end;

function UpperFileExtToExecType(const FileExt: string): TExecType;
begin
  if IsBdsProjectFile(FileExt) then
    Result := etProject
  else if IsBdsSourceFile(FileExt) or IsTextFile(FileExt) then
    Result := etLoadInIDE
  else
    Result := etShell;
end;

procedure TfmFavFiles.AddFilesToCurrentFolder(Files: TStrings);
var
  mFile: TGXFile;
  i: Integer;
  LItem: TListItem;
  Folder: TGXFolder;
  Cursor: IInterface;
begin
  if (Files = nil) or (Files.Count < 1) or (tvFolders.Selected = nil) then
    Exit;

  LItem := nil;
  Folder := GetFolder(tvFolders.Selected);
  Cursor := TempHourGlassCursor;
  ListView.Items.BeginUpdate;
  try
    for i := 0 to Files.Count - 1 do
    begin
      mFile := TGXFile.Create(Folder);
      try
        mFile.FileName := Files[i];
        mFile.Description := mFile.FileName;
        mFile.DName := ExtractFileName(mFile.FileName);
        mFile.ExecType := UpperFileExtToExecType(ExtractUpperFileExt(mFile.FileName));
        mFile.ExecProg := '';
        LItem := ListView.Items.Add;
        FileToListItem(mFile, LItem);
      except
        on E: Exception do
        begin
          FreeAndNil(mFile);
          raise;
        end;
      end;
      FModified := True;
    end;
  finally
    ListView.Items.EndUpdate;
  end;
  if Assigned(LItem) then
  begin
    ListView.Selected := nil;
    ListView.Selected := LItem;
    ListView.ItemFocused := LItem;
    ListView.Selected.MakeVisible(False);
  end;
end;

procedure TfmFavFiles.SetShowPreview(Value: Boolean);
begin
  if Value <> pnlFileView.Visible then
  begin
    pnlFileView.Visible := Value;
    splFileView.Visible := Value;
    FFileViewer.Clear;
    if (Value) and (ListView.Selected <> nil) then
      ListViewChange(ListView, ListView.Selected, ctState);
  end;
end;

function TfmFavFiles.GetShowPreview: Boolean;
begin
  Result := pnlFileView.Visible;
end;

procedure TfmFavFiles.actFileNewFolderExecute(Sender: TObject);
begin
  CreateNewFolder;
end;

procedure TfmFavFiles.actFileExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfmFavFiles.actFileNewFileExecute(Sender: TObject);
begin
  CreateNewFile;
end;

procedure TfmFavFiles.actNavLevelUpExecute(Sender: TObject);
begin
  if tvFolders.Selected <> nil then
    if tvFolders.Selected.Parent <> nil then
      tvFolders.Selected := tvFolders.Selected.Parent;
end;

procedure TfmFavFiles.actOptionsOptionsExecute(Sender: TObject);
var
  Dlg: TfmFavOptions;
begin
  Dlg := TfmFavOptions.Create(nil);
  try
    Dlg.chkConfirmFolderDelete.Checked := FFolderDelete;
    Dlg.chkExpandAllOnLoad.Checked := FExpandAll;
    Dlg.chkHideOnExecute.Checked := FExecHide;
    Dlg.chkShowPreview.Checked := ShowPreview;
    if Dlg.ShowModal = mrOk then
    begin
      FFolderDelete := Dlg.chkConfirmFolderDelete.Checked;
      FExpandAll := Dlg.chkExpandAllOnLoad.Checked;
      FExecHide := Dlg.chkHideOnExecute.Checked;
      ShowPreview := Dlg.chkShowPreview.Checked;
    end;
  finally
    FreeAndNil(Dlg);
  end;
end;

procedure TfmFavFiles.actNavContractExecute(Sender: TObject);
begin
  tvFolders.Items[0].Collapse(True);
end;

procedure TfmFavFiles.actNavExpandExecute(Sender: TObject);
begin
  tvFolders.Items[0].Expand(True);
end;

procedure TfmFavFiles.actHelpHelpExecute(Sender: TObject);
begin
  GxContextHelp(Self, 15);
end;

procedure TfmFavFiles.actHelpContentsExecute(Sender: TObject);
begin
  GxContextHelpContents(Self);
end;

procedure TfmFavFiles.actFileDeleteExecute(Sender: TObject);
begin
  if tvFolders.Selected = nil then Exit;

  if ListView.IsEditing then
  begin
    SendMessage(GetFocus, WM_KEYDOWN, VK_DELETE, 0);
    SendMessage(GetFocus, WM_KEYUP, VK_DELETE, 0);
  end
  else
  begin
    if ActiveControl = tvFolders then
      DeleteCurrentFolder
    else if ActiveControl = ListView then
      DeleteSelectedFiles;
  end;
end;

function TfmFavFiles.HaveSelectedItemInActiveControl: Boolean;
begin
  Result := False;

  if ActiveControl = tvFolders then
    Result := (tvFolders.Selected <> nil)
  else if ActiveControl = ListView then
    Result := (ListView.Selected <> nil);
end;

procedure TfmFavFiles.ActionsUpdate(Action: TBasicAction; var Handled: Boolean);
var
  HaveSelection: Boolean;
  NavEnabled: Boolean;
begin
  HaveSelection := HaveSelectedItemInActiveControl;
  if tvFolders.Selected <> nil then
    NavEnabled := not (tvFolders.Selected.Level = 0)
  else
    NavEnabled := False;

  actNavLevelUp.Enabled := NavEnabled;
  actFileDelete.Enabled := HaveSelection;
  actFileProperties.Enabled := HaveSelection;
  actFileExecute.Enabled := Assigned(ListView.Selected);
end;

procedure TfmFavFiles.CreateNewFolder;
begin
  with TfmFavNewFolder.Create(nil) do
  try
    FavoriteFilesForm := Self;
    if ShowModal = mrOk then
      tvFolders.Selected := AddFolder(edtFolderName.Text, TFolderType(cbxFolderType.ItemIndex));
  finally
    Free;
  end;
end;

procedure TfmFavFiles.CreateNewFile;
var
  CurrentIdeFolder: string;
begin
  if tvFolders.Selected = nil then Exit;

  SetFilter;
  CurrentIdeFolder := GetCurrentDir;
  try
    if GetOpenSaveDialogExecute(dlgGetFiles) then
      AddFilesToCurrentFolder(dlgGetFiles.Files);
  finally
    SetCurrentDir(CurrentIdeFolder);
  end;
end;

procedure TfmFavFiles.actFilePropertiesExecute(Sender: TObject);
begin
  if ActiveControl = tvFolders then
    EditFolder
  else if ActiveControl = ListView then
    EditFile;
end;

procedure TfmFavFiles.actFileExecuteExecute(Sender: TObject);
begin
  if ListView.IsEditing then
  begin
    SendMessage(GetFocus, WM_KEYDOWN, VK_RETURN, 0);
    SendMessage(GetFocus, WM_KEYUP, VK_RETURN, 0);
  end
  else
    ExecuteSelectedFiles;
end;

procedure TfmFavFiles.actViewLargeIconsExecute(Sender: TObject);
begin
  ListView.ViewStyle := vsIcon;
end;

procedure TfmFavFiles.actViewSmallIconsExecute(Sender: TObject);
begin
  ListView.ViewStyle := vsSmallIcon;
end;

procedure TfmFavFiles.actViewListExecute(Sender: TObject);
begin
  ListView.ViewStyle := vsList;
end;

procedure TfmFavFiles.actViewDetailsExecute(Sender: TObject);
begin
  ListView.ViewStyle := vsReport;
end;

function TfmFavFiles.ExecuteFile(AListItem: TListItem): Boolean;
var
  mFile: TGXFile;
  Ext: string;
resourcestring
  SFileDoesNotExist = 'Could not find the file %s to execute it.';
  SCouldNotOpen = 'Could not open file %s';
begin
  Result := False;
  mFile := GetFile(AListItem);
  if mFile = nil then
    Exit;

  if not (FileExists(mFile.FileName) or DirectoryExists (mFile.FileName)) then
  begin
    MessageDlg(Format(SFileDoesNotExist, [mFile.FileName]), mtError, [mbOK], 0);
    Exit;
  end
  else
    case mFile.ExecType of
      etLoadInIDE:
        begin
          if not GxOtaMakeSourceVisible(ExpandFileName(mFile.FileName)) then
            MessageDlg(Format(SCouldNotOpen, [ExpandFilename(mFile.FileName)]), mtError, [mbOK], 0)
          else begin
            Self.Hide;
            Result := True;
          end;
        end;
      etShell:
        begin
          GXShellExecute(mFile.FileName, '', True);
          Result := True;
        end;
      etCustom:
        begin
          GXShellExecute(mFile.ExecProg, mFile.FileName, True);
          Result := True;
        end;
      etProject:
        begin
          Ext := ExtractUpperFileExt(mFile.FileName);
          if (BorlandIdeServices as IOTAModuleServices).CloseAll then
          begin
            // D6 gives an AV calling OpenProject on these file types
            if IsBpg(Ext) or IsBpk(Ext) then
              (BorlandIdeServices as IOTAActionServices).OpenFile(mFile.FileName)
            else
              (BorlandIdeServices as IOTAActionServices).OpenProject(mFile.FileName, True);
            if (not IsStandAlone) and FExecHide then
              Self.Hide;
          end;
        end;
    end;
end;

procedure TfmFavFiles.ExecuteSelectedFiles;
var
  i: Integer;
  ListItem: TListItem;
begin
  if (not (csDestroying in ComponentState)) and (ListView.SelCount > 0) then
  begin
    SaveEntries;
    for i := 0 to ListView.Items.Count - 1 do
    begin
      ListItem := ListView.Items[i];
      if ListItem.Selected then
        if not ExecuteFile(ListItem) then
          Break;
    end;
  end;
end;

constructor TfmFavFiles.Create(AOwner: TComponent);
resourcestring
  SOpenFilter = // Note: Localize only the descriptive text, not the extensions
    'Source Files (*.dpr;*.bpr;*.dpk;*.bpk;*.bpg;*.pas;*.cpp;*.hpp;*.c;*.h)|*.dpr;*.bpr;*.dpk;*.bpk;*.bpg;*.pas;*.cpp;*.hpp;*.c;*.h' +
    '|Project Files (*.dpr;*.bpr;*.dpk;*.bpk;*.bpg;*.bdsproj;*.bdsgroup;*.dproj)|*.dpr;*.bpr;*.dpk;*.bpk;*.bpg;*.bdsproj;*.bdsgroup;*.dproj' +
    '|Pascal Files (*.pas;*.inc)|*.pas;*.inc' +
    '|Help Files (*.chm;*.hlp)|*.chm;*.hlp' +
    '|Graphics Files (*.bmp;*.wmf)|*.bmp;*.wmf' +
    '|Text Files (*.txt;*.me;*.asc)|*.txt;*.me;*.asc' +
    '|Executable Files (*.exe)|*.exe' +
    '|SQL Scripts (*.sql)|*.sql' +
    '|C/C++ (*.c;*.cpp;*.h;*.hpp)|*.c;*.cpp;*.h;*.hpp' +
    '|All Files (' + AllFilesWildCard + ')|' + AllFilesWildCard;
const
  FaveFavFile = 'FavoriteFiles.xml'; // Do not localize.
begin
  inherited;

  SetToolbarGradient(ToolBar);
  pnlFileView.Caption := '';
  SetNonModalFormPopupMode(Self);
  splTreeView.AutoSnap := False;
  splFileView.AutoSnap := False;

  dlgGetFiles.Filter := SOpenFilter;
  FFileViewer := TFileViewer.Create(nil);
  FFileViewer.Parent := pnlFileView;
  FFileViewer.Align := alClient;

  FExpandAll := False;
  FFolderDelete := True;
  FExecHide := True;
  ShowPreview := True;

  SetupSystemImageLists;
  FFileDrop := TDropFileTarget.Create(nil);
  FFileDrop.OnDrop := DropFiles;
  FFileDrop.DragTypes := [dtCopy, dtMove, dtLink];
  FFileDrop.ShowImage := True;
  FFileDrop.Register(ListView);

  // Assign a default entry file location.  Do not localize.
  if IsStandAlone then
  begin
    if RunningLinux then
      FEntryFile := '~/' + FaveFavFile
    else
      FEntryFile := 'C:\' + FaveFavFile;
  end
  else
    FEntryFile := AddSlash(ConfigInfo.ConfigPath) + FaveFavFile;

  CenterForm(Self);
  LoadSettings;
  LoadEntries;
  if (tvFolders.Selected = nil) and (tvFolders.Items.Count > 0) then
    tvFolders.Selected := tvFolders.Items[0];
  ListView.Columns[0].Width := ColumnTextWidth;
  ListView.Columns[1].Width := ColumnTextWidth;
  ListView.Columns[2].Width := ColumnTextWidth;
  ListView.Columns[3].Width := ColumnTextWidth;
end;

destructor TfmFavFiles.Destroy;
begin
  FreeAndNil(FFileViewer);

  FFileDrop.Unregister;
  FreeAndNil(FFileDrop);

  inherited;
end;

procedure TfmFavFiles.actHelpAboutExecute(Sender: TObject);
begin
  ShowGXAboutForm;
end;

procedure TfmFavFiles.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #27 then
  begin
    Key := #0;
    actFileExit.Execute;
  end;
end;

procedure TfmFavFiles.mitFViewClick(Sender: TObject);
begin
  mitViewDetails.Checked := ListView.ViewStyle = vsReport;
  mitViewLarge.Checked := ListView.ViewStyle = vsIcon;
  mitViewList.Checked := ListView.ViewStyle = vsList;
  mitViewSmall.Checked := ListView.ViewStyle = vsSmallIcon;
end;

procedure TfmFavFiles.actFileRenameExecute(Sender: TObject);
begin
  if Assigned(ListView.Selected) then
    ListView.Selected.EditCaption;
end;

function TfmFavFiles.CreateEmptyRootNode: TTreeNode;
begin
  Assert(Assigned(Root));
  Result := tvFolders.Items.AddObject(nil, SFavorites, Root);
  Root.FolderName := SFavorites;
  Root.FolderType := GX_FavUtil.ftNormal;
end;

function TfmFavFiles.ConfigurationKey: string;
begin
  Result := TFilesExpert.ConfigurationKey;
end;

function TfmFavFiles.GetFolder(const FolderNode: TTreeNode): TGXFolder;
begin
  if FolderNode = nil then
    Result := nil
  else
    Result := TGXFolder(FolderNode.Data);
end;

function TfmFavFiles.GetFile(const FileItem: TListItem): TGXFile;
begin
  if FileItem = nil then
    Result := nil
  else
    Result := TGXFile(FileItem.Data);
end;

procedure TfmFavFiles.FileToListItem(const AFile: TGXFile; const AListItem: TListItem);
begin
  AListItem.Caption := AFile.DName;
  AListItem.SubItems.Clear;
  AListItem.SubItems.Add(AFile.FileName);
  AListItem.SubItems.Add(AFile.Description);
  if AFile.ExecType = etCustom then
    AListItem.SubItems.Add(ExecTypeNames[AFile.ExecType] + ': ' + AFile.ExecProg)
  else
    AListItem.SubItems.Add(ExecTypeNames[AFile.ExecType]);
  AListItem.Data := AFile;
  AListItem.ImageIndex := GetSystemImageIndexForFile(AFile.FileName);
end;

{ TFilesExpert }

procedure TFilesExpert.SetActive(New: Boolean);
begin
  if New <> Active then
  begin
    inherited SetActive(New);
    if not New then
    begin
      if Assigned(FFavoriteFiles) then
      begin
        if FFavoriteFiles.Visible then
          FFavoriteFiles.Close;

        FreeAndNil(FFavoriteFiles);
      end;
    end;
  end;
end;

function TFilesExpert.GetActionCaption: string;
resourcestring
  SMenuCaption = 'Favorite &Files';
begin
  Result := SMenuCaption;
end;

class function TFilesExpert.GetName: string;
begin
  Result := 'FavoriteFiles'; // Do not localize.
end;

procedure TFilesExpert.Click(Sender: TObject);
begin
  if FFavoriteFiles = nil then
  begin
    FFavoriteFiles := TfmFavFiles.Create(nil);
    SetFormIcon(FFavoriteFiles);
  end;
  if FFavoriteFiles.WindowState = wsMinimized then
    FFavoriteFiles.WindowState := wsNormal;
  FFavoriteFiles.Show;
end;

function TFilesExpert.HasConfigOptions: Boolean;
begin
  Result := False;
end;

initialization
  RegisterGX_Expert(TFilesExpert);

end.

