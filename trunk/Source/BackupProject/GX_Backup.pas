unit GX_Backup;

{$I GX_CondDefine.inc}

interface

uses
  Classes, Controls, Forms, ExtCtrls, Dialogs, StdCtrls, UITypes, ComCtrls,
  ToolsAPI, GX_Progress, GX_Experts, GX_ConfigurationInfo,
  GX_Zipper, AbArcTyp, AbUtils, GX_BaseForm, GX_GenericUtils;

type
  TBackupProjectExpert = class;

  TBackupType = (btFile, btDir);

  TBackupScope = (bsActiveProject, bsProjectGroup);

  TfmBackup = class(TfmBaseForm)
    pnlButtons: TPanel;
    pnlFiles: TPanel;
    gbxFiles: TGroupBox;
    pnlButtonsRight: TPanel;
    btnBackup: TButton;
    btnCancel: TButton;
    btnHelp: TButton;
    pnlFileList: TPanel;
    tvFiles: TListView;
    pnlFileButtons: TPanel;
    pnlFileButtonsRight: TPanel;
    btnOptions: TButton;
    btnRemove: TButton;
    btnAdd: TButton;
    procedure btnBackupClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure btnOptionsClick(Sender: TObject);
    procedure tvFilesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure tvFilesData(Sender: TObject; Item: TListItem);
  private
    FDoAbortCollectingFiles: Boolean;
    FHaveCollectedFiles: Boolean;
    FLastZipFile: string;
    FZipEncrypted: Boolean;
    FZipPassword: string;
    FProgressForm: TfmProgress;
    FCurrentBackupScope: TBackupScope;
    FBackupExpert: TBackupProjectExpert;
    FLibraryPath: TStringList;
    FFilesFoundNowhere: TStringList;

    FExcludePatterns: TStringList;
    FProjectGlobalMasks: TStringList;
    FProjectFolders: TStringList;
    
    // ListView Management
    FBaseProjectDir: string;
    FFileList: TStringList; // Stores full file paths
    FTotalSize: Int64; // Total size of all files in bytes

    FZipComponent: TGXZipper;
    FFileSearchThreads: array of TFileFindThread;
    
    procedure AbbreviaProgress(Sender : TObject; Progress : Byte; var Abort : Boolean);
    procedure FileFailure(Sender: TObject; Item: TAbArchiveItem;
      ProcessType: TAbProcessType; ErrorClass: TAbErrorClass; ErrorCode: Integer);
    procedure AfterFileListChange;
    function ListFiles(const FileName, UnitName, FormName: string): Boolean;
    procedure LocateFileOnPathAndAdd(FilesNotFound: TStrings);
    procedure DoCollectFilesFromSingleProject(IProject: IOTAProject);
    procedure DoCollectFiles;
    procedure PerformBackup(const Path, FileName: string);
    procedure AddBackupFile(const FileName: string);
    procedure SaveSettings;
    procedure LoadSettings;
    procedure CollectFilesForBackup;
    procedure IncrementProgress;
    procedure tvFilesOnFilesDropped(_Sender: TObject; _Files: TStrings);
    procedure AddFilesInDirs(_Dirs: TStrings);
    
    // ListView Helper Methods
    procedure InitializeListView;
    procedure AddFileToListView(const FullPath: string);
    function GetFileSize(const FileName: string): Int64;
    function FormatFileSize(Size: Int64): string;
    function GetFileModifiedTime(const FileName: string): TDateTime;
    procedure UpdateSizeColumn;
    
    function IsFileExcluded(const FileName: string): Boolean;
    procedure ProcessGlobalBackupMasks; 
    procedure SearchAndAddFiles(const SearchPath, FileMask: string);
    procedure SearchAndAddFilesRecursive(const SearchPath, FileMask: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TBackupProjectExpert = class(TGX_Expert)
  private
    FFollowLibraryPath: Boolean;
    FBackupInc: Boolean;
    FBackupType: TBackupType;
    FBackupDir: string;
    FIncludeDir: Boolean;
    FBackupScope: TBackupScope;
    FAddDirsRecursively: Boolean;
    FIgnoreHistoryDir: Boolean;
    FIgnoreScmDirs: Boolean;
    FIgnoreBackupFiles: Boolean;
  protected
    procedure InternalLoadSettings(_Settings: IExpertSettings); override;
    procedure InternalSaveSettings(_Settings: IExpertSettings); override;
  public
    constructor Create; override;

    procedure Execute(Sender: TObject); override;
    procedure Configure(_Owner: TWinControl); override;
    function GetActionCaption: string; override;
    class function GetName: string; override;

    property BackupDir: string read FBackupDir;
    property BackupScope: TBackupScope read FBackupScope;
    property BackupType: TBackupType read FBackupType;
    property DoBackupIncludedFiles: Boolean read FBackupInc;
    property DoIncludeDirInfoInZip: Boolean read FIncludeDir;
    property FollowLibraryPath: Boolean read FFollowLibraryPath;
  end;

implementation

{$R *.dfm}

uses
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  Windows, SysUtils, AbConst, StrUtils, Math, Masks,
  GX_OtaUtils, GX_MacroParser, GX_GxUtils,
  GX_BackupOptions, GX_BackupConfig, u_dzVclUtils, GX_MessageBox,
  u_dzFileUtils, GX_BackupNotFound, u_dzStringUtils;

const 
  ItemSeparatorChar = '|';
  
  BackupIncludeDirective: array[0..8] of string =
  ( '{$I ',  '{$INCLUDE ',  '{$RESOURCE ',  '{$R ', '{#BACKUP ',
   '(*$I ', '(*$INCLUDE ', '(*$RESOURCE ', '(*$R ');

  BackupRecursiveIncludeDirective: array[0..0] of string = ( '{#+BACKUP' );

  BackupExcludeDirective: array[0..1] of string =
  ( '{#-BACKUP ', '(*$-BACKUP ' );

// --- ListView Helper Implementations ---

procedure TfmBackup.InitializeListView;
var
  Col: TListColumn;
begin
  tvFiles.ViewStyle := vsReport;
  tvFiles.RowSelect := True;
  tvFiles.ReadOnly := True;
  tvFiles.Columns.Clear;
  
  // Column 1: Path
  Col := tvFiles.Columns.Add;
  Col.Caption := 'Path';
  Col.Width := 300;
  
  // Column 2: File Name
  Col := tvFiles.Columns.Add;
  Col.Caption := 'File Name';
  Col.Width := 150;
  
  // Column 3: Size (with live total)
  Col := tvFiles.Columns.Add;
  Col.Caption := 'Size(0)';
  Col.Width := 120;
  Col.Alignment := taRightJustify;
  
  // Column 4: Last Modified
  Col := tvFiles.Columns.Add;
  Col.Caption := 'Last Modified';
  Col.Width := 150;
  
  FTotalSize := 0;
end;

function TfmBackup.GetFileSize(const FileName: string): Int64;
var
  SearchRec: TSearchRec;
begin
  Result := 0;
  if FindFirst(FileName, faAnyFile, SearchRec) = 0 then
  begin
    Result := Int64(SearchRec.FindData.nFileSizeHigh) shl 32 + SearchRec.FindData.nFileSizeLow;
    SysUtils.FindClose(SearchRec);
  end;
end;

function TfmBackup.FormatFileSize(Size: Int64): string;
const
  KB = 1024;
  MB = KB * 1024;
  GB = MB * 1024;
begin
  if Size >= GB then
    Result := Format('%.2f GB', [Size / GB])
  else if Size >= MB then
    Result := Format('%.2f MB', [Size / MB])
  else if Size >= KB then
    Result := Format('%.2f KB', [Size / KB])
  else
    Result := Format('%d bytes', [Size]);
end;

function TfmBackup.GetFileModifiedTime(const FileName: string): TDateTime;
var
  SearchRec: TSearchRec;
  FileTime: TFileTime;
  SystemTime: TSystemTime;
begin
  Result := 0;
  if FindFirst(FileName, faAnyFile, SearchRec) = 0 then
  begin
    FileTimeToLocalFileTime(SearchRec.FindData.ftLastWriteTime, FileTime);
    FileTimeToSystemTime(FileTime, SystemTime);
    Result := SystemTimeToDateTime(SystemTime);
    SysUtils.FindClose(SearchRec);
  end;
end;

procedure TfmBackup.AddFileToListView(const FullPath: string);
var
  Item: TListItem;
  FileSize: Int64;
  ModifiedTime: TDateTime;
begin
  if not FileExists(FullPath) then Exit;
  
  // Check if file already exists in the list
  if FFileList.IndexOf(FullPath) >= 0 then Exit;
  
  FFileList.Add(FullPath);
  
  Item := tvFiles.Items.Add;
  Item.Caption := ExtractFilePath(FullPath); // Path
  Item.SubItems.Add(ExtractFileName(FullPath)); // File Name
  
  FileSize := GetFileSize(FullPath);
  FTotalSize := FTotalSize + FileSize;
  Item.SubItems.Add(FormatFileSize(FileSize)); // Size
  
  ModifiedTime := GetFileModifiedTime(FullPath);
  Item.SubItems.Add(FormatDateTime('yyyy-mm-dd hh:nn', ModifiedTime)); // Last Modified
  
  UpdateSizeColumn;
end;

procedure TfmBackup.UpdateSizeColumn;
begin
  if tvFiles.Columns.Count > 2 then
    tvFiles.Columns[2].Caption := Format('Size(%s)', [FormatFileSize(FTotalSize)]);
end;

procedure TfmBackup.tvFilesData(Sender: TObject; Item: TListItem);
begin
  // Virtual list data handler (if needed in future)
end;

// --- Global Helper Procedures ---

procedure AddFileToList(const FileSpec: string; List: TStrings);
var
  StoreFileName: string;
begin
  Assert(Assigned(List));
  StoreFileName := Trim(FileSpec);
  if (StoreFileName <> '') and (List.IndexOf(FileSpec) = -1) then
    List.Add(FileSpec);
end;

procedure ScanForIncludesAndAdd(const FileName: string;
  BackupForm: TfmBackup; FoundFileList, NotFoundFileList: TStrings);

  function IsProjectFile(const AFileName: string): Boolean;
  var
    Ext: string;
  begin
    Ext := ExtractFileExt(AFileName);
    Result := SameText(Ext, '.dpr') or SameText(Ext, '.dpk');
  end;

  procedure CollectProjectDirectories(FileLines: TStrings; BaseDir: string);
  var
    i, pIn, pQuoteStart, pQuoteEnd: Integer;
    Line, SPath: string;
  begin
    if BackupForm.FProjectFolders = nil then Exit;
    for i := 0 to FileLines.Count - 1 do
    begin
      Line := Trim(FileLines[i]);
      Line := StringReplace(Line, #9, ' ', [rfReplaceAll]);
      while Pos('  ', Line) > 0 do
        Line := StringReplace(Line, '  ', ' ', [rfReplaceAll]);
      pIn := Pos(' in ''', AnsiLowerCase(Line));
      if pIn > 0 then
      begin
        pQuoteStart := pIn + 5;
        pQuoteEnd := PosEx('''', Line, pQuoteStart);
        if pQuoteEnd > pQuoteStart then
        begin
          SPath := Copy(Line, pQuoteStart, pQuoteEnd - pQuoteStart);
          SPath := ExtractFilePath(SPath);
          if (SPath <> '') then
          begin
            SPath := ExpandFileName(IncludeTrailingPathDelimiter(BaseDir) + SPath);
            SPath := IncludeTrailingPathDelimiter(SPath);
            if BackupForm.FProjectFolders.IndexOf(SPath) = -1 then
              BackupForm.FProjectFolders.Add(SPath);
          end;
        end;
      end;
    end;
  end;

  function GetDirectivePrefix(const Line: string; const Directives: array of string): string;
  var
    i: Integer;
  begin
    Result := '';
    for i := Low(Directives) to High(Directives) do
    begin
      if CaseInsensitivePos(Directives[i], Line) = 1 then
      begin
        Result := Directives[i];
        Break;
      end;
    end;
  end;

  function IsIgnoredExtension(const AFileName: string): Boolean;
  var
    Ext: string;
  begin
    Ext := AnsiLowerCase(AFileName); 
    Result := (Ext = '*.dfm') or (Ext = '*.xfm') or (Ext = '*.nfm');
  end;

  var
    IsProject: Boolean;
    CurrentFileDir: string;

  // --- Parsing Procedures ---

  procedure ParseFileForDirectives(FileLines: TStrings; IncludeLines: TStrings; RecursiveIncludeLines: TStrings);
  var
    i: Integer;
    Line: string;
    Prefix: string;
    FileSpec: string;
    ResolvedMask: string;
    CommentPos: Integer;
  begin
    Assert(Assigned(FileLines));
    Assert(Assigned(IncludeLines));
    Assert(Assigned(RecursiveIncludeLines));

    IncludeLines.Clear;
    RecursiveIncludeLines.Clear;

    for i := 0 to FileLines.Count - 1 do
    begin
      Line := Trim(FileLines[i]);
      
      // 1. Check for EXCLUDES
      Prefix := GetDirectivePrefix(Line, BackupExcludeDirective);
      if Prefix <> '' then
      begin
        FileSpec := Trim(Copy(Line, Length(Prefix) + 1));
        CommentPos := Pos('//', FileSpec);
        if CommentPos > 1 then FileSpec := Copy(FileSpec, 1, CommentPos - 1);
        if RightStr(FileSpec, 1) = '}' then FileSpec := Trim(Copy(FileSpec, 1, Length(FileSpec) - 1));
        if RightStr(FileSpec, 2) = '*)' then FileSpec := Trim(Copy(FileSpec, 1, Length(FileSpec) - 2));

        FileSpec := Trim(FileSpec);
        if FileSpec <> '' then
        begin
          if IsProject and (ExtractFilePath(FileSpec) = '') then
          begin
             ResolvedMask := ExpandFileName(FileSpec);
             if BackupForm.FExcludePatterns.IndexOf(ResolvedMask) = -1 then
               BackupForm.FExcludePatterns.Add(ResolvedMask);
             if BackupForm.FExcludePatterns.IndexOf(FileSpec) = -1 then
               BackupForm.FExcludePatterns.Add(FileSpec);
          end
          else
          begin
             ResolvedMask := ExpandFileName(FileSpec); 
             if BackupForm.FExcludePatterns.IndexOf(ResolvedMask) = -1 then
               BackupForm.FExcludePatterns.Add(ResolvedMask);
          end;
        end;
        Continue; 
      end;

      // 2. Check for RECURSIVE INCLUDES
      Prefix := GetDirectivePrefix(Line, BackupRecursiveIncludeDirective);
      if Prefix <> '' then
      begin
        FileSpec := Trim(Copy(Line, Length(Prefix) + 1));
        CommentPos := Pos('//', FileSpec);
        if CommentPos > 1 then FileSpec := Copy(FileSpec, 1, CommentPos - 1);
        
        if FileSpec = '' then Continue;

        if (FileSpec[1] = '''') or (FileSpec[1] = '"') then
          FileSpec := GetQuotedToken(FileSpec)
        else
        begin
           if RightStr(FileSpec, 1) = '}' then FileSpec := Trim(Copy(FileSpec, 1, Length(FileSpec) - 1));
           if RightStr(FileSpec, 2) = '*)' then FileSpec := Trim(Copy(FileSpec, 1, Length(FileSpec) - 2));
           FileSpec := Trim(FileSpec);
        end;

        if FileSpec <> '' then
          RecursiveIncludeLines.Add(FileSpec);
          
        Continue;
      end;

      // 3. Check for INCLUDES
      Prefix := GetDirectivePrefix(Line, BackupIncludeDirective);
      if Prefix <> '' then
      begin
        FileSpec := Trim(Copy(Line, Length(Prefix) + 1));
        CommentPos := Pos('//', FileSpec);
        if CommentPos > 1 then FileSpec := Copy(FileSpec, 1, CommentPos - 1);
        if FileSpec = '' then Continue;

        FileSpec := GetQuotedToken(FileSpec);
        if RightStr(FileSpec, 1) = '}' then FileSpec := Trim(Copy(FileSpec, 1, Length(FileSpec) - 1));
        if RightStr(FileSpec, 2) = '*)' then FileSpec := Trim(Copy(FileSpec, 1, Length(FileSpec) - 2));

        if FileSpec <> '' then
          IncludeLines.Add(FileSpec);
      end;
    end;
  end;

  procedure GetCppPragmaBackupLines(FileLines: TStrings; IncludeLines: TStrings);
  var
    i, j: Integer;
    Line, ParsedLine, PragmaBackup, FileSpec: string;
    SlashCommentActive, StarCommentActive, QuoteActive, DoubleQuoteActive: Boolean;
  begin
    Assert(Assigned(FileLines));
    Assert(Assigned(IncludeLines));
    StarCommentActive := False;
    IncludeLines.Clear;
    for i := 0 to FileLines.Count - 1 do
    begin
      SlashCommentActive := False;
      QuoteActive := False;
      DoubleQuoteActive := False;
      Line := Trim(FileLines[i]);
      ParsedLine := '';
      j := 1;
      while j <= Length(Line) do
      begin
        if QuoteActive then begin
          ParsedLine := ParsedLine + Line[j];
          if (Line[j] = #39) and ((j = 1) or (Line[j - 1] <> '\')) then QuoteActive := False;
        end else if DoubleQuoteActive then begin
          ParsedLine := ParsedLine + Line[j];
          if (Line[j] = '"') and ((j = 1) or (Line[j - 1] <> '\')) then DoubleQuoteActive := False;
        end else if StarCommentActive then begin
          if (Line[j] = '/') and (j > 1) and (Line[j - 1] = '*') then StarCommentActive := False;
        end else if not SlashCommentActive then begin
          case Line[j] of
            #39: begin QuoteActive := True; ParsedLine := ParsedLine + Line[j]; end;
            '"': begin DoubleQuoteActive := True; ParsedLine := ParsedLine + Line[j]; end;
            '/': if j <> Length(Line) then
                   case Line[j + 1] of
                     '*': begin StarCommentActive := True; Inc(j); end;
                     '/': SlashCommentActive := True;
                   else ParsedLine := ParsedLine + Line[j]; end
                 else ParsedLine := ParsedLine + Line[j];
          else ParsedLine := ParsedLine + Line[j];
          end;
        end;
        Inc(j);
      end;
      if (ParsedLine <> '') and (ParsedLine[1] = '#') then begin
        PragmaBackup := CompressWhiteSpace(Copy(ParsedLine, 2));
        if StartsStr('pragma backup ', PragmaBackup) then begin
          FileSpec := Trim(Copy(ParsedLine, Pos('backup', ParsedLine) + 6));
          if FileSpec = '' then Continue;
          FileSpec := GetQuotedToken(FileSpec);
          if FileSpec <> '' then IncludeLines.Add(FileSpec);
        end;
      end;
    end;
  end;

  procedure ProcessRecursiveIncludeLine(const Line: string; const ContextFileName: string);
  var
    FileSpec: string;
    RawDir, RawMask, AbsDir: string;
  begin
    FileSpec := Trim(Line);
    if FileSpec = '' then Exit;
    if IsIgnoredExtension(FileSpec) then Exit;

    RawMask := ExtractFileName(FileSpec);
    RawDir := ExtractFilePath(FileSpec);
    if RawMask = '' then RawMask := '*.*';

    // Build Absolute Path relative to current file
    if (Pos(':', RawDir) > 0) or (Pos('\\', RawDir) = 1) then
      AbsDir := RawDir
    else
      AbsDir := IncludeTrailingPathDelimiter(CurrentFileDir) + RawDir;

    AbsDir := ExpandFileName(AbsDir);
    AbsDir := IncludeTrailingPathDelimiter(AbsDir);

    if DirectoryExists(AbsDir) then
       BackupForm.SearchAndAddFilesRecursive(AbsDir, RawMask);
  end;

  procedure ProcessIncludeLine(const Line: string; const ContextFileName: string);
  var
    FileSpec: string;
    ResolvedPath, AbsPath: string;
  begin
    FileSpec := Trim(Line);
    if FileSpec = '' then Exit;
    if IsIgnoredExtension(FileSpec) then Exit;

    if SameText(FileSpec, '*.res') then 
      FileSpec := ChangeFileExt(ContextFileName, '.res')
    else if SameText(FileSpec, '*.dcr') then 
      FileSpec := ChangeFileExt(ContextFileName, '.dcr');

    if FileNameHasWildcards(FileSpec) then
    begin
      if (Pos(':', FileSpec) > 0) or (Pos('\\', FileSpec) = 1) then
        AbsPath := FileSpec
      else
        AbsPath := IncludeTrailingPathDelimiter(CurrentFileDir) + FileSpec;
      
      if ExtractFilePath(FileSpec) = '' then
      begin
         BackupForm.SearchAndAddFiles(CurrentFileDir, FileSpec);
         if IsProject and (BackupForm.FProjectGlobalMasks.IndexOf(FileSpec) = -1) then
           BackupForm.FProjectGlobalMasks.Add(FileSpec);
      end
      else
      begin
         AbsPath := IncludeTrailingPathDelimiter(CurrentFileDir) + ExtractFilePath(FileSpec);
         AbsPath := ExpandFileName(AbsPath);
         BackupForm.SearchAndAddFiles(AbsPath, ExtractFileName(FileSpec));
      end;
    end
    else
    begin
      ResolvedPath := ExpandFileName(FileSpec);
      if BackupForm.IsFileExcluded(ResolvedPath) then Exit;
      if FileExists(ResolvedPath) then
      begin
        AddFileToList(ResolvedPath, FoundFileList);
        if IsDprOrPas(ResolvedPath) or IsCpp(ResolvedPath) then
          ScanForIncludesAndAdd(ResolvedPath, BackupForm, FoundFileList, NotFoundFileList);
      end
      else
        AddFileToList(FileSpec + ItemSeparatorChar + ContextFileName, NotFoundFileList);
    end;
  end;

var
  CurrentLineNum: Integer;
  FileLines: TStringList;
  IncludeLines: TStringList;
  RecursiveIncludeLines: TStringList;
  OriginalPath: string;
begin
  FileLines := nil;
  IncludeLines := nil;
  RecursiveIncludeLines := nil;
  OriginalPath := GetCurrentDir;
  
  IsProject := IsProjectFile(FileName);
  CurrentFileDir := IncludeTrailingPathDelimiter(ExtractFilePath(ExpandFileName(FileName)));

  try
    if DirectoryExists(CurrentFileDir) then
      SafeChangeDirectory(CurrentFileDir);
      
    FileLines := TStringList.Create;
    IncludeLines := TStringList.Create;
    RecursiveIncludeLines := TStringList.Create;
    
    try
      if not FileNameHasWildcards(FileName) and FileExists(FileName) then
        FileLines.LoadFromFile(FileName)
      else begin
        AddFileToList(FileName + ItemSeparatorChar + FileName, NotFoundFileList);
        Exit;
      end;
    except
      AddFileToList(FileName + ItemSeparatorChar + FileName, NotFoundFileList);
      Exit;
    end;
    
    if IsProject then
    begin
      CollectProjectDirectories(FileLines, CurrentFileDir);
      if Assigned(BackupForm.FProjectFolders) and (BackupForm.FProjectFolders.IndexOf(CurrentFileDir) = -1) then
         BackupForm.FProjectFolders.Add(CurrentFileDir);
    end;
    
    if IsDprOrPas(FileName) then
      ParseFileForDirectives(FileLines, IncludeLines, RecursiveIncludeLines)
    else if IsCpp(FileName) then
      GetCppPragmaBackupLines(FileLines, IncludeLines);

    for CurrentLineNum := 0 to RecursiveIncludeLines.Count - 1 do
      ProcessRecursiveIncludeLine(RecursiveIncludeLines[CurrentLineNum], FileName);

    for CurrentLineNum := 0 to IncludeLines.Count - 1 do
      ProcessIncludeLine(IncludeLines[CurrentLineNum], FileName);
  finally
    FreeAndNil(FileLines);
    FreeAndNil(IncludeLines);
    FreeAndNil(RecursiveIncludeLines);
    SafeChangeDirectory(OriginalPath);
  end;
end;

{ TfmBackup }

constructor TfmBackup.Create(AOwner: TComponent);
begin
  inherited;
  FLibraryPath := TStringList.Create;
  FLibraryPath.Duplicates := dupIgnore;
  FFilesFoundNowhere := TStringList.Create;
  
  FExcludePatterns := TStringList.Create;
  FProjectGlobalMasks := TStringList.Create;
  FProjectFolders := TStringList.Create;
  FProjectFolders.Duplicates := dupIgnore;
  
  FFileList := TStringList.Create;
  FFileList.Duplicates := dupIgnore;
  FFileList.Sorted := True;

  FZipEncrypted := False;
  FHaveCollectedFiles := False;

  TWinControl_ActivateDropFiles(tvFiles, tvFilesOnFilesDropped);
  
  // Initialize ListView
  InitializeListView;
  
  InitDpiScaler;
  LoadSettings;
end;

destructor TfmBackup.Destroy;
var
  i: Integer;
begin
  for i := Low(FFileSearchThreads) to High(FFileSearchThreads) do
    FFileSearchThreads[i].Free;
  SetLength(FFileSearchThreads, 0); 

  FreeAndNil(FFilesFoundNowhere);
  FreeAndNil(FLibraryPath);
  FreeAndNil(FExcludePatterns);
  FreeAndNil(FProjectGlobalMasks);
  FreeAndNil(FProjectFolders);
  FreeAndNil(FFileList);

  inherited Destroy;
end;

function TfmBackup.IsFileExcluded(const FileName: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  if not Assigned(FExcludePatterns) then Exit;
  for i := 0 to FExcludePatterns.Count - 1 do
  begin
    if MatchesMask(FileName, FExcludePatterns[i]) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

procedure TfmBackup.SearchAndAddFiles(const SearchPath, FileMask: string);
var
  SearchRec: TSearchRec;
  FoundFile: string;
  FullSearchMask: string;
begin
  if Trim(SearchPath) = '' then Exit;
  FullSearchMask := IncludeTrailingPathDelimiter(SearchPath) + FileMask;
  if FindFirst(FullSearchMask, faAnyFile, SearchRec) = 0 then
  try
    repeat
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') and
         ((SearchRec.Attr and faDirectory) = 0) then
      begin
        FoundFile := IncludeTrailingPathDelimiter(SearchPath) + SearchRec.Name;
        if not IsFileExcluded(FoundFile) then
          AddBackupFile(FoundFile);
      end;
    until FindNext(SearchRec) <> 0;
  finally
    SysUtils.FindClose(SearchRec);
  end;
end;

procedure TfmBackup.SearchAndAddFilesRecursive(const SearchPath, FileMask: string);
var
  SearchRec: TSearchRec;
  CurrentPath: string;
begin
  CurrentPath := IncludeTrailingPathDelimiter(SearchPath);
  if not DirectoryExists(CurrentPath) then Exit;
  SearchAndAddFiles(CurrentPath, FileMask);
  if FindFirst(CurrentPath + '*.*', faAnyFile, SearchRec) = 0 then
  try
    repeat
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') and
         ((SearchRec.Attr and faDirectory) <> 0) then
      begin
        SearchAndAddFilesRecursive(CurrentPath + SearchRec.Name, FileMask);
      end;
    until FindNext(SearchRec) <> 0;
  finally
    SysUtils.FindClose(SearchRec);
  end;
end;

procedure TfmBackup.ProcessGlobalBackupMasks;
var
  iFolder, iMask: Integer;
  CurrentFolder, CurrentMask: string;
begin
  if (FProjectFolders.Count = 0) or (FProjectGlobalMasks.Count = 0) then Exit;
  for iFolder := 0 to FProjectFolders.Count - 1 do
  begin
    CurrentFolder := IncludeTrailingPathDelimiter(FProjectFolders[iFolder]);
    if not DirectoryExists(CurrentFolder) then Continue;
    for iMask := 0 to FProjectGlobalMasks.Count - 1 do
    begin
      CurrentMask := FProjectGlobalMasks[iMask];
      SearchAndAddFiles(CurrentFolder, CurrentMask);
    end;
  end;
end;

function TfmBackup.ListFiles(const FileName, UnitName, FormName: string): Boolean;
var
  TempFileName: string;
begin
  Result := True;
  if IsFileExcluded(FileName) then Exit;
  TempFileName := ExtractUpperFileExt(FileName);
  if (TempFileName = '.DCP') or (TempFileName = '.BPI') or (TempFileName = '.DLL') or (TempFileName = '.AQT') then
    Exit;

  AddBackupFile(FileName);

  if (TempFileName = '.DPR') or (TempFileName = '.DPK') then
  begin
    // Add .res and .rc for project files if they exist
    if FileExists(ChangeFileExt(FileName, '.res')) then
      AddBackupFile(ChangeFileExt(FileName, '.res'));
    if FileExists(ChangeFileExt(FileName, '.rc')) then
      AddBackupFile(ChangeFileExt(FileName, '.rc'));
  end;

  if (TempFileName = '.CPP') then
  begin
    AddBackupFile(ChangeFileExt(FileName, '.h')); 
    AddBackupFile(ChangeFileExt(FileName, '.bpr')); 
    AddBackupFile(ChangeFileExt(FileName, '.bpk')); 
  end;

  AddBackupFile(ChangeFileExt(FileName, '.dfm')); 
  AddBackupFile(ChangeFileExt(FileName, '.xfm')); 
  AddBackupFile(ChangeFileExt(FileName, '.nfm')); 
  AddBackupFile(ChangeFileExt(FileName, '.fmx')); 
  AddBackupFile(ChangeFileExt(FileName, '.todo')); 

  if FBackupExpert.DoBackupIncludedFiles and (IsDprOrPas(FileName) or IsCpp(FileName)) then
  begin
    ScanForIncludesAndAdd(FileName, Self, FFilesFoundNowhere, FFilesFoundNowhere); 
  end;
  Result := not FDoAbortCollectingFiles;
end;

procedure TfmBackup.LocateFileOnPathAndAdd(FilesNotFound: TStrings);
var
  LastFileChecked: string;
  LastFileCheckResult: boolean;

  function DoesFileExist(const Filename: string): boolean;
  begin
    if Filename <> LastFileChecked then begin
      LastFileCheckResult := FileExists(Filename);
      LastFileChecked := Filename;
    end;
    Result := LastFileCheckResult;
  end;

  procedure SplitUpEntry(const Entry: string; var IncludedFile, RefererFile: string);
  var
    SeparatorPos: Integer;
  begin
    SeparatorPos := Pos(ItemSeparatorChar, Entry);
    IncludedFile := Entry;
    if SeparatorPos > 0 then
    begin
      System.Delete(IncludedFile, SeparatorPos, Length(Entry));
      RefererFile := Copy(Entry, SeparatorPos + 1);
    end
    else
      RefererFile := '';
  end;

var
  i, j: Integer;
  IncludedFile: string;
  RefererFile: string;
  FileLocation: string;
begin
  LastFileChecked := '';
  for i := 0 to FLibraryPath.Count - 1 do
  begin
    j := FilesNotFound.Count - 1;
    while j >= 0 do
    begin
      SplitUpEntry(FilesNotFound[j], IncludedFile, RefererFile);
      FileLocation := FLibraryPath[i] + IncludedFile;
      if not IsFileExcluded(FileLocation) and DoesFileExist(FileLocation) then
      begin
        FilesNotFound.Delete(j);
        AddBackupFile(FileLocation);
      end;
      Dec(j);
    end;
  end;
  for j := 0 to FilesNotFound.Count - 1 do
  begin
    SplitUpEntry(FilesNotFound[j], IncludedFile, RefererFile);
    FilesNotFound[j] := Format('%s (%s)', [IncludedFile, RefererFile]);
  end;
end;

procedure TfmBackup.DoCollectFiles;
var
  ProjectGroupFileName: string;
  IProjectGroup: IOTAProjectGroup;
  i: Integer;
begin
  if FCurrentBackupScope = bsActiveProject then
    DoCollectFilesFromSingleProject(Ota.GetCurrentProject)
  else begin
    IProjectGroup := Ota.GetProjectGroup;
    if IProjectGroup = nil then Exit;
    ProjectGroupFileName := Ota.GetProjectGroupFileName;
    if not IsFileExcluded(ProjectGroupFileName) then
      AddBackupFile(ProjectGroupFileName);
    for i := 0 to IProjectGroup.ProjectCount - 1 do
      DoCollectFilesFromSingleProject(IProjectGroup.Projects[i]);
  end;
end;

procedure TfmBackup.DoCollectFilesFromSingleProject(IProject: IOTAProject);
var
  i: Integer;
  IModuleInfo: IOTAModuleInfo;
  IEditor: IOTAEditor;
  FileName: string;
begin
  if IProject = nil then Exit;
  Assert(Assigned(FProgressForm));
  GxOtaGetEffectiveLibraryPath(FLibraryPath, IProject);
  ListFiles(IProject.FileName, '', '');
  for i := 0 to IProject.GetModuleFileCount - 1 do
  begin
    IEditor := IProject.GetModuleFileEditor(i);
    Assert(IEditor <> nil);
    FileName := IEditor.FileName;
    if FileName <> '' then
      ListFiles(IEditor.FileName, '', '');
  end;
  for i := 0 to IProject.GetModuleCount - 1 do
  begin
    IModuleInfo := IProject.GetModule(i);
    Assert(IModuleInfo <> nil);
    FileName := IModuleInfo.FileName;
    if FileName <> '' then
      ListFiles(IModuleInfo.FileName, '', IModuleInfo.FormName);
  end;
  ProcessGlobalBackupMasks;
end;

procedure RemoveDuplicates(_NotFound: TStringList);
var
  i: Integer;
  s: string;
  p: Integer;
  j: Integer;
  cnt: Integer;
begin
  i := 0;
  while i < _NotFound.Count do begin
    s := _NotFound[i];
    p := Pos('|', s);
    if p > 1 then begin
      s := LeftStr(s, p - 1);
      cnt := 0;
      for j := _NotFound.Count - 1 downto i + 1 do begin
        if StartsText(s, _NotFound[j]) then begin
          _NotFound.Delete(j);
          Inc(cnt);
        end;
      end;
      if cnt > 0 then
        _NotFound[i] := _NotFound[i] + Format(' and %d more', [cnt]);
    end;
    Inc(i);
  end;
end;

procedure TfmBackup.CollectFilesForBackup;
resourcestring
  SCollectingBackupFiles = 'Collecting Files...';
var
  Cursor: IInterface;
  ProjectName: string;
begin
  tvFiles.Items.Clear;
  FFileList.Clear;
  FTotalSize := 0;
  FBaseProjectDir := '';

  FFilesFoundNowhere.Clear;
  FExcludePatterns.Clear; 
  FProjectGlobalMasks.Clear;
  FProjectFolders.Clear;
  
  if FCurrentBackupScope = bsActiveProject then
    ProjectName := Ota.GetCurrentProjectFileName
  else
    ProjectName := Ota.GetProjectGroupFileName;

  if ProjectName <> '' then
    FBaseProjectDir := IncludeTrailingPathDelimiter(ExtractFilePath(ProjectName));

  Assert(FProgressForm = nil);
  FProgressForm := TfmProgress.Create(nil);
  try
    FProgressForm.Caption := SCollectingBackupFiles;
    Self.Enabled := False;
    FProgressForm.Show;
    FProgressForm.Progress.Max := 40;
    FDoAbortCollectingFiles := False;
    TCursor_TempHourglass;
    tvFiles.Items.BeginUpdate;
    try
      DoCollectFiles;
    finally
      tvFiles.Items.EndUpdate;
    end;

    if FBackupExpert.FBackupInc then
    begin
      if FBackupExpert.FollowLibraryPath and (FFilesFoundNowhere.Count > 0) then
        LocateFileOnPathAndAdd(FFilesFoundNowhere);
      RemoveDuplicates(FFilesFoundNowhere);
      FFilesFoundNowhere.Sort;
      FFilesFoundNowhere.Text := StringReplace(FFilesFoundNowhere.Text,
        '|', ' from ', [rfReplaceAll]);
      Self.Enabled := True;
      FreeAndNil(FProgressForm);
      if FFilesFoundNowhere.Count > 0 then begin
        Cursor := nil;
        TfmBackupNotFound.Execute(Self, FFilesFoundNowhere, FBackupExpert.FollowLibraryPath);
      end;
    end;
  finally
    Self.Enabled := True;
    FreeAndNil(FProgressForm);
  end;
  AfterFileListChange;
end;

procedure TfmBackup.PerformBackup(const Path, FileName: string);
var
  DestFile: string;
  FileList: TStringList;
begin
  Assert(FProgressForm = nil);
  FZipComponent := nil;
  FileList := TStringList.Create;
  FProgressForm := TfmProgress.Create(nil);
  try
    DestFile := Path + FileName;
    FZipComponent := TGXZipper.Create(DestFile, fmCreate or fmShareDenyWrite);
    TCursor_TempHourglass;
    
    // Extract Files from ListView (use FFileList which contains all file paths)
    FileList.Assign(FFileList);

    FProgressForm.Progress.Position := 0;
    Self.Enabled := False;
    FProgressForm.Show;
    Application.ProcessMessages;
    FProgressForm.Progress.Max := 100;
    FZipComponent.IncludePath := FBackupExpert.DoIncludeDirInfoInZip;
    if FZipEncrypted then
      FZipComponent.Password := AnsiString(FZipPassword);
    FZipComponent.OnProcessItemFailure := FileFailure;
    FZipComponent.OnArchiveProgress := AbbreviaProgress;
    
    FZipComponent.AddFiles(FileList);
    
    FZipComponent.Save;
  finally
    FileList.Free;
    Self.Enabled := True;
    FreeAndNil(FProgressForm);
    FreeAndNil(FZipComponent);
  end;
end;

procedure TfmBackup.btnBackupClick(Sender: TObject);
resourcestring
  SFileExists = 'File %s already exists, do you want to overwrite this file?';
  SDirectoryDoesNotExist = 'Directory %s does not exist, do you want to create this directory?';
const
  ZipExtension = '.zip'; 
var
  CurrentZipFileName: string;
  ZipFilePath: string;
  i: Integer;
begin
  if FBackupExpert.BackupType = btFile then
  begin
    CurrentZipFileName := FLastZipFile;
    if not ShowSaveDialog('Backup As', 'zip', CurrentZipFileName) then Exit;
    if ExtractFileExt(CurrentZipFileName) = '' then
      CurrentZipFileName := CurrentZipFileName + ZipExtension;
    if FileExists(CurrentZipFileName) then
    begin
      if MessageDlg(Format(SFileExists, [CurrentZipFileName]), mtConfirmation, [mbYes, mbNo], 0) = mrNo then Exit;
      DeleteFile(CurrentZipFileName);
    end;
  end
  else
  begin
    CurrentZipFileName := ReplaceStrings(FBackupExpert.BackupDir, True);
    CurrentZipFileName := TFileSystem.MakeValidFilename(CurrentZipFileName);
    ZipFilePath := ExtractFilePath(CurrentZipFileName);
    CurrentZipFileName := ExtractFileName(CurrentZipFileName);
    if not DirectoryExists(ZipFilePath) then
    begin
      if MessageDlg(Format(SDirectoryDoesNotExist, [ZipFilePath]), mtConfirmation, [mbYes, mbNo], 0) = mrNo then Exit
      else ForceDirectories(ZipFilePath);
    end;
    if ExtractUpperFileExt(CurrentZipFileName) = '.ZIP' then
      CurrentZipFileName := ChangeFileExt(CurrentZipFileName, '');
    if FileExists(ZipFilePath + CurrentZipFileName + ZipExtension) then
    begin
      i := 1;
      while i < 999 do
      begin
        if not FileExists(ZipFilePath + CurrentZipFileName + IntToStr(i) + ZipExtension) then
        begin
          CurrentZipFileName := CurrentZipFileName + IntToStr(i) + ZipExtension;
          Break;
        end;
        Inc(i);
      end;
    end
    else
      CurrentZipFileName := CurrentZipFileName + ZipExtension;
  end;
  PerformBackup(ZipFilePath, CurrentZipFileName);
  FLastZipFile := CurrentZipFileName;
  SaveSettings;
  ModalResult := mrOk;
end;

procedure TfmBackup.btnAddClick(Sender: TObject);
var
  i: Integer;
  Files: TStrings;
begin
  Files := TStringList.Create;
  try
    if ShowOpenDialog('Add to Backup', '', Files,
      'Delphi Files (*.pas;*.dfm;*.xfm;*.dpr;*.dpk;*.bpg;*.res)|*.pas;*.dfm;*.xfm;*.dpr;*.dpk;*.bpg;*.res') then
      for i := 0 to Files.Count - 1 do
        AddBackupFile(Files[i]);
    AfterFileListChange;
  finally
    FreeAndNil(Files);
  end;
end;

procedure TfmBackup.btnRemoveClick(Sender: TObject);
var
  SelectedItem: TListItem;
  FilePath: string;
  FileSize: Int64;
begin
  SelectedItem := tvFiles.Selected;
  if SelectedItem <> nil then
  begin
    // Get the full file path from FFileList
    if SelectedItem.Index < FFileList.Count then
    begin
      FilePath := FFileList[SelectedItem.Index];
      FileSize := GetFileSize(FilePath);
      FTotalSize := FTotalSize - FileSize;
      FFileList.Delete(SelectedItem.Index);
    end;
    
    SelectedItem.Delete;
    UpdateSizeColumn;
    AfterFileListChange;
  end;
end;

procedure TfmBackup.FormDestroy(Sender: TObject);
begin
  FDoAbortCollectingFiles := True;
end;

procedure TfmBackup.btnHelpClick(Sender: TObject);
begin
  GxContextHelp(Self, 10);
end;

procedure TfmBackup.FormActivate(Sender: TObject);
begin
  if not FHaveCollectedFiles then
  begin
    FHaveCollectedFiles := True;
    CollectFilesForBackup;
  end;
end;

procedure TfmBackup.btnOptionsClick(Sender: TObject);
var
  frm: TfmBackupOptions;
  RefreshRequired: Boolean;
begin
  frm := TfmBackupOptions.Create(nil);
  try
    frm.cbPassword.Checked := FZipEncrypted;
    frm.edPassword.Text := FZipPassword;
    frm.cbSearchLibraryPath.Checked := FBackupExpert.FollowLibraryPath;
    frm.rgScope.ItemIndex := Ord(FCurrentBackupScope);
    pnlButtons.Enabled := False;
    pnlFiles.Enabled := False;
    if frm.ShowModal = mrOk then
    begin
      FZipPassword := frm.edPassword.Text;
      FZipEncrypted := frm.cbPassword.Checked;
      RefreshRequired := not (FCurrentBackupScope = TBackupScope(frm.rgScope.ItemIndex));
      FCurrentBackupScope := TBackupScope(frm.rgScope.ItemIndex);
      RefreshRequired := RefreshRequired or (not (FBackupExpert.FFollowLibraryPath = frm.cbSearchLibraryPath.Checked));
      FBackupExpert.FFollowLibraryPath := frm.cbSearchLibraryPath.Checked;
      if RefreshRequired then
        CollectFilesForBackup;
    end;
  finally
    pnlButtons.Enabled := True;
    pnlFiles.Enabled := True;
    FreeAndNil(frm);
  end;
end;

type
  TGxContainsDirectoriesRecursiveMessage = class(TGxMsgBoxAdaptor)
  protected
    function GetMessage: string; override;
    function GetButtons: TMsgDlgButtons; override;
    function GetDefaultButton: TMsgDlgBtn; override;
  end;

function TGxContainsDirectoriesRecursiveMessage.GetButtons: TMsgDlgButtons;
begin
  Result := [mbYes, mbNo, mbCancel];
end;

function TGxContainsDirectoriesRecursiveMessage.GetDefaultButton: TMsgDlgBtn;
begin
  Result := mbCancel;
end;

function TGxContainsDirectoriesRecursiveMessage.GetMessage: string;
resourcestring
  SDroppedFilesContainedDirectories =
    'The files you dropped contained at least one directory.  ' +
    'Do you want to recursively add all files within these directories?';
begin
  Result := SDroppedFilesContainedDirectories + #13#10 + FData;
end;

type
  TGxContainsDirectoriesMessage = class(TGxMsgBoxAdaptor)
  protected
    function GetMessage: string; override;
    function GetButtons: TMsgDlgButtons; override;
    function GetDefaultButton: TMsgDlgBtn; override;
  end;

function TGxContainsDirectoriesMessage.GetButtons: TMsgDlgButtons;
begin
  Result := [mbYes, mbNo, mbCancel];
end;

function TGxContainsDirectoriesMessage.GetDefaultButton: TMsgDlgBtn;
begin
  Result := mbCancel;
end;

function TGxContainsDirectoriesMessage.GetMessage: string;
resourcestring
  SDroppedFilesContainedDirectoriesMsg =
    'The files you dropped contained at least one directory.  ' +
    'Do you want to add all files within these directories?';
begin
  Result := SDroppedFilesContainedDirectoriesMsg + #13#10 + FData;
end;

type
  TExtFindFileThread = class(TFileFindThread)
  private
    FListView: TListView;
    FIgnoreBackupFiles: Boolean;
    FExcludePatterns: TStringList;
  public
    constructor Create(_ListView: TListView; _IgnoreBackupFiles: boolean; _ExcludePatterns: TStringList);
    procedure SyncFindComplete;
  end;

constructor TExtFindFileThread.Create(_ListView: TListView; _IgnoreBackupFiles: boolean; _ExcludePatterns: TStringList);
begin
  inherited Create;
  FListView := _ListView;
  FIgnoreBackupFiles := _IgnoreBackupFiles;
  FExcludePatterns := _ExcludePatterns;
end;

procedure TExtFindFileThread.SyncFindComplete;

  function IsBackupFile(const Filename: string): boolean;
  var
    Ext: string;
  begin
    Ext := ExtractFileExt(Filename);
    Result := (Copy(Ext, 1, 2) = '.~') or (Copy(Ext, Length(Ext), 1) = '~');
  end;

  function IsExcluded(const Filename: string): boolean;
  var
    k: Integer;
  begin
    Result := False;
    if not Assigned(FExcludePatterns) then Exit;
    for k := 0 to FExcludePatterns.Count - 1 do
      if MatchesMask(Filename, FExcludePatterns[k]) then
      begin
        Result := True;
        Exit;
      end;
  end;

  procedure SafeAddToListView(const FilePath: string);
  begin
    TfmBackup(FListView.Owner).AddFileToListView(FilePath);
  end;

var
  i: Integer;
  fn: string;
begin
  LockResults;
  try
    FListView.Items.BeginUpdate;
    try
      for i := 0 to Results.Count - 1 do begin
        fn := Results[i];
        if not DirectoryExists(fn) then
          if (not FIgnoreBackupFiles or not IsBackupFile(fn)) then
            if not IsExcluded(fn) then 
               SafeAddToListView(fn);
      end;
    finally
      FListView.Items.EndUpdate;
    end;
  finally
    ReleaseResults;
  end;
end;

procedure TfmBackup.AddFilesInDirs(_Dirs: TStrings);
var
  DirThread: TExtFindFileThread;
  Idx: Integer;
begin
  DirThread := TExtFindFileThread.Create(tvFiles, FBackupExpert.FIgnoreBackupFiles, FExcludePatterns);
  try
    DirThread.FileMasks.Add(AllFilesWildCard);
    if FBackupExpert.FAddDirsRecursively and FBackupExpert.FIgnoreHistoryDir then
      DirThread.AddDelphiDirsToIgnore;
    if FBackupExpert.FAddDirsRecursively and FBackupExpert.FIgnoreScmDirs then
      DirThread.AddSCMDirsToIgnore;
    if FBackupExpert.FAddDirsRecursively then
      DirThread.RecursiveSearchDirs.AddStrings(_Dirs)
    else
      DirThread.SearchDirs.AddStrings(_Dirs);
    DirThread.OnFindComplete := DirThread.SyncFindComplete;
    DirThread.StartFind;
    Idx :=  Length(FFileSearchThreads);
    SetLength(FFileSearchThreads, Idx + 1);
    FFileSearchThreads[Idx] := DirThread;
    DirThread := nil;
  finally
    FreeAndNil(DirThread);
  end;
end;

procedure TfmBackup.tvFilesOnFilesDropped(_Sender: TObject; _Files: TStrings);
var
  i: Integer;
  fn: string;
  Dirs: TStringList;
begin
  Dirs := TStringList.Create;
  try
  for i := _Files.Count - 1 downto 0 do begin
    fn := _Files[i];
    if DirectoryExists(fn) then begin
      Dirs.Add(fn);
      _Files.Delete(i);
    end;
  end;
  if Dirs.Count > 0 then begin
    if FBackupExpert.FAddDirsRecursively then begin
      case ShowGxMessageBox(TGxContainsDirectoriesRecursiveMessage, Dirs.Text) of
        mrYes: AddFilesInDirs(Dirs);
        mrNo: ;
      else Exit;
      end;
    end else begin
      case ShowGxMessageBox(TGxContainsDirectoriesMessage, Dirs.Text) of
        mrYes: AddFilesInDirs(Dirs);
        mrNo: ;
      else Exit;
      end;
    end;
  end;
  finally
    FreeAndNil(Dirs);
  end;
  for i := 0 to _Files.Count - 1 do begin
    if not IsFileExcluded(_Files[i]) then
      AddBackupFile(_Files[i]);
  end;
  AfterFileListChange;
end;

procedure TfmBackup.tvFilesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_DELETE) and (tvFiles.Selected <> nil) then
  begin
    btnRemove.Click;
    Key := 0;
  end;
end;

procedure TfmBackup.AddBackupFile(const FileName: string);
begin
  IncrementProgress;
  if IsFileExcluded(FileName) then Exit;
  if FileExists(FileName) then
    AddFileToListView(FileName);
end;

procedure TfmBackup.AfterFileListChange;
resourcestring
  SBackupFormCaptionUnknown  = 'Backup Project (%d+ Files)';
  SBackupFormCaptionPlural   = 'Backup Project (%d Files)';
  SBackupFormCaptionSingular = 'Backup Project (1 File)';
var
  FileCount: Integer;
begin
  FileCount := tvFiles.Items.Count;

  if FileCount = 1 then
    Caption := SBackupFormCaptionSingular
  else
    Caption := Format(SBackupFormCaptionPlural, [FileCount]);
  
  btnBackup.Enabled := (FileCount > 0);
end;

procedure TfmBackup.SaveSettings;
var
  Settings: IExpertSettings;
begin
  Settings := TBackupProjectExpert.GetSettings;
  Settings.SaveForm('Window', Self, [fsSize]);
  Settings.WriteString('LastZipDir', ExtractFilePath(FLastZipFile));
end;

procedure TfmBackup.LoadSettings;
var
  Settings: IExpertSettings;
  fn: string;
begin
  Settings := TBackupProjectExpert.GetSettings;
  Settings.LoadForm('Window', Self, [fsSize]);
  if FCurrentBackupScope = bsActiveProject then
    fn := ChangeFileExt(ExtractFileName(Ota.GetCurrentProjectFileName), '')
  else
    fn := ChangeFileExt(ExtractFileName(Ota.GetProjectGroupFileName), '');
  FLastZipFile := Settings.ReadString('LastZipDir', '');
  if FLastZipFile <> '' then
    FLastZipFile := IncludeTrailingPathDelimiter(FLastZipFile) + fn
  else
    FLastZipFile := fn;
  EnsureFormVisible(Self);
end;

procedure TfmBackup.IncrementProgress;
begin
  if not Assigned(FProgressForm) then Exit;
  with FProgressForm.Progress do
    if Position = Max then Position := 0 else Position := Position + 1;
end;

procedure TfmBackup.FileFailure(Sender: TObject; Item: TAbArchiveItem;
  ProcessType: TAbProcessType; ErrorClass: TAbErrorClass; ErrorCode: Integer);
var
  Msg: string;
begin
  Msg := '';
  if ExceptObject is Exception then
    Msg := Exception(ExceptObject).Message;
  case ErrorClass of
    ecAbbrevia: begin
      if ErrorCode = AbDuplicateName then Exit
      else Msg := AbStrRes(ErrorCode) + ' ' + Msg;
    end;
    ecInOutError: Msg := Format('EInOutError (%d)', [ErrorCode]) + ' ' + Msg;
    ecFilerError: Msg := 'EFilerError' + ' ' + Msg;
    ecFileCreateError: Msg := 'EFCreateError' + ' ' + Msg;
    ecFileOpenError: Msg := 'EFOpenError' + ' ' + Msg;
  end;
  Msg := Trim(Msg);
  MessageDlg('Error processing file: ' + Item.FileName + '  ' + Msg, mtError, [mbOK], 0);
end;

procedure TfmBackup.AbbreviaProgress(Sender: TObject; Progress: Byte; var Abort: Boolean);
begin
  FProgressForm.Progress.Position := Progress;
end;

{ TBackupProjectExpert }

constructor TBackupProjectExpert.Create;
begin
  inherited Create;
  FBackupType := btFile;
  FBackupDir := AddSlash('%PROJECTDIR%') + '%PROJECTNAME%'; 
  FIncludeDir := True;
  FBackupScope := bsActiveProject;
  FAddDirsRecursively := True;
  FIgnoreHistoryDir := True;
  FIgnoreScmDirs := True;
  FIgnoreBackupFiles := True;
end;

function TBackupProjectExpert.GetActionCaption: string;
resourcestring
  SMenuCaption = '&Backup Project...';
begin
  Result := SMenuCaption;
end;

class function TBackupProjectExpert.GetName: string;
begin
  Result := 'BackupProject'; 
end;

procedure TBackupProjectExpert.Execute(Sender: TObject);
var
  frm: TfmBackup;
begin
  frm := TfmBackup.Create(nil);
  try
    SetFormIcon(frm);
    frm.FBackupExpert := Self;
    frm.FCurrentBackupScope := BackupScope;
    frm.ShowModal;
    IncCallCount;
  finally
    FreeAndNil(frm);
  end;
end;

procedure TBackupProjectExpert.Configure(_Owner: TWinControl);
var
  frm: TfmBackupConfig;
begin
  frm := TfmBackupConfig.Create(_Owner);
  try
    TForm_CenterOn(frm, _Owner);
    frm.cbBackupInc.Checked := FBackupInc;
    frm.cbIncludeDir.Checked := FIncludeDir;
    frm.rbBackupAskForFile.Checked := (FBackupType = btFile);
    frm.rbBackupToDirectory.Checked := (FBackupType = btDir);
    frm.edBackupDir.Text := FBackupDir;
    frm.rgDefaultScope.ItemIndex := Ord(FBackupScope);
    frm.cbSearchOnLibraryPath.Checked := FFollowLibraryPath;
    frm.cbAddRecursively.Checked := FAddDirsRecursively;
    frm.cbIgnoreHistoryDir.Checked := FIgnoreHistoryDir;
    frm.cbIgnoreScmDirs.Checked := FIgnoreScmDirs;
    frm.cbIgnoreBackupFiles.Checked := FIgnoreBackupFiles;

    if frm.ShowModal = mrOk then
    begin
      if frm.rbBackupAskForFile.Checked then FBackupType := btFile else FBackupType := btDir;
      FBackupDir := frm.edBackupDir.Text;
      FBackupInc := frm.cbBackupInc.Checked;
      FIncludeDir := frm.cbIncludeDir.Checked;
      FBackupScope := TBackupScope(frm.rgDefaultScope.ItemIndex);
      FFollowLibraryPath := frm.cbSearchOnLibraryPath.Checked;
      FAddDirsRecursively := frm.cbAddRecursively.Checked;
      FIgnoreHistoryDir := frm.cbIgnoreHistoryDir.Checked;
      FIgnoreScmDirs := frm.cbIgnoreScmDirs.Checked;
      FIgnoreBackupFiles := frm.cbIgnoreBackupFiles.Checked;
    end;
  finally
    FreeAndNil(frm);
  end;
end;

procedure TBackupProjectExpert.InternalSaveSettings(_Settings: IExpertSettings);
begin
  inherited InternalSaveSettings(_Settings);
  _Settings.WriteBool('Include', FBackupInc);
  _Settings.WriteEnumerated('Type', TypeInfo(TBackupType), Ord(FBackupType));
  _Settings.WriteString('Directory', FBackupDir);
  _Settings.WriteBool('IncludeDir', FIncludeDir);
  _Settings.WriteEnumerated('BackupScope', TypeInfo(TBackupScope), Ord(FBackupScope));
  _Settings.WriteBool('FollowLibraryPath', FFollowLibraryPath);
  _Settings.WriteBool('AddDirsRecursively', FAddDirsRecursively);
  _Settings.WriteBool('IgnoreHistoryDir', FIgnoreHistoryDir);
  _Settings.WriteBool('IgnoreScmDirs', FIgnoreScmDirs);
  _Settings.WriteBool('IgnoreBackupFiles', FIgnoreBackupFiles);
end;

procedure TBackupProjectExpert.InternalLoadSettings(_Settings: IExpertSettings);
begin
  inherited InternalLoadSettings(_Settings);
  FBackupInc := _Settings.ReadBool('Include', FBackupInc);
  FBackupType := TBackupType(_Settings.ReadEnumerated('Type', TypeInfo(TBackupType), Ord(FBackupType)));
  FBackupDir := _Settings.ReadString('Directory', FBackupDir);
  FIncludeDir := _Settings.ReadBool('IncludeDir', FIncludeDir);
  FBackupScope := TBackupScope(_Settings.ReadEnumerated('BackupScope', TypeInfo(TBackupScope), Ord(FBackupScope)));
  FFollowLibraryPath := _Settings.ReadBool('FollowLibraryPath', FFollowLibraryPath);
  FAddDirsRecursively := _Settings.ReadBool('AddDirsRecursively', FAddDirsRecursively);
  FIgnoreHistoryDir := _Settings.ReadBool('IgnoreHistoryDir', FIgnoreHistoryDir);
  FIgnoreScmDirs := _Settings.ReadBool('IgnoreScmDirs', FIgnoreScmDirs);
  FIgnoreBackupFiles := _Settings.ReadBool('IgnoreBackupFiles', FIgnoreBackupFiles);
end;

initialization
  RegisterGX_Expert(TBackupProjectExpert);

end.