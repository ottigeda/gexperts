unit GX_GrepBackend;

{$I GX_CondDefine.inc}

interface

uses
  Classes,
  ToolsAPI,
  RegExpr,
  GX_GrepRegExSearch, GX_GenericUtils;

type
  TGrepAction = (gaProjGrep, gaCurrentOnlyGrep, gaOpenFilesGrep, gaDirGrep, gaProjGroupGrep, gaResults);

  // Saved grep settings (used for refresh)
  TGrepSettings = record
    IncludeComments: Boolean;
    CaseSensitive: Boolean;
    WholeWord: Boolean;
    RegEx: Boolean;
    IncludeSubdirs: Boolean;
    Directories: string;
    ExcludedDirs: string;
    Mask: string;
    Pattern: string;
    Replace: string;
    GrepAction: TGrepAction;
    CanRefresh: Boolean;
    IncludeForms: Boolean;
  end;

type
  // Individual grep match in a line
  TMatchResult = class(TCollectionItem)
  private
    FSPos: Integer;
    FEPos: Integer;
    FShowBold: Boolean;
  public
    property SPos: Integer read FSPos write FSPos;
    property EPos: Integer read FEPos write FEPos;
    property ShowBold: Boolean read FShowBold write FShowBold;
    constructor Create(Collection: TCollection); override;
    function Length: Integer;
  end;

  // Collection of TMatchResult
  // Collection of all matches in a line
  TLineMatches = class(TCollection)
  private
    function GetItem(Index: Integer): TMatchResult;
    procedure SetItem(Index: Integer; Value: TMatchResult);
  public
    constructor Create;
    function Add: TMatchResult;
    property Items[Index: Integer]: TMatchResult read GetItem write SetItem; default;
  end;

  // A single line that has a match from a file
  // One collection item per line with any number of matches
  TLineResult = class(TCollectionItem)
  private
    FLine: string;
    FLineNo: Integer; // 1-based
    FMatches: TLineMatches;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    function Add: TMatchResult;
  public
    property Line: string read FLine write FLine;
    property LineNo: Integer read FLineNo write FLineNo; // 1-based
    // Collection of all matches in a line
    property Matches: TLineMatches read FMatches;
  end;

  TMatchArray = array of TMatchResult;

  // Contains collection of all lines in a single source file that match.
  TFileResult = class(TCollection)
  private
    FExpanded: Boolean;
    FFileName: string;
    FRelativeFileName: string;
    FLastLineResult: Integer; // Last LineNo added to result set
    FLastIndex: Integer;      // Index of last added result
    FTotalMatches: Integer;   // Total matches in file
    function GetItem(Index: Integer): TLineResult;
    procedure SetItem(Index: Integer; Value: TLineResult);
  public
    constructor Create;
    function Add: TLineResult;
    procedure GetMatchesOnLine(Line: Integer; var Matches: TMatchArray);
    property Expanded: Boolean read FExpanded write FExpanded;
    property FileName: string read FFileName write FFileName;
    property RelativeFileName: string read FRelativeFileName write FRelativeFileName;
    property LastIndex: Integer read FLastIndex write FLastIndex;
    property LastLineResult: Integer read FLastLineResult write FLastLineResult;
    property Items[Index: Integer]: TLineResult read GetItem write SetItem; default;
    property TotalMatches: Integer read FTotalMatches write FTotalMatches;
  end;

type
  TOnHitMatch = procedure(Sender: TObject; LineNo: Integer; const Line: string;
      SPos, EPos: Integer) of object;
  TOnSearchFile = procedure(Sender: TObject; const FileName: string) of object;

  TGrepSearchContext = class(TObject)
  public
    Project: string;
    function ToString: string; {$ifdef GX_VER200_up} override {$else} virtual {$endif GX_VER200_up};
  end;

  TGrepSearchRunner = class(TObject)
  private
    FOnHitMatch: TOnHitMatch;
    FOnSearchFile: TOnSearchFile;
    FStorageTarget: TStrings;
    FDupeFileList: TStringList;
    FExceptionList: TStrings;
    FAbortSignalled: Boolean;
    FFileSearchCount: Integer;
    FMatchCount: Integer;
    FExcludedDirsRegEx: TRegExpr;
    FFileResult: TFileResult;
    FSearcher: TSearcher;
    FSearchRoot: string;
    FFilesInResults: TStrings;
    procedure FoundIt(LineNo, StartCol, EndCol: Integer; const Line: TGXUnicodeString);
    procedure StartFileSearch(const FileName: string);
    procedure ExecuteSearchOnFile(const FileName: string; Context: TGrepSearchContext; FromProject: Boolean = False);
    procedure SearchFormForFile(const FileName: string; Context: TGrepSearchContext);
  private
    FGrepSettings: TGrepSettings;
    procedure GrepProjectFile(const FileName: string; Context: TGrepSearchContext);
  protected
    procedure DoHitMatch(LineNo: Integer; const Line: string;
      SPos, EPos: Integer); virtual;
    procedure GrepCurrentSourceEditor;
    procedure GrepProjectGroup;
    procedure GrepProject(Project: IOTAProject);
    procedure GrepDirectory(Dir, Mask: string);
    procedure GrepDirectories(const Dir, Mask: string);
    procedure GrepResults;
  public
    constructor Create(const Settings: TGrepSettings; StorageTarget, FilesInResults: TStrings);
    procedure Execute;
    property OnSearchFile: TOnSearchFile read FOnSearchFile write FOnSearchFile;
    property FileSearchCount: Integer read FFileSearchCount;
    property MatchCount: Integer read FMatchCount;
    property AbortSignalled: Boolean read FAbortSignalled write FAbortSignalled;
  end;

implementation

uses
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  SysUtils, Forms,
  GX_OtaUtils, GX_EditReader, GX_IdeUtils, Dialogs, Controls;

{ TLineMatches }

constructor TLineMatches.Create;
begin
  inherited Create(TMatchResult);
end;

function TLineMatches.Add: TMatchResult;
begin
  Result := TMatchResult(inherited Add);
end;

function TLineMatches.GetItem(Index: Integer): TMatchResult;
begin
  Result := TMatchResult(inherited GetItem(Index));
end;

procedure TLineMatches.SetItem(Index: Integer; Value: TMatchResult);
begin
  inherited SetItem(Index, Value);
end;

{ TLineResult }

constructor TLineResult.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FMatches := TLineMatches.Create;
end;

destructor TLineResult.Destroy;
begin
  if Assigned(FMatches) then
  begin
    FMatches.Clear;
    FreeAndNil(FMatches);
  end;
  inherited Destroy;
end;

function TLineResult.Add: TMatchResult;
begin
  Result := Matches.Add;
end;

{ TFileResult }

constructor TFileResult.Create;
begin
  inherited Create(TLineResult);
  FLastLineResult := -1;
  FTotalMatches := 0;
end;

function TFileResult.Add: TLineResult;
begin
  Result := TLineResult(inherited Add);
end;

function TFileResult.GetItem(Index: Integer): TLineResult;
begin
  Result := TLineResult(inherited GetItem(Index));
end;

procedure TFileResult.SetItem(Index: Integer; Value: TLineResult);
begin
  inherited SetItem(Index, Value);
end;

procedure TFileResult.GetMatchesOnLine(Line: Integer; var Matches: TMatchArray);
var
  i, j: Integer;
  LineMatches: TLineResult;
  MR: TMatchResult;
begin
  SetLength(Matches, 0);
  for i := 0 to Count - 1 do
  begin
    LineMatches := GetItem(i);
    if LineMatches.FLineNo = Line then
    begin
      for j := 0 to LineMatches.Matches.Count - 1 do
      begin
        SetLength(Matches, Length(Matches) + 1);
        MR := LineMatches.Matches.GetItem(j);
        Matches[Length(Matches) - 1] := MR;
      end;
    end;
  end;
end;

{ TGrepSearchRunner }

procedure TGrepSearchRunner.GrepProjectFile(const FileName: string; Context: TGrepSearchContext);
begin
  try
    Application.ProcessMessages;

    if IsBdsSourceFile(FileName) then
    begin
      Assert(FFileResult = nil, 'FFileResult leak');
      FFileResult := nil;

      if (FGrepSettings.GrepAction = gaOpenFilesGrep) and (not GxOtaIsFileOpen(FileName)) then
        Exit;
      ExecuteSearchOnFile(FileName, Context, True);
      if IsCpp(FileName) and (FGrepSettings.GrepAction in [gaProjGrep, gaOpenFilesGrep, gaProjGroupGrep]) then
        if GxOtaFileOrModuleExists(ChangeFileExt(FileName, '.h')) then
          ExecuteSearchOnFile(ChangeFileExt(FileName, '.h'), Context, True);
      FFileResult := nil;
    end;
  except
    on E: Exception do
      {$IFOPT D+} SendDebugError('GrepFile: ' + E.Message); {$ENDIF}
  end;
end;

constructor TGrepSearchRunner.Create(const Settings: TGrepSettings; StorageTarget, FilesInResults: TStrings);
begin
  inherited Create;

  Assert(Assigned(StorageTarget));
  Assert(Assigned(FilesInResults));
  FStorageTarget := StorageTarget;
  FFilesInResults := FilesInResults;
  FGrepSettings := Settings;
end;

procedure TGrepSearchRunner.GrepProjectGroup;
var
  i: Integer;
  ProjectGroup: IOTAProjectGroup;
  Context: TGrepSearchContext;
begin
  ProjectGroup := GxOtaGetProjectGroup;
  if ProjectGroup = nil then
    Exit;
  FSearchRoot := ExtractFilePath(ProjectGroup.FileName);
  Context := TGrepSearchContext.Create;
  try
    GrepProjectFile(ProjectGroup.FileName, Context);
  finally
    FreeAndNil(Context);
  end;
  for i := 0 to ProjectGroup.ProjectCount - 1 do
    GrepProject(ProjectGroup.Projects[i]);
end;

procedure TGrepSearchRunner.GrepProject(Project: IOTAProject);
var
  i: Integer;
  Context: TGrepSearchContext;
begin
  if Project = nil then
    Exit;

  FSearchRoot := ExtractFilePath(Project.FileName);
  Context := TGrepSearchContext.Create;
  try
    Context.Project := GxOtaGetProjectFileName(Project, True);
    GrepProjectFile(Context.Project, Context);

    for i := 0 to Project.GetModuleCount - 1 do
    begin
      GrepProjectFile(Project.GetModule(i).GetFileName, Context);
      if FAbortSignalled then
        Break;
    end;
  finally
    FreeAndNil(Context);
  end;
end;

procedure TGrepSearchRunner.GrepCurrentSourceEditor;
resourcestring
  SNoFileOpen = 'No file is currently open';
var
  CurrentFile: string;
  Context: TGrepSearchContext;
begin
  if IsStandAlone then Exit;

  CurrentFile := GxOtaGetBaseModuleFileName(GxOtaGetCurrentSourceFile);

  Assert(FFileResult = nil, 'FFileResult leak');
  FFileResult := nil;

  FSearchRoot := ExtractFilePath(CurrentFile);
  if NotEmpty(CurrentFile) and (not FileIsWelcomePage(CurrentFile)) then
  begin
    Context := TGrepSearchContext.Create;
    try
      ExecuteSearchOnFile(CurrentFile, Context)
    finally
      FreeAndNil(Context);
    end;
  end
  else
    raise Exception.Create(SNoFileOpen);
end;

procedure TGrepSearchRunner.GrepDirectories(const Dir, Mask: string);
var
  i: Integer;
  DirList: TStringList;
begin
  DirList := TStringList.Create;
  try
    AnsiStrTok(Dir, ';', DirList);
    for i := 0 to DirList.Count - 1 do
    begin
      if FAbortSignalled then
        Break;
      FSearchRoot := DirList[i];
      GrepDirectory(DirList[i], Mask);
    end;
  finally
    FreeAndNil(DirList);
  end;
end;

{$WARN SYMBOL_PLATFORM OFF}

procedure TGrepSearchRunner.GrepDirectory(Dir, Mask: string);
resourcestring
  SSpecifiedDirectoryDoesNotExist = 'The search directory %s does not exist';
var
  Search: TSearchRec;
  Result: Integer;
  Masks: TStrings;
  i: Integer;
  SearchFile: string;
  Context: TGrepSearchContext;
begin
  {$IFOPT D+} SendDebug('DirGrep on: ' +Dir+'; Mask: '+Mask); {$ENDIF}
  Dir := AddSlash(Dir);
  if not DirectoryExists(Dir) then
    raise Exception.CreateFmt(SSpecifiedDirectoryDoesNotExist, [Dir]);

  Masks := TStringList.Create;
  try
    for i := 1 to Length(Mask) do
      if CharInSet(Mask[i], [';', ',']) then
        Mask[i] := #13;

    Masks.Text := Mask;

    if FGrepSettings.IncludeSubdirs then
    begin
      Result := FindFirst(Dir + AllFilesWildCard, faAnyFile, Search);
      try
        while Result = 0 do
        begin
          if ((Search.Attr and faDirectory) <> 0) {$IFDEF GX_VER150_up} and ((Search.Attr and faSymLink) = 0) {$ENDIF} then
          begin
            if (Search.Name <> '.') and (Search.Name <> '..') then
            begin
              if IsEmpty(FGrepSettings.ExcludedDirs) or (not FExcludedDirsRegEx.Exec(Dir + Search.Name)) then
                GrepDirectory(Dir + Search.Name, Mask);
            end;
          end;
          if FAbortSignalled then
            Exit;
          Result := FindNext(Search);
        end;
      finally
        FindClose(Search);
      end;
    end;

    for i := 0 to Masks.Count-1 do
    begin
      if FAbortSignalled then
        Break;

      Result := FindFirst(Dir + Trim(Masks.Strings[i]), faAnyFile, Search);
      try
        Context := TGrepSearchContext.Create;
        try
          while Result = 0 do
          begin
            if (Search.Attr and faDirectory) <> 0 then
              Result := FindNext(Search)
            else
            begin
              Assert(FFileResult = nil, 'FFileResult leak');
              FFileResult := nil;

              // FindFirst matches *.pas~ with a wildcard of *.pas, so we correct for that here
              if WildcardCompare(Masks.Strings[i], Search.Name, True) then
              begin
                SearchFile := Dir + Search.Name;
                if IsEmpty(FGrepSettings.ExcludedDirs) or (not FExcludedDirsRegEx.Exec(SearchFile)) then
                  ExecuteSearchOnFile(SearchFile, Context);
              end;
              FFileResult := nil;

              if FAbortSignalled then
                Break;

              Result := FindNext(Search);
            end;
          end; // while
        finally
          FreeAndNil(Context);
        end; // finally
      finally
        FindClose(Search);
      end; // finally
    end;
  finally
    FreeAndNil(Masks);
  end; // finally
end;

procedure TGrepSearchRunner.GrepResults;
var
  i: Integer;
  Context: TGrepSearchContext;
begin
  Context := TGrepSearchContext.Create;
  try
    for i := 0 to FFilesInResults.Count - 1 do
    begin
      if GxOtaFileOrModuleExists(FFilesInResults[i]) then
      begin
        {$IFOPT D+} SendDebug('ResultsGrep on ' + FFilesInResults[i]); {$ENDIF}
        ExecuteSearchOnFile(FFilesInResults[i], Context);
      end;
    end;
  finally
    FreeAndNil(Context);
  end; // finally
end;

procedure TGrepSearchRunner.Execute;
var
  i: Integer;
  lExcludedDirs: string;
begin
  FFileSearchCount := 0;
  FMatchCount := 0;

  FExcludedDirsRegEx := TRegExpr.Create;
  try
    if NotEmpty(FGrepSettings.ExcludedDirs) then
    begin
      lExcludedDirs := Trim(FGrepSettings.ExcludedDirs);
      i := Length(lExcludedDirs);
      while (i > 0) and (lExcludedDirs[i] = ';') do
        Dec(i);
      SetLength(lExcludedDirs, i);
      lExcludedDirs := QuoteRegExprMetaChars(lExcludedDirs);
      FExcludedDirsRegEx.Expression := StringReplace(lExcludedDirs, ';', '|', [rfReplaceAll]);
      try
        FExcludedDirsRegEx.Compile;
      except
        on E: Exception do
        begin
          E.Message := 'Invalid or empty item in directory exclusion list: ' + E.Message;
          raise;
        end;
      end;
    end;

    FSearcher := TSearcher.Create;
    try
      FSearcher.OnFound := FoundIt;
      FSearcher.NoComments := not FGrepSettings.IncludeComments;
      FSearcher.CaseSensitive := FGrepSettings.CaseSensitive;
      FSearcher.WholeWord := FGrepSettings.WholeWord;
      FSearcher.RegularExpression := FGrepSettings.RegEx;
      FSearcher.Pattern := FGrepSettings.Pattern;

      FDupeFileList := TStringList.Create;
      try
        FDupeFileList.Sorted := True;
        FExceptionList := TStringList.Create;
        try
          case FGrepSettings.GrepAction of
            gaProjGrep:
              GrepProject(GxOtaGetCurrentProject);
            gaProjGroupGrep:
              GrepProjectGroup;
            gaCurrentOnlyGrep:
              GrepCurrentSourceEditor;
            gaOpenFilesGrep:
              GrepProject(GxOtaGetCurrentProject);
            gaDirGrep:
              begin
                if Length(Trim(FGrepSettings.Mask)) = 0 then
                begin
                  if GxOtaCurrentProjectIsNativeCpp then
                    GrepDirectories(FGrepSettings.Directories, '*.cpp;*.hpp;*.h;*.pas;*.inc')
                  else if GxOtaCurrentProjectIsCSharp then
                    GrepDirectories(FGrepSettings.Directories, '*.cs')
                  else
                    GrepDirectories(FGrepSettings.Directories, '*.pas;*.dpr;*.inc')
                end
                else
                  GrepDirectories(FGrepSettings.Directories, AnsiUpperCase(FGrepSettings.Mask));
              end;
            gaResults:
              GrepResults;
          end;	// end case
          if FExceptionList.Count > 0 then
          begin
            if MessageDlg(FExceptionList.Text, mtError, [mbOK, mbCancel], 0) = mrCancel then
              Abort;
          end;
        finally
          FreeAndNil(FExceptionList);
        end;
      finally
        FreeAndNil(FDupeFileList);
      end;

    finally
      FreeAndNil(FSearcher);
    end;
  finally
    FreeAndNil(FExcludedDirsRegEx);
  end;
end;

procedure TGrepSearchRunner.SearchFormForFile(const FileName: string; Context: TGrepSearchContext);
var
  Module: IOTAModule;
  FormEditor: IOTAFormEditor;
  FormFile: string;
begin
  FormEditor := nil;
  if RunningInsideIDE then
  begin
    Module := GxOtaGetModule(FileName);
    FormEditor := GxOtaGetFormEditorFromModule(Module);
  end;
  if Assigned(FormEditor) then
    ExecuteSearchOnFile(FormEditor.FileName, Context)
  else
  begin
    {TODO -o##jwp -cFix : When project has multiple forms using IFDEF, search them all}
    FormFile := ChangeFileExt(FileName, '.dfm');
    if not FileExists(FormFile) then
      FormFile := ChangeFileExt(FormFile, '.nfm');
    if not FileExists(FormFile) then
      FormFile := ChangeFileExt(FormFile, '.xfm');
    if FileExists(FormFile) then
      ExecuteSearchOnFile(FormFile, Context);
  end;
end;

procedure TGrepSearchRunner.ExecuteSearchOnFile(const FileName: string; Context: TGrepSearchContext; FromProject: Boolean);
var
  TmpNoComments: Boolean;
  ContextString: string;
begin
  Assert(Assigned(FDupeFileList));
  if FDupeFileList.IndexOf(FileName) = -1 then
  begin
    StartFileSearch(FileName);
    try
      FSearcher.FileName := FileName;
      FDupeFileList.Add(FileName);

      // Because we can search directories and multiple extensions, and because the
      // ignore comments option is ignored with anything non Delphi, we may need to
      // turn it off temporarily, depending on what is searched for.
      TmpNoComments := FSearcher.NoComments;
      FSearcher.NoComments := ((FSearcher.NoComments) and (IsPascalSourceFile(FileName)));

      FSearcher.Execute;

      FSearcher.NoComments := TmpNoComments;
    except
      on E: Exception do
      begin
        if FromProject and (E is EGXFileNotFound) then
          E.Message := E.Message + '  Please check your dpr/dproj files and correct the path/filename referenced there.';
        ContextString := Context.ToString;
        if ContextString = '' then
          FExceptionList.Add(Format('Exception %s while searching "%s"; Message %s', [E.ClassName, FileName, E.Message]))
        else
          FExceptionList.Add(Format('Exception %s while searching "%s" in context "%s"; Message %s', [E.ClassName, FileName, ContextString, E.Message]));
        if not (E is EGXFileNotFound) then
          if MessageDlg(E.Message, mtError, [mbOK, mbCancel], 0) = mrCancel then
            Abort;
      end;
    end;

    if FGrepSettings.IncludeForms and (IsPas(FileName) or IsCpp(FileName)) then
      SearchFormForFile(FileName, Context);
  end;
end;

procedure TGrepSearchRunner.FoundIt(LineNo, StartCol, EndCol: Integer; const Line: TGXUnicodeString);
var
  ALineResult: TLineResult;
  AMatchResult: TMatchResult;
begin
  Inc(FMatchCount);

  // If this is the first match or the match is on a
  // different file then add a new TFileResult.
  if (FFileResult = nil) or (FFileResult.FileName <> FSearcher.FileName) then
  begin
    FFileResult := TFileResult.Create;
    FFileResult.FileName := FSearcher.FileName;
    FFileResult.RelativeFileName := StringReplace(FSearcher.FileName, FSearchRoot, '', [rfIgnoreCase]);
    FStorageTarget.AddObject(FSearcher.FileName, FFileResult);
  end;

  // If the match is not on the same line number as the
  // last match then add another TLineResult to the file's
  // result set.
  if FFileResult.LastLineResult <> LineNo then
  begin
    ALineResult := FFileResult.Add;
    ALineResult.Line := Line;
    ALineResult.LineNo := LineNo;

    // Save Index number and line number for next match
    FFileResult.LastIndex := FFileResult.Count-1;
    FFileResult.LastLineResult := LineNo;
  end
  else
  begin
    // If the match is on the same line then add the
    // match to the previous match line
    ALineResult := FFileResult[FFileResult.LastIndex];
  end;

  AMatchResult := ALineResult.Add;
  AMatchResult.SPos := StartCol;
  AMatchResult.EPos := EndCol;
  FFileResult.TotalMatches := FFileResult.TotalMatches + 1;
end;

procedure TGrepSearchRunner.StartFileSearch(const FileName: string);
begin
  Inc(FFileSearchCount);
  if Assigned(FOnSearchFile) then
    FOnSearchFile(Self, FileName);
end;

procedure TGrepSearchRunner.DoHitMatch(LineNo: Integer; const Line: string;
  SPos, EPos: Integer);
begin
  if Assigned(FOnHitMatch) then
    FOnHitMatch(Self, LineNo, Line, SPos, EPos);
end;

{ TMatchResult }

constructor TMatchResult.Create(Collection: TCollection);
begin
  inherited;
  ShowBold := True;
end;

function TMatchResult.Length: Integer;
begin
  Result := EPos - SPos + 1;
end;

{ TGrepSearchContext }

function TGrepSearchContext.ToString: string;
begin
  if Project <> '' then
    Result := Format('Project: %s', [Project]);
{
For now, handle the Project.
In the future, try to handle these:

- GrepProjectGroup
- GrepProject
- GrepDirectories
- GrepProjectFile
- GrepCurrentSourceEditor
- GrepDirectory
- GrepResults
- SearchFormForFile
}
end;

end.
