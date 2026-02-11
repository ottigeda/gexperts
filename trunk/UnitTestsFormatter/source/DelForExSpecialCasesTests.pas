unit DelForExSpecialCasesTests;

{$OPTIMIZATION off}

interface

uses
  Classes,
  SysUtils,
  Types,
  TestFramework,
  TestFrameworkExt,
  GX_CodeFormatterTypes,
  GX_CodeFormatterSettings,
  GX_CodeFormatterDefaultSettings,
  GX_CodeFormatterEngine,
  GX_GenericUtils,
  GX_StringList;

type
  TAdjustSettingsProc = procedure(var _cfg: TCodeFormatterEngineSettings);

  ///<summary>
  /// Test case for a single file within a special case configuration.
  /// Each instance tests one input file against one special case configuration.</summary>
  TSpecialCaseFileTest = class(TTestCase)
  private
    FFormatter: TCodeFormatterEngine;
    FSpecialCaseName: string;
    FTestFileName: string;
    FOnAdjustSettings: TAdjustSettingsProc;
    procedure TrimTrailingCrLf(_sl: TGXUnicodeStringList);
    function GetBaseSettings: TCodeFormatterEngineSettings;
    function GetConfigDirBS: string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    procedure ExecuteDoubleClickAction; override;
  public
    constructor Create(const _SpecialCaseName, _TestFileName: string;
      _OnAdjustSettigs: TAdjustSettingsProc); reintroduce;
    function GetName: string; override;
  published
    procedure TestFormatting;
  end;

  ///<summary>
  /// Test suite that discovers all .pas files in a special case's input folder
  /// and creates a TSpecialCaseFileTest for each one.</summary>
  TSpecialCaseTestSuite = class(TTestSuite)
  public
    constructor Create(const _SpecialCaseName: string; _OnAdjustSettigs: TAdjustSettingsProc);
  end;

// Settings adjusters for each special case
procedure AdjustSettings_IndentSections(var _cfg: TCodeFormatterEngineSettings);
procedure AdjustSettings_NoIndentVarDecl(var _cfg: TCodeFormatterEngineSettings);
procedure AdjustSettings_AlignAssign(var _cfg: TCodeFormatterEngineSettings);

implementation

uses
  Windows,
  ShellAPI,
  u_dzVclUtils,
  u_dzFileUtils,
  GX_CodeFormatterConfigHandler,
  DelForExBeyondCompare;

{ TSpecialCaseFileTest }

constructor TSpecialCaseFileTest.Create(const _SpecialCaseName, _TestFileName: string;
  _OnAdjustSettigs: TAdjustSettingsProc);
begin
  inherited Create('TestFormatting');
  FSpecialCaseName := _SpecialCaseName;
  FTestFileName := _TestFileName;
  FOnAdjustSettings := _OnAdjustSettigs;
end;

function TSpecialCaseFileTest.GetName: string;
begin
  Result := ChangeFileExt(ExtractFileName(FTestFileName), '');
end;

procedure TSpecialCaseFileTest.SetUp;
begin
  inherited;
  FFormatter := TCodeFormatterEngine.Create;
end;

procedure TSpecialCaseFileTest.TearDown;
begin
  FFormatter.Free;
  inherited;
end;

procedure TSpecialCaseFileTest.ExecuteDoubleClickAction;
var
  fn: string;
  ExpectedFile: string;
  OutputFile: string;
  Params: string;
begin
  fn := GetName + '.pas';
  ExpectedFile := 'testcases\SpecialCases\' + FSpecialCaseName + '\expected\' + fn;
  OutputFile := 'testcases\SpecialCases\' + FSpecialCaseName + '\output\' + fn;
  Params := Format('"%s" "%s"', [ExpectedFile, OutputFile]);
  ShellExecute(0, '', PChar(GetBeyondCompareExePath), PChar(Params), '', SW_NORMAL);
end;

function TSpecialCaseFileTest.GetConfigDirBS: string;
begin
  Result := itpd(TFileSystem.ExpandFileNameRelBaseDir('..\binaries', TApplication_GetExePath));
end;

function TSpecialCaseFileTest.GetBaseSettings: TCodeFormatterEngineSettings;
var
  Settings: TCodeFormatterSettings;
begin
  Settings := TCodeFormatterSettings.Create;
  try
    TCodeFormatterConfigHandler.ImportFromFile(GetConfigDirBS + 'FormatterSettings-borland.ini', Settings, '');
    Result := Settings.Settings;
  finally
    Settings.Free;
  end;
end;

procedure TSpecialCaseFileTest.TrimTrailingCrLf(_sl: TGXUnicodeStringList);
var
  Cnt: Integer;
begin
  Cnt := _sl.Count;
  while Cnt > 0 do begin
    if _sl[Cnt - 1] <> '' then
      Exit;
    Dec(Cnt);
    _sl.Delete(Cnt);
  end;
end;

procedure TSpecialCaseFileTest.TestFormatting;
var
  FileName: string;
  ExpectedFile: string;
  OutputFile: string;
  ExpectedText: TGXUnicodeStringList;
  st: TGXUnicodeStringList;
  cfg: TCodeFormatterEngineSettings;
begin
  cfg := GetBaseSettings;
  if Assigned(FOnAdjustSettings) then
    FOnAdjustSettings(cfg);
  FFormatter.Settings.Settings := cfg;

  FileName := ExtractFileName(FTestFileName);
  ExpectedFile := 'testcases\SpecialCases\' + FSpecialCaseName + '\expected\' + FileName;
  OutputFile := 'testcases\SpecialCases\' + FSpecialCaseName + '\output\' + FileName;

  if not FileExists(FTestFileName) then
    raise Exception.CreateFmt('Input file does not exist: %s', [FTestFileName]);

  if not FileExists(ExpectedFile) then begin
    CheckTrue(CopyFile(PChar(FTestFileName), PChar(ExpectedFile), True), 'Copying file failed');
    self.Status('Warning: Input file was copied to expected!');
  end;

  ExpectedText := nil;
  st := TGXUnicodeStringList.Create;
  try
    st.LoadFromFile(FTestFileName);
    ExpectedText := TGXUnicodeStringList.Create;
    ExpectedText.LoadFromFile(ExpectedFile);
    FFormatter.Execute(st);
    try
      TrimTrailingCrLf(ExpectedText);
      TrimTrailingCrLf(st);
      CheckEquals(ExpectedText.Text, st.Text, 'error in output');
    except
      on e: ETestFailure do begin
        ForceDirectories(ExtractFilePath(OutputFile));
        st.SaveToFile(OutputFile);
        raise;
      end;
    end;
  finally
    ExpectedText.Free;
    st.Free;
  end;
end;

{ TSpecialCaseTestSuite }

constructor TSpecialCaseTestSuite.Create(const _SpecialCaseName: string;
  _OnAdjustSettigs: TAdjustSettingsProc);
var
  InputPath: string;
  FileList: TStringDynArray;
  FileName: string;
begin
  inherited Create(_SpecialCaseName);

  InputPath := 'testcases\SpecialCases\' + _SpecialCaseName + '\input';
  FileList := TSimpleDirEnumerator.EnumFilesOnly(IncludeTrailingPathDelimiter(InputPath) + '*.pas', True);
  for FileName in FileList do
    AddTest(TSpecialCaseFileTest.Create(_SpecialCaseName, FileName, _OnAdjustSettigs));
end;

{ Settings adjusters }

procedure AdjustSettings_IndentSections(var _cfg: TCodeFormatterEngineSettings);
begin
  _cfg.IndentInterfaceImpl := True;
end;

procedure AdjustSettings_NoIndentVarDecl(var _cfg: TCodeFormatterEngineSettings);
begin
  _cfg.NoIndentVarDecl := True;
end;

procedure AdjustSettings_AlignAssign(var _cfg: TCodeFormatterEngineSettings);
begin
  _cfg.AlignAssign := True;
  _cfg.AlignAssignPos := 30;
end;

{ Registration }

function CreateSpecialCasesSuite: ITestSuite;
var
  suite: TTestSuite;
begin
  suite := TTestSuite.Create('SpecialCases');
  // Add each special case as a nested suite
  // To add a new special case:
  //   1. Create testcases\SpecialCases\<name>\input\, expected\, output\ directories
  //   2. Add a procedure AdjustSettings_<name> above
  //   3. Add Suite.AddSuite(TSpecialCaseTestSuite.Create('<name>', AdjustSettings_<name>)) below
  suite.AddSuite(TSpecialCaseTestSuite.Create('indentsections', AdjustSettings_IndentSections));
  suite.AddSuite(TSpecialCaseTestSuite.Create('NoIndentVarDecl', AdjustSettings_NoIndentVarDecl));
  suite.AddSuite(TSpecialCaseTestSuite.Create('AlignAssign', AdjustSettings_AlignAssign));
  Result := suite;
end;

initialization
  RegisterTest(CreateSpecialCasesSuite);

end.
