unit DelForExTestFiles;

{$OPTIMIZATION off}

interface

uses
  Classes,
  SysUtils,
  TestFrameWork,
  TestFrameworkExt,
  GX_CodeFormatterTypes,
  GX_CodeFormatterSettings,
  GX_CodeFormatterDefaultSettings,
  GX_CodeFormatterEngine,
  GX_GenericUtils,
  GX_StringList;

type
  TAdjustSettings = procedure(var _cfg: TCodeFormatterEngineSettings) of object;

type
  TTestTestfiles = class(TFileTestCase)
  private
    FFormatter: TCodeFormatterEngine;
    FConfigName: string;
    procedure TrimTrailingCrLf(_sl: TGxUnicodeStringList);
    procedure TestFormatting(const _ConfigName: string; _AdjustSettings: TAdjustSettings = nil);
    function GetFormatSettings(const _Name: string): TCodeFormatterEngineSettings;
    function GetConfigDirBS: string;
    procedure AdjustSettings_Special(var _cfg: TCodeFormatterEngineSettings);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    procedure ExecuteDoubleClickAction; override;
  public
    class function Suite: ITestSuite; override;
  published
    procedure TestHeadworkFormatting;
    procedure TestTwmFormatting;
    procedure TestBorlandFormatting;
    procedure TestDelforFormatting;
    procedure TestDefaultFormatting;
    procedure TestSpecialFormatting;
  end;

implementation

uses
  Windows,
  ShellAPI,
  StrUtils,
  Dialogs,
  Registry,
  GX_CodeFormatterConfigHandler,
  GX_DbugIntf,
  u_dzAssertTrace;

{ TTestTestfiles }

procedure TTestTestfiles.AdjustSettings_Special(var _cfg: TCodeFormatterEngineSettings);
begin
  _cfg.ExceptSingle := True;
  FConfigName := 'special';
end;

procedure TTestTestfiles.ExecuteDoubleClickAction;
const
  CURRENTLY_FAILS = 'CurrentlyFails';

  function TryReadFromReg(_HKey: HKEY; const _Key: string; _Value: string; out _ExePath: string): Boolean;
  var
    reg: TRegistry;
  begin
    Result := False;
    reg := TRegistry.Create;
    try
      reg.RootKey := _HKey;
      if reg.OpenKeyReadOnly(_Key) then begin
        try
          if reg.ValueExists(_Value) then begin
            _ExePath := reg.ReadString(_Value);
            Result := True;
            Exit;
          end;
        finally
          reg.CloseKey;
        end;
      end;
    finally
      FreeAndNil(reg);
    end;
  end;

  function TryReadScooter(out _ExePath: string): Boolean;
  const
    REG_KEY = 'Software\Scooter Software\Beyond Compare';
    REG_VALUE = 'ExePath';
  begin
    Result := TryReadFromReg(HKEY_LOCAL_MACHINE, REG_KEY, REG_VALUE, _ExePath)
  end;

  function TryReadIntelliPoint(out _ExePath: string): Boolean;
  const
    REG_KEY = 'Software\Microsoft\IntelliPoint\AppSpecific\BCompareLite.exe';
    REG_VALUE = 'Path';
  begin
    Result := TryReadFromReg(HKEY_CURRENT_USER, REG_KEY, REG_VALUE, _ExePath)
  end;
var
  fn: string;
  InFile: string;
  ExpectedFile: string;
  OutputFile: string;
  Params: string;
  ExePath: string;
begin
  fn := Name + '.pas';
  InFile := 'testcases\input\' + fn;
  ExpectedFile := 'testcases\expected\' + FConfigName + '\' + fn;
  OutputFile := 'testcases\output\' + FConfigName + '\' + fn;
  Params := Format('"%s" "%s"', [ExpectedFile, OutputFile]);
  if not TryReadScooter(ExePath) then
    if not TryReadIntelliPoint(ExePath) then
      ExePath := 'C:\Program Files (x86)\Beyond Compare 3\bcompare.exe';
  ShellExecute(0, '', PChar(ExePath), PChar(Params), '', SW_NORMAL);
end;

function TTestTestfiles.GetConfigDirBS: string;
begin
  Result := '..\binaries\';
end;

procedure TTestTestfiles.SetUp;
begin
  inherited;
  FFormatter := TCodeFormatterEngine.Create;
end;

class function TTestTestfiles.Suite: ITestSuite;
begin
  Result := TFolderTestSuite.Create(Self, 'testcases\input', '*.pas', False);
end;

procedure TTestTestfiles.TearDown;
begin
  inherited;
  FFormatter.Free;
end;

procedure TTestTestfiles.TrimTrailingCrLf(_sl: TGxUnicodeStringList);
var
  cnt: Integer;
begin
  cnt := _sl.Count;
  while cnt > 0 do begin
    if _sl[cnt - 1] <> '' then
      Exit;
    Dec(cnt);
    _sl.Delete(cnt);
  end;
end;

type
  EFileDoesNotExist = class(EAbort)

  end;

procedure TTestTestfiles.TestHeadworkFormatting;
begin
  TestFormatting('headwork');
end;

procedure TTestTestfiles.TestSpecialFormatting;
begin
  TestFormatting('twm', AdjustSettings_Special);
end;

procedure TTestTestfiles.TestTwmFormatting;
begin
  TestFormatting('twm');
end;

procedure TTestTestfiles.TestBorlandFormatting;
begin
  TestFormatting('borland');
end;

procedure TTestTestfiles.TestDefaultFormatting;
begin
  TestFormatting('default');
end;

procedure TTestTestfiles.TestDelforFormatting;
begin
  TestFormatting('delforex');
end;

procedure TTestTestfiles.TestFormatting(const _ConfigName: string; _AdjustSettings: TAdjustSettings = nil);
var
  Filename: string;
  InFile: string;
  ExpectedFile: string;
  ExpectedText: TGxUnicodeStringList;
  st: TGxUnicodeStringList;
  Cfg: TCodeFormatterEngineSettings;
begin
  FConfigName := _ConfigName;
  Cfg := GetFormatSettings(FConfigName);
  if Assigned(_AdjustSettings) then
    _AdjustSettings(Cfg);
  FFormatter.Settings.Settings := Cfg;
  InFile := TestFileName;
  Filename := ExtractFileName(InFile);
  ExpectedFile := 'testcases\expected\' + FConfigName + '\' + Filename;
  if not FileExists(InFile) then begin
//    ExpectedException := EFileDoesNotExist;
    raise EFileDoesNotExist.CreateFmt('Input file does not exist: %s', [InFile]);
  end;
  if not FileExists(ExpectedFile) then begin
    CheckTrue(CopyFile(PChar(InFile), PChar(ExpectedFile), True), 'Copying file failed');
    Self.Status('Warning: Input file was copied to expected!');
  end;

  ExpectedText := nil;
  st := TGxUnicodeStringList.Create;
  try
    st.LoadFromFile(InFile);
    ExpectedText := TGxUnicodeStringList.Create;
    ExpectedText.LoadFromFile(ExpectedFile);
    FFormatter.Execute(st);
    try
      TrimTrailingCrLf(ExpectedText);
      TrimTrailingCrLf(st);
// uncomment if you want to use e.g. BeyondCompare to do the comparison
//      st.SaveToFile('testcases\output\' + GetResultDir + '\' + Filename);
      WriteTrace('d:\' + Filename + '.Log');
      ClearTrace;
      CheckEquals(ExpectedText.Text, st.Text, 'error in output');
    except
      on e: ETestFailure do begin
        // Since st.LoadFromFile assumes UTF-8 even if there is no BOM but the content
        // can be converted from UTF-8, st.SaveFormat has been set sfUTF8.
        // This will add a BOM in st.SaveToFile.
        // This is not quite what we want, but since BeyondCompare can handle that
        // we don't really care.
        st.SaveToFile('testcases\output\' + FConfigName + '\' + Filename);
//        if _AllowFailure then
//          e.Message := 'known ' + e.Message;
        raise;
      end;
    end;
  finally
    ExpectedText.Free;
    st.Free;
  end;
end;

function TTestTestfiles.GetFormatSettings(const _Name: string): TCodeFormatterEngineSettings;
var
  Settings: TCodeFormatterSettings;
begin
  Settings := TCodeFormatterSettings.Create;
  try
    TCodeFormatterConfigHandler.ImportFromFile(GetConfigDirBS + 'FormatterSettings-' + _Name + '.ini', Settings, '');
    Result := Settings.Settings;
  finally
    Settings.Free;
  end;
end;

initialization
  RegisterTest(TTestTestfiles.Suite);
end.
