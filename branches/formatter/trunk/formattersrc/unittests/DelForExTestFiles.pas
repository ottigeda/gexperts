unit DelForExTestFiles;

{$OPTIMIZATION off}

interface

uses
  Classes,
  SysUtils,
  TestFrameWork,
  GX_CodeFormatterTypes,
  GX_CodeFormatterSettings,
  GX_CodeFormatterDefaultSettings,
  GX_CodeFormatterEngine;

type
  TTestTestfiles = class(TTestCase)
  private
    FFormatter: TCodeFormatterEngine;
    procedure TestFile(const _Filename: string; _AllowFaiure: boolean = false);
  protected
    function GetFormatSettings: TCodeFormatterEngineSettings; virtual; abstract;
    function GetResultDir: string; virtual; abstract;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure testStrictVisibility;
    procedure testCompilerDirectives;
    procedure testAssemblerNewlines;
    procedure testIfdefs;
    procedure testUnmatchedIfdef;
    procedure testUnmatchedElse;
    procedure testUnmatchedEndif;
    procedure testIfElseendif;
    procedure testDisableFormatComment;
    procedure testLargeFile;
    procedure testTripleQuotes;
    procedure testSingleElse;
    procedure testQuotesError;
    procedure testTabBeforeEndInAsm;
    procedure testElseAtEnd;
    procedure testEmptyProgram;
    procedure testEmptyStringAssignment;
    procedure testEmptyUnit;
    procedure testHashCharStrings;
    procedure testIndentComment;
    procedure testJustOpeningComment;
    procedure testJustOpeningStarCommentInAsm;
    procedure testStringWithSingleQuotes;
    procedure testUnterminatedString;
    procedure testDotFloat;
    procedure testHexNumbers;
    procedure testControlChars;
    procedure testFormula;
    procedure testAsmProblem1;
    procedure testCommentEnd;
    procedure testCurlyHalfCommentEndCurrentlyFails;
    procedure testIfThenTry;
    procedure testIfThenElse;
    procedure testStarCommentAtEol;
    procedure testClassPropertiesCurrentlyFails;
    procedure testNestedClassCurrentlyFails;
    procedure testRecordMethod;
    procedure testAbstractSealedClass;
    procedure testOperatorOverloading;
    procedure testAsm;
    procedure testConstSet;
    procedure testConstSetWithComment;
    procedure testConstSetWithCommentAtEnd;
  end;

type
  TTestFilesHeadworkFormatting = class(TTestTestfiles)
  protected
    function GetFormatSettings: TCodeFormatterEngineSettings; override;
    function GetResultDir: string; override;
  published
  end;

type
  TTestFilesBorlandFormatting = class(TTestTestfiles)
  protected
    function GetFormatSettings: TCodeFormatterEngineSettings; override;
    function GetResultDir: string; override;
  end;

type
  TTestFilesDelforFormatting = class(TTestTestfiles)
  protected
    function GetFormatSettings: TCodeFormatterEngineSettings; override;
    function GetResultDir: string; override;
  end;

type
  TTestFilesTwmFormatting = class(TTestTestfiles)
  protected
    function GetFormatSettings: TCodeFormatterEngineSettings; override;
    function GetResultDir: string; override;
  end;

implementation

uses
  Dialogs,
  GX_CodeFormatterConfigHandler;

{ TTestTestfiles }

procedure TTestTestfiles.SetUp;
var
  Settings: TCodeFormatterEngineSettings;
begin
  inherited;
  Settings := GetFormatSettings;
  FFormatter := TCodeFormatterEngine.Create;
  FFormatter.Settings.Settings := Settings;
end;

procedure TTestTestfiles.TearDown;
begin
  inherited;
  FFormatter.Free;
end;

procedure TTestTestfiles.TestFile(const _Filename: string; _AllowFaiure: boolean);
var
  Filename: string;
  InFile: string;
  ExpectedFile: string;
  ExpectedText: TStringList;
  st: TStringList;
begin
  Filename := 'testfile_' + _Filename + '.pas';
  InFile := 'unittests\testcases\input\' + Filename;
  ExpectedFile := 'unittests\testcases\expected-' + GetResultDir + '\' + Filename;
  ExpectedText := nil;
  st := TStringList.Create;
  try
    st.LoadFromFile(InFile);
    ExpectedText := TStringList.Create;
    ExpectedText.LoadFromFile(ExpectedFile);
    FFormatter.Execute(st);
    try
      CheckEquals(ExpectedText.Text, st.Text, 'error in output');
    except
      st.SaveToFile('unittests\testcases\output-' + GetResultDir + '\' + Filename);
      if not _AllowFaiure then
        raise;
    end;
  finally
    ExpectedText.Free;
    st.Free;
  end;
end;

procedure TTestTestfiles.testStarCommentAtEol;
begin
  TestFile('StarCommentAtEol');
end;

procedure TTestTestfiles.testStrictVisibility;
begin
  TestFile('strictvisibility');
end;

procedure TTestTestfiles.testCompilerDirectives;
begin
  TestFile('compilerdirectives');
end;

procedure TTestTestfiles.testAssemblerNewlines;
begin
  TestFile('assemblernewline');
end;

procedure TTestTestfiles.testIfdefs;
begin
  TestFile('ifdefs');
end;

procedure TTestTestfiles.testUnmatchedIfdef;
begin
  TestFile('unmatchedifdef');
end;

procedure TTestTestfiles.testUnmatchedElse;
begin
  TestFile('unmatchedelse');
end;

procedure TTestTestfiles.testUnmatchedEndif;
begin
  TestFile('unmatchedendif');
end;

procedure TTestTestfiles.testIfElseendif;
begin
  TestFile('IfElseEndif');
end;

procedure TTestTestfiles.testIfThenElse;
begin
  TestFile('ifthenelse');
end;

procedure TTestTestfiles.testIfThenTry;
begin
  Testfile('ifthentry');
end;

procedure TTestTestfiles.testDisableFormatComment;
begin
  Testfile('DisableFormatComment');
end;

procedure TTestTestfiles.testLargeFile;
begin
  Testfile('xdom_3_1');
end;

procedure TTestTestfiles.testNestedClassCurrentlyFails;
begin
  Testfile('NestedClass', true);
end;

procedure TTestTestfiles.testOperatorOverloading;
begin
  Testfile('OperatorOverloading');
end;

procedure TTestTestfiles.testTripleQuotes;
begin
  Testfile('triplequotes');
end;

procedure TTestTestfiles.testSingleElse;
begin
  TestFile('singleelse');
end;

procedure TTestTestfiles.testQuotesError;
begin
  TestFile('QuotesError');
end;

procedure TTestTestfiles.testRecordMethod;
begin
  TestFile('RecordMethod');
end;

procedure TTestTestfiles.testJustOpeningComment;
begin
  TestFile('OpeningCommentOnly');
end;

procedure TTestTestfiles.testElseAtEnd;
begin
  TestFile('ElseAtEnd');
end;

procedure TTestTestfiles.testJustOpeningStarCommentInAsm;
begin
  // I actually thought this would crash...
  TestFile('OpeningStarCommentInAsm');
end;

procedure TTestTestfiles.testTabBeforeEndInAsm;
begin
  TestFile('TabBeforeEndInAsm');
end;

procedure TTestTestfiles.testEmptyProgram;
begin
  TestFile('EmptyProgram');
end;

procedure TTestTestfiles.testEmptyUnit;
begin
  TestFile('EmptyUnit');
end;

procedure TTestTestfiles.testIndentComment;
begin
  TestFile('IndentComment');
end;

procedure TTestTestfiles.testUnterminatedString;
begin
  TestFile('UnterminatedString');
end;

procedure TTestTestfiles.testStringWithSingleQuotes;
begin
  // note this actually contains a string with the TEXT #13#10:
  // >hello ' #13#10);<
  TestFile('StringWithSingleQuote');
end;

procedure TTestTestfiles.testEmptyStringAssignment;
begin
  TestFile('EmptyStringAssignment');
end;

procedure TTestTestfiles.testHashCharStrings;
begin
  TestFile('HashCharStrings');
end;

procedure TTestTestfiles.testDotFloat;
begin
  TestFile('DotFloat');
end;

procedure TTestTestfiles.testHexNumbers;
begin
  TestFile('HexNumbers');
end;

procedure TTestTestfiles.testConstSet;
begin
  TestFile('ConstSet');
end;

procedure TTestTestfiles.testConstSetWithComment;
begin
  TestFile('ConstSetWithComment');
end;

procedure TTestTestfiles.testConstSetWithCommentAtEnd;
begin
  TestFile('ConstSetWithCommentAtEnd');
end;

procedure TTestTestfiles.testControlChars;
begin
  TestFile('ControlChars');
end;

procedure TTestTestfiles.testFormula;
begin
  TestFile('Formula');
end;

procedure TTestTestfiles.testAbstractSealedClass;
begin
  TestFile('AbstractSealedClass');
end;

procedure TTestTestfiles.testAsm;
begin
  TestFile('asm');
end;

procedure TTestTestfiles.testAsmProblem1;
begin
  TestFile('AsmProblem1');
end;

procedure TTestTestfiles.testClassPropertiesCurrentlyFails;
begin
  TestFile('ClassProperties', true);
end;

procedure TTestTestfiles.testCommentEnd;
begin
  TestFile('CommentEnd');
end;

procedure TTestTestfiles.testCurlyHalfCommentEndCurrentlyFails;
begin
  TestFile('CurlyHalfCommentEnd', true);
end;

{ TTestFilesHeadworkFormatting }

function TTestFilesHeadworkFormatting.GetFormatSettings: TCodeFormatterEngineSettings;
var
  Settings: TCodeFormatterSettings;
begin
  Settings := TCodeFormatterSettings.Create;
  try
    TCodeFormatterConfigHandler.ImportFromFile('packagewiz\DelForExOptions-headwork.ini', Settings);
    Result := Settings.Settings;
  finally
    Settings.Free;
  end;
end;

function TTestFilesHeadworkFormatting.GetResultDir: string;
begin
  Result := 'headwork';
end;

{ TTestFilesBorlandFormatting }

function TTestFilesBorlandFormatting.GetFormatSettings: TCodeFormatterEngineSettings;
begin
  Result := BorlandDefaults;
end;

function TTestFilesBorlandFormatting.GetResultDir: string;
begin
  Result := 'borland';
end;

{ TTestFilesDelforFormatting }

function TTestFilesDelforFormatting.GetFormatSettings: TCodeFormatterEngineSettings;
var
  Settings: TCodeFormatterSettings;
begin
  Settings := TCodeFormatterSettings.Create;
  try
    TCodeFormatterConfigHandler.ImportFromFile('packagewiz\DelForExOptions-Default.ini', Settings);
    Result := Settings.Settings;
  finally
    Settings.Free;
  end;
end;

function TTestFilesDelforFormatting.GetResultDir: string;
begin
  Result := 'default';
end;

{ TTestFilesTwmFormatting }

function TTestFilesTwmFormatting.GetFormatSettings: TCodeFormatterEngineSettings;
var
  Settings: TCodeFormatterSettings;
begin
  Settings := TCodeFormatterSettings.Create;
  try
    TCodeFormatterConfigHandler.ImportFromFile('packagewiz\DelForExOptions-twm.ini', Settings);
    Result := Settings.Settings;
  finally
    Settings.Free;
  end;
end;

function TTestFilesTwmFormatting.GetResultDir: string;
begin
  Result := 'twm';
end;

initialization
  RegisterTest(TTestFilesHeadworkFormatting.Suite);
  RegisterTest(TTestFilesBorlandFormatting.Suite);
  RegisterTest(TTestFilesDelforFormatting.Suite);
  RegisterTest(TTestFilesTwmFormatting.Suite);
end.

