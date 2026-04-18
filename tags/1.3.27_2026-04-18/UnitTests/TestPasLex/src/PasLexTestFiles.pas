unit PasLexTestFiles;

interface

uses
  SysUtils,
  Classes,
  TestFramework,
  TestFrameworkExt;

type
  TTestPasLex = class(TFileTestCase)
  published
    procedure TestTokenize;
  end;

function Suite: ITestSuite;

implementation

uses
  TypInfo,
  mPasLex,
  mwPasParserTypes;

function EscapeTokenText(const S: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(S) do
    case S[I] of
      #13: Result := Result + '\r';
      #10: Result := Result + '\n';
      #9:  Result := Result + '\t';
      '\': Result := Result + '\\';
    else
      Result := Result + S[I];
    end;
end;

function TokenizeToString(const Source: string): string;
var
  Lexer: TmwPasLex;
  Lines: TStringList;
  TokenName: string;
begin
  Lines := TStringList.Create;
  try
    Lexer := TmwPasLex.Create;
    try
      Lexer.Origin := PChar(Source);
      while Lexer.TokenID <> tkNull do begin
        TokenName := GetEnumName(TypeInfo(TTokenKind), Ord(Lexer.TokenID));
        Lines.Add(Format('%d'#9'%s'#9'%s', [
          Lexer.LineNumber + 1,
          TokenName,
          EscapeTokenText(Lexer.Token)
        ]));
        Lexer.Next;
      end;
    finally
      Lexer.Free;
    end;
    Result := Lines.Text;
  finally
    Lines.Free;
  end;
end;

procedure TTestPasLex.TestTokenize;
var
  InputFile, ExpectedFile, OutputFile: string;
  InputText, Expected, Actual: TStringList;
  ActualOutput, ExpectedOutput: string;
begin
  InputFile := TestFileName;
  ExpectedFile := StringReplace(InputFile, '\input\', '\expected\', []);
  ExpectedFile := ChangeFileExt(ExpectedFile, '.txt');
  OutputFile := StringReplace(InputFile, '\input\', '\output\', []);
  OutputFile := ChangeFileExt(OutputFile, '.txt');

  InputText := TStringList.Create;
  try
    InputText.LoadFromFile(InputFile);
    ActualOutput := TokenizeToString(InputText.Text);
  finally
    InputText.Free;
  end;

  if not FileExists(ExpectedFile) then begin
    ForceDirectories(ExtractFilePath(ExpectedFile));
    Actual := TStringList.Create;
    try
      Actual.Text := ActualOutput;
      Actual.SaveToFile(ExpectedFile);
    finally
      Actual.Free;
    end;
    Status('Warning: Expected file created from actual output');
  end;

  Expected := TStringList.Create;
  try
    Expected.LoadFromFile(ExpectedFile);
    ExpectedOutput := Expected.Text;
  finally
    Expected.Free;
  end;

  if ActualOutput <> ExpectedOutput then begin
    ForceDirectories(ExtractFilePath(OutputFile));
    Actual := TStringList.Create;
    try
      Actual.Text := ActualOutput;
      Actual.SaveToFile(OutputFile);
    finally
      Actual.Free;
    end;
  end;

  CheckEquals(ExpectedOutput, ActualOutput, 'Token output mismatch');
end;

function Suite: ITestSuite;
begin
  Result := TFolderTestSuite.Create(TTestPasLex, 'Testcases\input', '*.pas', False);
end;

initialization
  RegisterTest(Suite);
end.
