unit FileScannerTestFiles;

interface

uses
  SysUtils,
  Classes,
  TestFramework,
  TestFrameworkExt;

type
  TTestFileScanner = class(TFileTestCase)
  published
    procedure TestScan;
  end;

function Suite: ITestSuite;

implementation

uses
  GX_FileScanner;

function ScanToString(const Source: string): string;
var
  Scanner: TFileScanner;
  Procs: TProcedures;
  Proc: TProcedure;
  Lines: TStringList;
  I: Integer;
  BodyLineCount: Integer;
  Body: string;
begin
  Lines := TStringList.Create;
  try
    Scanner := TFileScanner.CreateWithFileName(nil, 'test.pas');
    try
      Scanner.Language := ltPas;
      Scanner.UnitText := Source;
      Scanner.Execute;
      Procs := Scanner.Procedures;
      for I := 0 to Procs.Count - 1 do begin
        Proc := Procs[I];
        Body := Proc.Body;
        // Count lines in Body
        if Body = '' then
          BodyLineCount := 0
        else if Pos(#13#10, Body) > 0 then begin
          with TStringList.Create do
          try
            Text := Body;
            BodyLineCount := Count;
          finally
            Free;
          end;
        end else
          BodyLineCount := 1;
        Lines.Add(Format('%d'#9'%s'#9'%s'#9'%s'#9'%d', [
          Proc.LineNo,
          Proc.ProcedureType,
          Proc.ProcClass,
          Proc.ProcName,
          BodyLineCount
        ]));
      end;
    finally
      Scanner.Free;
    end;
    Result := Lines.Text;
  finally
    Lines.Free;
  end;
end;

procedure TTestFileScanner.TestScan;
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
    ActualOutput := ScanToString(InputText.Text);
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

  CheckEquals(ExpectedOutput, ActualOutput, 'Scanner output mismatch');
end;

function Suite: ITestSuite;
begin
  Result := TFolderTestSuite.Create(TTestFileScanner, 'Testcases\input', '*.pas', False);
end;

initialization
  RegisterTest(Suite);
end.
