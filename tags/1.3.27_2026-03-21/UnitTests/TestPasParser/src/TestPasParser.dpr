program TestPasParser;

uses
  Windows,
  SysUtils,
  PasParserTests in 'PasParserTests.pas',
  mwPasParser in '..\..\..\ExternalSource\mwParser\mwPasParser.pas',
  mwPasParserTypes in '..\..\..\ExternalSource\mwParser\mwPasParserTypes.pas',
  mPasLex in '..\..\..\ExternalSource\mwParser\mPasLex.pas',
  DUnitConsts in '..\..\..\UnitTestsFormatter\dunit\DUnitConsts.pas',
  TestFramework in '..\..\..\UnitTestsFormatter\dunit\TestFramework.pas',
  GUITestRunner in '..\..\..\UnitTestsFormatter\dunit\GUITestRunner.pas',
  TextTestRunner in '..\..\..\UnitTestsFormatter\dunit\TextTestRunner.pas';

function IsConsoleMode(out _Filename: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 1 to ParamCount do begin
    if SameText(ParamStr(i), '-c') or SameText(ParamStr(i), '--console') then begin
      Result := True;
      if i < ParamCount then
        _Filename := ParamStr(i + 1);
      Exit;
    end;
  end;
end;

const
  ATTACH_PARENT_PROCESS = DWORD(-1);

{$IF not Declared(AttachConsole)}
function AttachConsole(dwProcessId: DWORD): BOOL; stdcall; external kernel32 name 'AttachConsole';
{$IFEND}

procedure Main;
var
  OutputFileName: string;
  Res: TTestResult;
begin
  ReportMemoryLeaksOnShutdown := True;
  if IsConsoleMode(OutputFileName) then begin
    if not AttachConsole(ATTACH_PARENT_PROCESS) then
      AllocConsole;
    if OutputFileName <> '' then begin
      WriteLn('Writing results to file ' + OutputFileName);
      AssignFile(Output, OutputFileName);
      Rewrite(Output);
    end;
    Res := TextTestRunner.RunRegisteredTests(rxbContinue);
    try
      ExitCode := Res.ErrorCount + Res.FailureCount;
    finally
      FreeAndNil(Res);
    end;
    if OutputFileName <> '' then
      CloseFile(Output);
  end else
    TGUITestRunner.RunRegisteredTests;
end;

begin
  Main;
end.
