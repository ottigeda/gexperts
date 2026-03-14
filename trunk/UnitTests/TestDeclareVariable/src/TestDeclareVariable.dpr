program TestDeclareVariable;

uses
  Windows,
  SysUtils,
  GX_TestDeclareVariableTest in 'GX_TestDeclareVariableTest.pas',
  GX_DeclareVariable in '..\..\..\Source\DeclareVariable\GX_DeclareVariable.pas',
  GX_PascalTokenAnalyzer in '..\..\..\Source\Utils\GX_PascalTokenAnalyzer.pas',
  GX_RenameIdentifier in '..\..\..\Source\Editor\GX_RenameIdentifier.pas',
  mPasLex in '..\..\..\ExternalSource\mwParser\mPasLex.pas',
  mwPasParserTypes in '..\..\..\ExternalSource\mwParser\mwPasParserTypes.pas',
  DUnitConsts in '..\..\..\UnitTestsFormatter\dunit\DUnitConsts.pas',
  TestFramework in '..\..\..\UnitTestsFormatter\dunit\TestFramework.pas',
  TestFrameworkExt in '..\..\..\UnitTestsFormatter\dunit\TestFrameworkExt.pas',
  GUITestRunner in '..\..\..\UnitTestsFormatter\dunit\GUITestRunner.pas',
  TextTestRunner in '..\..\..\UnitTestsFormatter\dunit\TextTestRunner.pas';

function IsConsoleMode(out _Filename: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  _Filename := '';
  for I := 1 to ParamCount do begin
    if SameText(ParamStr(I), '-c') or SameText(ParamStr(I), '--console') then begin
      Result := True;
      if I < ParamCount then
        _Filename := ParamStr(I + 1);
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
