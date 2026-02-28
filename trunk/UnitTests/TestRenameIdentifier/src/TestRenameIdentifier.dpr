program TestRenameIdentifier;

uses
  Windows,
  SysUtils,
  GX_TestRenameIdentifierTest in 'GX_TestRenameIdentifierTest.pas',
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

procedure Main;
var
  OutputFileName: string;
  Res: TTestResult;
begin
  ReportMemoryLeaksOnShutdown := True;
  if IsConsoleMode(OutputFileName) then begin
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
