program TestUnitExportsParser;
{

  Delphi DUnit Test Project
  -------------------------
  This project contains the DUnit test framework and the GUI/Console test runners.
  Add "CONSOLE_TESTRUNNER" to the conditional defines entry in the project options
  to use the console test runner.  Otherwise the GUI test runner will be used by
  default.

}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  GX_UnitExportsParserTest in 'GX_UnitExportsParserTest.pas',
  GX_UnitExportsParser in '..\..\..\Source\Framework\GX_UnitExportsParser.pas',
  DUnitConsts in '..\..\dunit\DUnitConsts.pas',
  GUITestRunner in '..\..\dunit\GUITestRunner.pas' {GUITestRunner},
  TestFramework in '..\..\dunit\TestFramework.pas',
  TestFrameworkExt in '..\..\dunit\TestFrameworkExt.pas',
  GX_UnitExportList in '..\..\..\Source\UsesExpert\GX_UnitExportList.pas';

{$R *.RES}

begin
  GUITestRunner.RunRegisteredTests;
end.

