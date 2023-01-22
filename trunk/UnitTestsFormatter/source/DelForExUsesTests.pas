unit DelForExUsesTests;

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

implementation

type
  TFormatterUsesTests = class(TTestCase)
  private
    FFormatter: TCodeFormatterEngine;

    function GenerateUsesSource: TGxUnicodeStringList;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    ///  <summary>
    ///  Basic test for uses indentation, uses the default configuration
    ///  setup in <see cref="DelForExUsesTests|TFormatterUsesTests.SetUp" />.
    ///  </summary>
    ///  <remarks>
    ///  <para>
    ///  Goals:
    ///  </para><para>
    ///  - each unit in a separate line (break after uses)
    ///  </para><para>
    ///  - line starting with comma
    ///  </para><para>
    ///  - separate line for the closing semicolon
    ///  </para><para>
    ///  - only the first unit is indented (the one without comma)
    ///  </para>
    ///  </remarks>
    procedure TestIndentUses;

    ///  <summary>
    ///  Basic test with all uses indentation options off.
    ///  </summary>
    ///  <remarks>
    ///  <para>
    ///  The expected result is the same as the input.
    ///  </remarks>
    procedure TestIndentUsesAllOff;

    ///  <summary>
    ///  <c>NoIndentUsesComma</c> is set to False.
    ///  </summary>
    ///  <remarks>
    ///  <para>
    ///  Goals:
    ///  </para><para>
    ///  - each unit in a separate line (break after uses)
    ///  </para><para>
    ///  - line starting with comma/semicolon
    ///  </para><para>
    ///  - Every unit is indented (with and without comma)
    ///  </para>
    ///  </remarks>
    procedure TestDoNotIndentUsesStartingWithComma1;

    ///  <summary>
    ///  Same test as <see cref="DelForExUsesTests|TFormatterUsesTests.TestDoNotIndentUsesStartingWithComma1" />
    ///  but also sets <c>FeedEachUnitBeforeComma</c> to False.
    ///  </summary>
    ///  <remarks>
    ///  <para>
    ///  Goals:
    ///  </para><para>
    ///  - each unit in a separate line (break after uses)
    ///  </para><para>
    ///  - The comma/semicolon comes after the unit
    ///  </para><para>
    ///  - Every unit is indented (with and without comma)
    ///  </para>
    ///  </remarks>
    procedure TestDoNotIndentUsesStartingWithComma2;

    ///  <summary>
    ///  Sets <c>FeedAfterUses</c> to False.
    ///  </summary>
    ///  <remarks>
    ///  <para>
    ///  Goals:
    ///  </para><para>
    ///  - each unit in a separate line (no break after uses)
    ///  </para><para>
    ///  - line starting with comma/semicolon
    ///  </para><para>
    ///  - Every unit is indented (with and without comma)
    ///  </para>
    ///  </remarks>
    procedure TestNoFeedAfterUses;

    ///  <summary>
    ///  Sets <c>FeedEachUnit</c> to False.
    ///  </summary>
    ///  <remarks>
    ///  <para>
    ///  Goals:
    ///  </para><para>
    ///  - break after uses
    ///  </para><para>
    ///  - Separate line for the closing semicolon
    ///  </para><para>
    ///  - Same layout of the units as before
    ///  </para>
    ///  </remarks>
    procedure TestNoFeedEachUnit1;

    ///  <summary>
    ///  Same Tests as <see cref="DelForExUsesTests|TFormatterUsesTests.TestNoFeedEachUnit1" />,
    ///  but also sets <c>FeedEachUnitBeforeComma</c> to False.
    ///  Should be the same result.
    ///  </summary>
    ///  <remarks>
    ///  <para>
    ///  Goals:
    ///  </para><para>
    ///  - break after uses
    ///  </para><para>
    ///  - Separate line for the closing semicolon
    ///  </para><para>
    ///  - Same layout of the units as before
    ///  </para>
    ///  </remarks>
    procedure TestNoFeedEachUnit2;
  end;

{ TFormatterUsesTests }

function TFormatterUsesTests.GenerateUsesSource: TGxUnicodeStringList;
begin
  Result := TGxUnicodeStringList.Create;

  Result.Add('unit Preview;');
  Result.Add('');
  Result.Add('interface');
  Result.Add('');
  Result.Add('uses Windows, SysUtils, Classes,');
  Result.Add('  Graphics, Forms;');
  Result.Add('');
  Result.Add('implementation');
  Result.Add('');
  Result.Add('uses Windows, SysUtils, Classes,');
  Result.Add('  Graphics, Forms;');
  Result.Add('');
  Result.Add('end.');
end;

procedure TFormatterUsesTests.SetUp;
var
  Settings: TCodeFormatterEngineSettings;
begin
  inherited;

  FFormatter := TCodeFormatterEngine.Create;

  Settings := FFormatter.Settings.Settings;
  Settings.ChangeIndent := True;

  { Indent }
  Settings.SpacePerIndent := 2; { Spaces per indent }

  { Indent / Extra Indent Before }
  Settings.IndentBegin := False; { begin }
  Settings.IndentTry := True; { try }
  Settings.IndentTryElse := False; { else in a try block }
  Settings.IndentCaseElse := False; { else in a case block }

  Settings.NoIndentElseIf := False; { Never indent else if }
  Settings.NoIndentVarDecl := False; { Never indent var declaration }
  Settings.IndentComments := False; { Indent comments }
  Settings.IndentCompDirectives := False; { Indent compiler directives }
  Settings.NoIndentUsesComma := True; { Do not indent uses starting with , }


  { Spacing }
  Settings.SpaceEqualOper := spBoth;
  Settings.SpaceOperators := spBoth;
  Settings.SpaceColon := [spAfter];
  Settings.SpaceSemiColon := [spAfter];
  Settings.SpaceComma := [spAfter];
  Settings.SpaceLeftHook := spNone;
  Settings.SpaceRightHook := spNone;
  Settings.SpaceLeftBr := spNone;
  Settings.SpaceRightBr := spNone;


  { Line Breaks }

  { Line Breaks / Always Break Line }
  Settings.FeedAfterVar := False; { After "var", "type" etc. }
  Settings.FeedBeforeElse := False; { Before "else" }
  Settings.FeedBeforeEnd := False; { Before "end" }
  Settings.FeedAfterSemiColon := False; { After semicolon (except directives) }
  Settings.FeedElseIf := False; { Between else and if }
  Settings.FeedAfterThen := False; { After "then","else","do",":" }
  Settings.ExceptSingle := False; { Except single lines }
  Settings.NoFeedBeforeThen := False; { Never before "then", "do" }
  Settings.FeedAfterUses := True; { After "uses" }
  Settings.FeedEachUnit := True; { Between every unit in "uses" }
  Settings.FeedEachUnitBeforeComma := True; { Before the comma }
  Settings.RemoveDoubleBlank := False; { Remove double blank lines }

  { Line Breaks / Force a Blank Line Between }
  Settings.BlankProc := True; { Main procedures/functions }
  Settings.BlankSubProc := False; { Local procedures/functions }

  Settings.FeedRoundBegin := Unchanged; { Begin style }
  Settings.FeedRoundTry := Unchanged; { Try style }

  Settings.WrapLines := False; { Wrap long lines }
  Settings.WrapPosition := 81; { At position }


  { Capitalization }
  Settings.UpperCompDirectives := True; { Compiler directives }
  Settings.UpperNumbers := True; { Hex numbers }

  Settings.ReservedCase := rfLowerCase; { Reserved words }
  Settings.StandDirectivesCase := rfLowerCase; { Standard directives }
  Settings.IdentifiersCase := rfUnchanged; { Identifiers }

  Settings.FillNewWords := []; { User Defined Capitalization }

  { Align }
  Settings.AlignComments := False; { Align simple comments after code }
  Settings.AlignCommentPos := 40; { At position }

  Settings.AlignVar := 0; { Align var statements }
  Settings.AlignVarPos := 20; { At position }


  { Misc. }
  Settings.EndCommentOut := '{*)}';
  Settings.StartCommentOut := '{(*}';

  FFormatter.Settings.Settings := Settings;
end;

procedure TFormatterUsesTests.TearDown;
begin
  try
    FreeAndNil(FFormatter);
  finally
    inherited;
  end;
end;

procedure TFormatterUsesTests.TestDoNotIndentUsesStartingWithComma1;
var
  ActualText: TGxUnicodeStringList;
  ExpectedText: TGxUnicodeStringList;
  Settings: TCodeFormatterEngineSettings;
begin
  ExpectedText := TGxUnicodeStringList.Create;
  try
    ActualText := GenerateUsesSource;
    try
      Settings := FFormatter.Settings.Settings;
      Settings.NoIndentUsesComma := False; { Do not indent uses starting with , }
      FFormatter.Settings.Settings := Settings;

      ExpectedText.Add('unit Preview;');
      ExpectedText.Add('');
      ExpectedText.Add('interface');
      ExpectedText.Add('');
      ExpectedText.Add('uses');
      ExpectedText.Add('  Windows');
      ExpectedText.Add('  , SysUtils');
      ExpectedText.Add('  , Classes');
      ExpectedText.Add('  , Graphics');
      ExpectedText.Add('  , Forms');
      ExpectedText.Add('  ;');
      ExpectedText.Add('');
      ExpectedText.Add('implementation');
      ExpectedText.Add('');
      ExpectedText.Add('uses');
      ExpectedText.Add('  Windows');
      ExpectedText.Add('  , SysUtils');
      ExpectedText.Add('  , Classes');
      ExpectedText.Add('  , Graphics');
      ExpectedText.Add('  , Forms');
      ExpectedText.Add('  ;');
      ExpectedText.Add('');
      ExpectedText.Add('end.');

      FFormatter.Execute(ActualText);

      CheckEquals(ExpectedText.Text, ActualText.Text, 'error in output');
    finally
      FreeAndNil(ActualText);
    end;
  finally
    FreeAndNil(ExpectedText);
  end;
end;

procedure TFormatterUsesTests.TestDoNotIndentUsesStartingWithComma2;
var
  ActualText: TGxUnicodeStringList;
  ExpectedText: TGxUnicodeStringList;
  Settings: TCodeFormatterEngineSettings;
begin
  ExpectedText := TGxUnicodeStringList.Create;
  try
    ActualText := GenerateUsesSource;
    try
      Settings := FFormatter.Settings.Settings;
      Settings.NoIndentUsesComma := False; { Do not indent uses starting with , }
      Settings.FeedEachUnitBeforeComma := False; { Before the comma }
      FFormatter.Settings.Settings := Settings;

      ExpectedText.Clear;
      ExpectedText.Add('unit Preview;');
      ExpectedText.Add('');
      ExpectedText.Add('interface');
      ExpectedText.Add('');
      ExpectedText.Add('uses');
      ExpectedText.Add('  Windows,');
      ExpectedText.Add('  SysUtils,');
      ExpectedText.Add('  Classes,');
      ExpectedText.Add('  Graphics,');
      ExpectedText.Add('  Forms;');
      ExpectedText.Add('');
      ExpectedText.Add('implementation');
      ExpectedText.Add('');
      ExpectedText.Add('uses');
      ExpectedText.Add('  Windows,');
      ExpectedText.Add('  SysUtils,');
      ExpectedText.Add('  Classes,');
      ExpectedText.Add('  Graphics,');
      ExpectedText.Add('  Forms;');
      ExpectedText.Add('');
      ExpectedText.Add('end.');

      FFormatter.Execute(ActualText);

      CheckEquals(ExpectedText.Text, ActualText.Text, 'error in output');

    finally
      FreeAndNil(ActualText);
    end;
  finally
    FreeAndNil(ExpectedText);
  end;
end;

procedure TFormatterUsesTests.TestNoFeedAfterUses;
var
  ActualText: TGxUnicodeStringList;
  ExpectedText: TGxUnicodeStringList;
  Settings: TCodeFormatterEngineSettings;
begin
  ExpectedText := TGxUnicodeStringList.Create;
  try
    ActualText := GenerateUsesSource;
    try
      Settings := FFormatter.Settings.Settings;
      Settings.FeedAfterUses := False; { After "uses" }
      FFormatter.Settings.Settings := Settings;

      ExpectedText.Add('unit Preview;');
      ExpectedText.Add('');
      ExpectedText.Add('interface');
      ExpectedText.Add('');
      ExpectedText.Add('uses Windows');
      ExpectedText.Add(', SysUtils');
      ExpectedText.Add(', Classes');
      ExpectedText.Add(', Graphics');
      ExpectedText.Add(', Forms');
      ExpectedText.Add(';');
      ExpectedText.Add('');
      ExpectedText.Add('implementation');
      ExpectedText.Add('');
      ExpectedText.Add('uses Windows');
      ExpectedText.Add(', SysUtils');
      ExpectedText.Add(', Classes');
      ExpectedText.Add(', Graphics');
      ExpectedText.Add(', Forms');
      ExpectedText.Add(';');
      ExpectedText.Add('');
      ExpectedText.Add('end.');

      FFormatter.Execute(ActualText);

      CheckEquals(ExpectedText.Text, ActualText.Text, 'Feed after uses');
    finally
      FreeAndNil(ActualText);
    end;
  finally
    FreeAndNil(ExpectedText);
  end;
end;

procedure TFormatterUsesTests.TestNoFeedEachUnit1;
var
  ActualText: TGxUnicodeStringList;
  ExpectedText: TGxUnicodeStringList;
  Settings: TCodeFormatterEngineSettings;
begin
  ExpectedText := TGxUnicodeStringList.Create;
  try
    ActualText := GenerateUsesSource;
    try
      Settings := FFormatter.Settings.Settings;
      Settings.FeedEachUnit := False; { Between every unit in "uses" }
      FFormatter.Settings.Settings := Settings;

      ExpectedText.Add('unit Preview;');
      ExpectedText.Add('');
      ExpectedText.Add('interface');
      ExpectedText.Add('');
      ExpectedText.Add('uses');
      ExpectedText.Add('  Windows, SysUtils, Classes,');
      ExpectedText.Add('  Graphics, Forms;');
      ExpectedText.Add('');
      ExpectedText.Add('implementation');
      ExpectedText.Add('');
      ExpectedText.Add('uses');
      ExpectedText.Add('  Windows, SysUtils, Classes,');
      ExpectedText.Add('  Graphics, Forms;');
      ExpectedText.Add('');
      ExpectedText.Add('end.');

      FFormatter.Execute(ActualText);

      CheckEquals(ExpectedText.Text, ActualText.Text, 'Feed after uses');
    finally
      FreeAndNil(ActualText);
    end;
  finally
    FreeAndNil(ExpectedText);
  end;
end;

procedure TFormatterUsesTests.TestNoFeedEachUnit2;
var
  ActualText: TGxUnicodeStringList;
  ExpectedText: TGxUnicodeStringList;
  Settings: TCodeFormatterEngineSettings;
begin
  ExpectedText := TGxUnicodeStringList.Create;
  try
    ActualText := GenerateUsesSource;
    try
      Settings := FFormatter.Settings.Settings;
      Settings.FeedEachUnit := False; { Between every unit in "uses" }
      Settings.FeedEachUnitBeforeComma := False; { Before the comma }
      FFormatter.Settings.Settings := Settings;

      ExpectedText.Add('unit Preview;');
      ExpectedText.Add('');
      ExpectedText.Add('interface');
      ExpectedText.Add('');
      ExpectedText.Add('uses');
      ExpectedText.Add('  Windows, SysUtils, Classes,');
      ExpectedText.Add('  Graphics, Forms;');
      ExpectedText.Add('');
      ExpectedText.Add('implementation');
      ExpectedText.Add('');
      ExpectedText.Add('uses');
      ExpectedText.Add('  Windows, SysUtils, Classes,');
      ExpectedText.Add('  Graphics, Forms;');
      ExpectedText.Add('');
      ExpectedText.Add('end.');

      FFormatter.Execute(ActualText);

      CheckEquals(ExpectedText.Text, ActualText.Text, 'Feed after uses');
    finally
      FreeAndNil(ActualText);
    end;
  finally
    FreeAndNil(ExpectedText);
  end;
end;

procedure TFormatterUsesTests.TestIndentUses;
var
  ActualText: TGxUnicodeStringList;
  ExpectedText: TGxUnicodeStringList;
  Settings: TCodeFormatterEngineSettings;
begin
  ExpectedText := TGxUnicodeStringList.Create;
  try
    ActualText := GenerateUsesSource;
    try
      ExpectedText.Add('unit Preview;');
      ExpectedText.Add('');
      ExpectedText.Add('interface');
      ExpectedText.Add('');
      ExpectedText.Add('uses');
      ExpectedText.Add('  Windows');
      ExpectedText.Add(', SysUtils');
      ExpectedText.Add(', Classes');
      ExpectedText.Add(', Graphics');
      ExpectedText.Add(', Forms');
      ExpectedText.Add(';');
      ExpectedText.Add('');
      ExpectedText.Add('implementation');
      ExpectedText.Add('');
      ExpectedText.Add('uses');
      ExpectedText.Add('  Windows');
      ExpectedText.Add(', SysUtils');
      ExpectedText.Add(', Classes');
      ExpectedText.Add(', Graphics');
      ExpectedText.Add(', Forms');
      ExpectedText.Add(';');
      ExpectedText.Add('');
      ExpectedText.Add('end.');

      FFormatter.Execute(ActualText);

      CheckEquals(ExpectedText.Text, ActualText.Text, 'Test with "default" options');
    finally
      FreeAndNil(ActualText);
    end;
  finally
    FreeAndNil(ExpectedText);
  end;
end;

procedure TFormatterUsesTests.TestIndentUsesAllOff;
var
  ActualText: TGxUnicodeStringList;
  ExpectedText: TGxUnicodeStringList;
  Settings: TCodeFormatterEngineSettings;
begin
  ExpectedText := GenerateUsesSource;
  try
    ActualText := GenerateUsesSource;
    try
      Settings := FFormatter.Settings.Settings;
      Settings.NoIndentUsesComma := False; { Do not indent uses starting with , }
      Settings.FeedAfterUses := False; { After "uses" }
      Settings.FeedEachUnit := False; { Between every unit in "uses" }
      Settings.FeedEachUnitBeforeComma := False; { Before the comma }
      FFormatter.Settings.Settings := Settings;

      FFormatter.Execute(ActualText);

      CheckEquals(ExpectedText.Text, ActualText.Text, 'error in output');
    finally
      FreeAndNil(ActualText);
    end;
  finally
    FreeAndNil(ExpectedText);
  end;
end;

initialization

  RegisterTest(TFormatterUsesTests.Suite);

end.
