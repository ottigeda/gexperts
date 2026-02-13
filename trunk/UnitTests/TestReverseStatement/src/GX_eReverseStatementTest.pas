unit GX_eReverseStatementTest;

interface

uses
  TestFramework,
  Classes,
  GX_ReverseStatementUtils;

type
  TTestReverseForLoop = class(TTestCase)
  published
    procedure TestSimpleForTo;
    procedure TestSimpleForDownto;
    procedure TestForWithLeadingCode;
    procedure TestForWithTrailingComment;
    procedure TestForCaseInsensitive;
    procedure TestNoForLoop;
  end;

  TTestReverseFunctionPair = class(TTestCase)
  private
    FRules: TReversalRuleList;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // Simple conversion pair tests
    procedure TestSimpleReplacement;
    procedure TestReverseDirection;
    procedure TestCaseInsensitive;
    procedure TestNoMatch;
    procedure TestEmptyRules;
    procedure TestWithLeadingWhitespace;
    procedure TestPreservesTrailingComment;
    procedure TestNestedParenthesesSkipped;
    procedure TestStringLiteralArgSkipped;
    // Read/Write pattern tests
    procedure TestReadStringToWriteString;
    procedure TestWriteStringToReadString;
    procedure TestReadStringLiteralParams;
    procedure TestWriteStringComplexParamSkipped;
    procedure TestReadIntegerToWriteInteger;
    procedure TestWriteIntegerToReadInteger;
    procedure TestReadBoolToWriteBool;
    procedure TestWriteBoolToReadBool;
    procedure TestReadFloatToWriteFloat;
    procedure TestWriteFloatToReadFloat;
    procedure TestReadDateToWriteDate;
    procedure TestWriteDateToReadDate;
    procedure TestReadDateTimeToWriteDateTime;
    procedure TestWriteDateTimeToReadDateTime;
    procedure TestReadTimeToWriteTime;
    procedure TestWriteTimeToReadTime;
    // LoadFromStrings / SaveToStrings
    procedure TestLoadFromStrings;
    procedure TestLoadFromStringsSkipsComments;
    procedure TestSaveToStrings;
    procedure TestNoMatchReturnsOriginal;
  end;

  TTestReverseAssignment = class(TTestCase)
  published
    procedure TestDelphiAssignment;
    procedure TestDelphiWithSpaces;
    procedure TestCppAssignment;
    procedure TestDelphiWithComment;
    procedure TestNoAssignment;
    procedure TestWithLeadingWhitespace;
  end;

implementation

uses
  SysUtils;

const
  TC = '(\s*(?://.*|\{[^}]*\}|\(\*.*?\*\))?)';

{ TTestReverseForLoop }

procedure TTestReverseForLoop.TestSimpleForTo;
var
  S: string;
begin
  S := 'for i := 0 to 99 do';
  CheckTrue(ReverseForLoopStr(S), 'Should return True');
  CheckEquals('for i := 99 downto 0 do', S);
end;

procedure TTestReverseForLoop.TestSimpleForDownto;
var
  S: string;
begin
  S := 'for i := 99 downto 0 do';
  CheckTrue(ReverseForLoopStr(S), 'Should return True');
  CheckEquals('for i := 0 to 99 do', S);
end;

procedure TTestReverseForLoop.TestForWithLeadingCode;
var
  S: string;
begin
  S := 'begin for i := 0 to 10 do';
  CheckTrue(ReverseForLoopStr(S), 'Should return True');
  CheckEquals('begin for i := 10 downto 0 do', S);
end;

procedure TTestReverseForLoop.TestForWithTrailingComment;
var
  S: string;
begin
  S := 'for i := 0 to 10 do // comment';
  CheckTrue(ReverseForLoopStr(S), 'Should return True');
  CheckEquals('for i := 10 downto 0 do // comment', S);
end;

procedure TTestReverseForLoop.TestForCaseInsensitive;
var
  S: string;
begin
  S := 'FOR I := 0 TO 10 DO';
  CheckTrue(ReverseForLoopStr(S), 'Should return True');
  CheckEquals('FOR I := 10 downto 0 DO', S);
end;

procedure TTestReverseForLoop.TestNoForLoop;
var
  S, Original: string;
begin
  S := 'WriteLn(Hello);';
  Original := S;
  CheckFalse(ReverseForLoopStr(S), 'Should return False');
  CheckEquals(Original, S, 'String should be unchanged');
end;

{ TTestReverseFunctionPair }

procedure TTestReverseFunctionPair.SetUp;
begin
  inherited;
  FRules := TReversalRuleList.Create;
  // IntToStr <-> StrToInt (simple pair, arg restricted to identifiers)
  FRules.Add(
    '(\s*)([\w.]+)\s*:=\s*([\w.]*?)IntToStr\(([\w.]+)\)\s*;' + TC,
    '$1$4 := $3StrToInt($2);$5');
  FRules.Add(
    '(\s*)([\w.]+)\s*:=\s*([\w.]*?)StrToInt\(([\w.]+)\)\s*;' + TC,
    '$1$4 := $3IntToStr($2);$5');
  // ReadString <-> WriteString (3-param Read/Write pair)
  FRules.Add(
    '(\s*)([\w.]+)\s*:=\s*([\w.]*?)ReadString\(\s*([^,()]+?)\s*,\s*([^,()]+?)\s*,\s*([^()]+?)\s*\)\s*;' + TC,
    '$1$3WriteString($4, $5, $2);$7');
  FRules.Add(
    '(\s*)([\w.]*?)WriteString\(\s*([^,()]+?)\s*,\s*([^,()]+?)\s*,\s*([\w.]+)\s*\)\s*;' + TC,
    '$1$5 := $2ReadString($3, $4, '''');$6');
  // ReadInteger <-> WriteInteger
  FRules.Add(
    '(\s*)([\w.]+)\s*:=\s*([\w.]*?)ReadInteger\(\s*([^,()]+?)\s*,\s*([^,()]+?)\s*,\s*([^()]+?)\s*\)\s*;' + TC,
    '$1$3WriteInteger($4, $5, $2);$7');
  FRules.Add(
    '(\s*)([\w.]*?)WriteInteger\(\s*([^,()]+?)\s*,\s*([^,()]+?)\s*,\s*([\w.]+)\s*\)\s*;' + TC,
    '$1$5 := $2ReadInteger($3, $4, 0);$6');
  // ReadBool <-> WriteBool
  FRules.Add(
    '(\s*)([\w.]+)\s*:=\s*([\w.]*?)ReadBool\(\s*([^,()]+?)\s*,\s*([^,()]+?)\s*,\s*([^()]+?)\s*\)\s*;' + TC,
    '$1$3WriteBool($4, $5, $2);$7');
  FRules.Add(
    '(\s*)([\w.]*?)WriteBool\(\s*([^,()]+?)\s*,\s*([^,()]+?)\s*,\s*([\w.]+)\s*\)\s*;' + TC,
    '$1$5 := $2ReadBool($3, $4, False);$6');
  // ReadFloat <-> WriteFloat
  FRules.Add(
    '(\s*)([\w.]+)\s*:=\s*([\w.]*?)ReadFloat\(\s*([^,()]+?)\s*,\s*([^,()]+?)\s*,\s*([^()]+?)\s*\)\s*;' + TC,
    '$1$3WriteFloat($4, $5, $2);$7');
  FRules.Add(
    '(\s*)([\w.]*?)WriteFloat\(\s*([^,()]+?)\s*,\s*([^,()]+?)\s*,\s*([\w.]+)\s*\)\s*;' + TC,
    '$1$5 := $2ReadFloat($3, $4, 0);$6');
  // ReadDate <-> WriteDate
  FRules.Add(
    '(\s*)([\w.]+)\s*:=\s*([\w.]*?)ReadDate\(\s*([^,()]+?)\s*,\s*([^,()]+?)\s*,\s*([^()]+?)\s*\)\s*;' + TC,
    '$1$3WriteDate($4, $5, $2);$7');
  FRules.Add(
    '(\s*)([\w.]*?)WriteDate\(\s*([^,()]+?)\s*,\s*([^,()]+?)\s*,\s*([\w.]+)\s*\)\s*;' + TC,
    '$1$5 := $2ReadDate($3, $4, Date);$6');
  // ReadDateTime <-> WriteDateTime
  FRules.Add(
    '(\s*)([\w.]+)\s*:=\s*([\w.]*?)ReadDateTime\(\s*([^,()]+?)\s*,\s*([^,()]+?)\s*,\s*([^()]+?)\s*\)\s*;' + TC,
    '$1$3WriteDateTime($4, $5, $2);$7');
  FRules.Add(
    '(\s*)([\w.]*?)WriteDateTime\(\s*([^,()]+?)\s*,\s*([^,()]+?)\s*,\s*([\w.]+)\s*\)\s*;' + TC,
    '$1$5 := $2ReadDateTime($3, $4, Now);$6');
  // ReadTime <-> WriteTime
  FRules.Add(
    '(\s*)([\w.]+)\s*:=\s*([\w.]*?)ReadTime\(\s*([^,()]+?)\s*,\s*([^,()]+?)\s*,\s*([^()]+?)\s*\)\s*;' + TC,
    '$1$3WriteTime($4, $5, $2);$7');
  FRules.Add(
    '(\s*)([\w.]*?)WriteTime\(\s*([^,()]+?)\s*,\s*([^,()]+?)\s*,\s*([\w.]+)\s*\)\s*;' + TC,
    '$1$5 := $2ReadTime($3, $4, Time);$6');
end;

procedure TTestReverseFunctionPair.TearDown;
begin
  FreeAndNil(FRules);
  inherited;
end;

procedure TTestReverseFunctionPair.TestSimpleReplacement;
var
  S: string;
begin
  S := 'S := IntToStr(X);';
  CheckTrue(ReverseFunctionPairStr(S, FRules), 'Should return True');
  CheckEquals('X := StrToInt(S);', S);
end;

procedure TTestReverseFunctionPair.TestReverseDirection;
var
  S: string;
begin
  S := 'X := StrToInt(S);';
  CheckTrue(ReverseFunctionPairStr(S, FRules), 'Should return True');
  CheckEquals('S := IntToStr(X);', S);
end;

procedure TTestReverseFunctionPair.TestCaseInsensitive;
var
  S: string;
begin
  S := 'Result := inttostr(X);';
  CheckTrue(ReverseFunctionPairStr(S, FRules), 'Should return True');
  CheckEquals('X := StrToInt(Result);', S);
end;

procedure TTestReverseFunctionPair.TestNoMatch;
var
  S, Original: string;
begin
  S := 'Result := SomeOtherFunc(X);';
  Original := S;
  CheckFalse(ReverseFunctionPairStr(S, FRules), 'Should return False');
  CheckEquals(Original, S, 'String should be unchanged');
end;

procedure TTestReverseFunctionPair.TestEmptyRules;
var
  EmptyRules: TReversalRuleList;
  S, Original: string;
begin
  EmptyRules := TReversalRuleList.Create;
  try
    S := 'Result := IntToStr(X);';
    Original := S;
    CheckFalse(ReverseFunctionPairStr(S, EmptyRules), 'Should return False');
    CheckEquals(Original, S, 'String should be unchanged');
  finally
    FreeAndNil(EmptyRules);
  end;
end;

procedure TTestReverseFunctionPair.TestWithLeadingWhitespace;
var
  S: string;
begin
  S := '  S := IntToStr(X);';
  CheckTrue(ReverseFunctionPairStr(S, FRules), 'Should return True');
  CheckEquals('  X := StrToInt(S);', S);
end;

procedure TTestReverseFunctionPair.TestPreservesTrailingComment;
var
  S: string;
begin
  S := '  S := IntToStr(X); // convert';
  CheckTrue(ReverseFunctionPairStr(S, FRules), 'Should return True');
  CheckEquals('  X := StrToInt(S); // convert', S);
end;

procedure TTestReverseFunctionPair.TestNestedParenthesesSkipped;
var
  S, Original: string;
begin
  // Nested parens won't match [^()]+ so regex won't match — no reversal
  S := 'S := IntToStr(Abs(X));';
  Original := S;
  CheckFalse(ReverseFunctionPairStr(S, FRules), 'Should return False');
  CheckEquals(Original, S, 'String should be unchanged');
end;

procedure TTestReverseFunctionPair.TestStringLiteralArgSkipped;
var
  S, Original: string;
begin
  // String literal can't be an LHS, so reversal must not happen
  S := 'X := StrToInt(''hallo'');';
  Original := S;
  CheckFalse(ReverseFunctionPairStr(S, FRules), 'Should return False');
  CheckEquals(Original, S, 'String should be unchanged');
end;

procedure TTestReverseFunctionPair.TestReadStringToWriteString;
var
  S: string;
begin
  S := '  s := Ini.ReadString(Section, Key, Default);';
  CheckTrue(ReverseFunctionPairStr(S, FRules), 'Should return True');
  CheckEquals('  Ini.WriteString(Section, Key, s);', S);
end;

procedure TTestReverseFunctionPair.TestWriteStringToReadString;
var
  S: string;
begin
  S := '  Ini.WriteString(Section, Key, Value);';
  CheckTrue(ReverseFunctionPairStr(S, FRules), 'Should return True');
  CheckEquals('  Value := Ini.ReadString(Section, Key, '''');', S);
end;

procedure TTestReverseFunctionPair.TestReadStringLiteralParams;
var
  S: string;
begin
  S := '  s := Ini.ReadString(''Sect'', ''Key'', '''');';
  CheckTrue(ReverseFunctionPairStr(S, FRules), 'Should return True');
  CheckEquals('  Ini.WriteString(''Sect'', ''Key'', s);', S);
end;

procedure TTestReverseFunctionPair.TestWriteStringComplexParamSkipped;
var
  S, Original: string;
begin
  // Function call in param won't match [^,()]+ so regex won't match
  S := '  Ini.WriteString(GetSect(), Key, Value);';
  Original := S;
  CheckFalse(ReverseFunctionPairStr(S, FRules), 'Should return False');
  CheckEquals(Original, S, 'String should be unchanged');
end;

procedure TTestReverseFunctionPair.TestReadIntegerToWriteInteger;
var
  S: string;
begin
  S := '  N := Reg.ReadInteger(Section, Key, 0);';
  CheckTrue(ReverseFunctionPairStr(S, FRules), 'Should return True');
  CheckEquals('  Reg.WriteInteger(Section, Key, N);', S);
end;

procedure TTestReverseFunctionPair.TestWriteIntegerToReadInteger;
var
  S: string;
begin
  S := '  Reg.WriteInteger(Section, Key, N);';
  CheckTrue(ReverseFunctionPairStr(S, FRules), 'Should return True');
  CheckEquals('  N := Reg.ReadInteger(Section, Key, 0);', S);
end;

procedure TTestReverseFunctionPair.TestReadBoolToWriteBool;
var
  S: string;
begin
  S := '  B := Ini.ReadBool(Section, Key, False);';
  CheckTrue(ReverseFunctionPairStr(S, FRules), 'Should return True');
  CheckEquals('  Ini.WriteBool(Section, Key, B);', S);
end;

procedure TTestReverseFunctionPair.TestWriteBoolToReadBool;
var
  S: string;
begin
  S := '  Ini.WriteBool(Section, Key, B);';
  CheckTrue(ReverseFunctionPairStr(S, FRules), 'Should return True');
  CheckEquals('  B := Ini.ReadBool(Section, Key, False);', S);
end;

procedure TTestReverseFunctionPair.TestReadFloatToWriteFloat;
var
  S: string;
begin
  S := '  F := Ini.ReadFloat(Section, Key, 0);';
  CheckTrue(ReverseFunctionPairStr(S, FRules), 'Should return True');
  CheckEquals('  Ini.WriteFloat(Section, Key, F);', S);
end;

procedure TTestReverseFunctionPair.TestWriteFloatToReadFloat;
var
  S: string;
begin
  S := '  Ini.WriteFloat(Section, Key, F);';
  CheckTrue(ReverseFunctionPairStr(S, FRules), 'Should return True');
  CheckEquals('  F := Ini.ReadFloat(Section, Key, 0);', S);
end;

procedure TTestReverseFunctionPair.TestReadDateToWriteDate;
var
  S: string;
begin
  S := '  D := Ini.ReadDate(Section, Key, Date);';
  CheckTrue(ReverseFunctionPairStr(S, FRules), 'Should return True');
  CheckEquals('  Ini.WriteDate(Section, Key, D);', S);
end;

procedure TTestReverseFunctionPair.TestWriteDateToReadDate;
var
  S: string;
begin
  S := '  Ini.WriteDate(Section, Key, D);';
  CheckTrue(ReverseFunctionPairStr(S, FRules), 'Should return True');
  CheckEquals('  D := Ini.ReadDate(Section, Key, Date);', S);
end;

procedure TTestReverseFunctionPair.TestReadDateTimeToWriteDateTime;
var
  S: string;
begin
  S := '  DT := Ini.ReadDateTime(Section, Key, Now);';
  CheckTrue(ReverseFunctionPairStr(S, FRules), 'Should return True');
  CheckEquals('  Ini.WriteDateTime(Section, Key, DT);', S);
end;

procedure TTestReverseFunctionPair.TestWriteDateTimeToReadDateTime;
var
  S: string;
begin
  S := '  Ini.WriteDateTime(Section, Key, DT);';
  CheckTrue(ReverseFunctionPairStr(S, FRules), 'Should return True');
  CheckEquals('  DT := Ini.ReadDateTime(Section, Key, Now);', S);
end;

procedure TTestReverseFunctionPair.TestReadTimeToWriteTime;
var
  S: string;
begin
  S := '  T := Ini.ReadTime(Section, Key, Time);';
  CheckTrue(ReverseFunctionPairStr(S, FRules), 'Should return True');
  CheckEquals('  Ini.WriteTime(Section, Key, T);', S);
end;

procedure TTestReverseFunctionPair.TestWriteTimeToReadTime;
var
  S: string;
begin
  S := '  Ini.WriteTime(Section, Key, T);';
  CheckTrue(ReverseFunctionPairStr(S, FRules), 'Should return True');
  CheckEquals('  T := Ini.ReadTime(Section, Key, Time);', S);
end;

procedure TTestReverseFunctionPair.TestLoadFromStrings;
var
  Lines: TStringList;
  Rules: TReversalRuleList;
  S: string;
begin
  Lines := TStringList.Create;
  Rules := TReversalRuleList.Create;
  try
    Lines.Add('(\s*)([\w.]+)\s*:=\s*([\w.]*?)IntToStr\(([^()]+)\)\s*;' + TC);
    Lines.Add('$1$4 := $3StrToInt($2);$5');
    Rules.LoadFromStrings(Lines);
    CheckEquals(1, Rules.Count, 'Should have 1 rule');

    S := 'X := IntToStr(Y);';
    CheckTrue(ReverseFunctionPairStr(S, Rules), 'Should match');
    CheckEquals('Y := StrToInt(X);', S);
  finally
    FreeAndNil(Rules);
    FreeAndNil(Lines);
  end;
end;

procedure TTestReverseFunctionPair.TestLoadFromStringsSkipsComments;
var
  Lines: TStringList;
  Rules: TReversalRuleList;
begin
  Lines := TStringList.Create;
  Rules := TReversalRuleList.Create;
  try
    Lines.Add('# This is a comment');
    Lines.Add('');
    Lines.Add('(\s*)([\w.]+)\s*:=\s*([\w.]*?)IntToStr\(([^()]+)\)\s*;' + TC);
    Lines.Add('$1$4 := $3StrToInt($2);$5');
    Lines.Add('');
    Lines.Add('# Another comment');
    Rules.LoadFromStrings(Lines);
    CheckEquals(1, Rules.Count, 'Should have 1 rule (comments and blanks skipped)');
  finally
    FreeAndNil(Rules);
    FreeAndNil(Lines);
  end;
end;

procedure TTestReverseFunctionPair.TestSaveToStrings;
var
  Lines: TStringList;
  Rules: TReversalRuleList;
begin
  Lines := TStringList.Create;
  Rules := TReversalRuleList.Create;
  try
    Rules.Add('pattern1', 'replace1');
    Rules.Add('pattern2', 'replace2');
    Rules.SaveToStrings(Lines);
    CheckEquals(5, Lines.Count, 'Should have 5 lines (2 rules + 1 blank separator)');
    CheckEquals('pattern1', Lines[0]);
    CheckEquals('replace1', Lines[1]);
    CheckEquals('', Lines[2]);
    CheckEquals('pattern2', Lines[3]);
    CheckEquals('replace2', Lines[4]);
  finally
    FreeAndNil(Rules);
    FreeAndNil(Lines);
  end;
end;

procedure TTestReverseFunctionPair.TestNoMatchReturnsOriginal;
var
  S, Original: string;
begin
  S := 'WriteLn(''Hello'');';
  Original := S;
  CheckFalse(ReverseFunctionPairStr(S, FRules), 'Should return False');
  CheckEquals(Original, S, 'String should be unchanged');
end;

{ TTestReverseAssignment }

procedure TTestReverseAssignment.TestDelphiAssignment;
var
  S: string;
begin
  S := 'Foo := Bar;';
  CheckTrue(ReverseAssignmentStr(S, False), 'Should return True');
  CheckEquals('Bar := Foo;', S);
end;

procedure TTestReverseAssignment.TestDelphiWithSpaces;
var
  S: string;
begin
  S := 'Foo   :=   Bar;';
  CheckTrue(ReverseAssignmentStr(S, False), 'Should return True');
  CheckEquals('Bar   :=   Foo;', S);
end;

procedure TTestReverseAssignment.TestCppAssignment;
var
  S: string;
begin
  S := 'Foo = Bar;';
  CheckTrue(ReverseAssignmentStr(S, True), 'Should return True');
  CheckEquals('Bar = Foo;', S);
end;

procedure TTestReverseAssignment.TestDelphiWithComment;
var
  S, Original: string;
begin
  S := '// Foo := Bar;';
  Original := S;
  CheckFalse(ReverseAssignmentStr(S, False), 'Should return False');
  CheckEquals(Original, S, 'String should be unchanged');
end;

procedure TTestReverseAssignment.TestNoAssignment;
var
  S, Original: string;
begin
  S := 'WriteLn(Hello);';
  Original := S;
  CheckFalse(ReverseAssignmentStr(S, False), 'Should return False');
  CheckEquals(Original, S, 'String should be unchanged');
end;

procedure TTestReverseAssignment.TestWithLeadingWhitespace;
var
  S: string;
begin
  S := '  Foo := Bar;';
  CheckTrue(ReverseAssignmentStr(S, False), 'Should return True');
  CheckEquals('  Bar := Foo;', S);
end;

initialization
  RegisterTest(TTestReverseForLoop.Suite);
  RegisterTest(TTestReverseFunctionPair.Suite);
  RegisterTest(TTestReverseAssignment.Suite);
end.
