unit GX_TestRenameIdentifierTest;

interface

uses
  TestFramework,
  GX_RenameIdentifier;

type
  TTestRenameLocalVar = class(TTestCase)
  private
    procedure DoRename(const Source, NewName, Expected: string);
  published
    procedure TestRenameLocalVar;
    procedure TestSameNameDifferentProcs;
    procedure TestNestedProcInnerVar;
    procedure TestNestedProcOuterVar;
    procedure TestMultipleVarsOnOneLine;
    procedure TestForLoopVariable;
    procedure TestForwardDeclaredProc;
    procedure TestDeeplyNestedProcs;
  end;

  TTestRenameParam = class(TTestCase)
  private
    procedure DoRename(const Source, NewName, Expected: string);
  published
    procedure TestRenameParam;
    procedure TestRenameConstParam;
    procedure TestRenameVarParam;
    procedure TestRenameOutParam;
    procedure TestRenameMethodParam;
    procedure TestRenameMethodParamFromImpl;
  end;

  TTestRenameClassMember = class(TTestCase)
  private
    procedure DoRename(const Source, NewName, Expected: string);
  published
    procedure TestRenameField;
    procedure TestRenameProperty;
    procedure TestRenameMethodName;
    procedure TestRenameRecordField;
    procedure TestRenameFieldFromMethodBody;
    procedure TestMultipleClassesSameMemberName;
    procedure TestClassProcedure;
  end;

  TTestRenameUnitLevel = class(TTestCase)
  private
    procedure DoRename(const Source, NewName, Expected: string);
  published
    procedure TestRenameUnitLevelType;
    procedure TestRenameUnitLevelConst;
    procedure TestRenameUnitLevelVar;
  end;

  TTestRenameShadowing = class(TTestCase)
  private
    procedure DoRename(const Source, NewName, Expected: string);
  published
    procedure TestUnitVarShadowedByLocal;
    procedure TestClassFieldShadowedByLocal;
    procedure TestOutParamShadowsUnitVar;
  end;

  TTestRenameEdgeCases = class(TTestCase)
  private
    procedure DoRename(const Source, NewName, Expected: string);
    procedure DoRenameExpectError(const Source: string);
  published
    procedure TestIdentInStringNotRenamed;
    procedure TestIdentInCommentNotRenamed;
    procedure TestCaseInsensitiveUniformOutput;
    procedure TestCursorNotOnIdentifier;
    procedure TestNewNameSameAsOld;
    procedure TestContextDirectiveAsIdentifier;
    procedure TestNestedRecordInClass;
    procedure TestInterfaceSectionOnly;
    procedure TestTypeNameAndConstructorCall;
    procedure TestCursorAfterIdentifier;
    procedure TestCursorOnSemicolonAfterIdent;
    procedure TestCursorOnParenAfterIdent;
  end;

  TTestRenameAllTokenOccurrences = class(TTestCase)
  published
    procedure TestBasicRename;
    procedure TestNoFalsePositiveInStrings;
    procedure TestNoFalsePositiveInComments;
    procedure TestCaseInsensitiveMatch;
    procedure TestNoMatchReturnsSameSource;
    procedure TestSameNameReturnsSameSource;
    procedure TestMultipleOccurrences;
  end;

  TTestIdentifierInfo = class(TTestCase)
  private
    procedure DoCheckInfo(const Source: string; ExpectedKind: TIdentifierKind;
      const ExpectedKindDesc, ExpectedTypeName: string);
  published
    procedure TestLocalVariable;
    procedure TestLocalConstant;
    procedure TestLocalType;
    procedure TestParameter;
    procedure TestMethodParameter;
    procedure TestClassField;
    procedure TestClassProperty;
    procedure TestClassMethod;
    procedure TestUnitVariable;
    procedure TestUnitConstant;
    procedure TestUnitType;
    procedure TestUnitProcedure;
    procedure TestExternalIdent;
    procedure TestUnitVarImplSection;
  end;

  TTestRenameResult = class(TTestCase)
  private
    procedure DoRename(const Source, NewName, Expected: string);
    procedure DoCheckInfo(const Source: string; ExpectedKind: TIdentifierKind;
      const ExpectedKindDesc, ExpectedTypeName: string);
  published
    procedure TestResultInFunction;
    procedure TestResultInNestedProcedure;
    procedure TestResultInNestedFunction;
    procedure TestResultTwoFunctions;
    procedure TestResultInfoInFunction;
    procedure TestResultInfoInConstructor;
  end;

  TTestConflictDetection = class(TTestCase)
  private
    procedure DoCheckConflict(const Source, NewName: string;
      ExpectConflict: Boolean; const ExpectedSubstr: string = '');
  published
    procedure TestNoConflict;
    procedure TestLocalVarConflict;
    procedure TestParamConflict;
    procedure TestClassMemberConflict;
    procedure TestUnitLevelConflict;
    procedure TestMethodSeesClassMember;
  end;

implementation

uses
  SysUtils;

// Marker: {|} indicates the cursor position in the source.
// It is removed before calling RenameIdentifierInSource, and the
// cursor position is calculated from the marker's location.
function FindAndRemoveMarker(var Source: string): Integer;
var
  P: Integer;
begin
  P := Pos('{|}', Source);
  if P = 0 then
    raise Exception.Create('Test source must contain {|} marker');
  Delete(Source, P, 3);
  Result := P;
end;

{ TTestRenameLocalVar }

procedure TTestRenameLocalVar.DoRename(const Source, NewName, Expected: string);
var
  Src, RenamedSource, OldName, ScopeDesc, ErrorMsg: string;
  CursorPos, OccurrenceCount: Integer;
  IdentInfo: TIdentifierInfo;
begin
  Src := Source;
  CursorPos := FindAndRemoveMarker(Src);
  CheckTrue(RenameIdentifierInSource(Src, CursorPos, NewName,
    RenamedSource, OccurrenceCount, OldName, ScopeDesc, IdentInfo, ErrorMsg),
    'RenameIdentifierInSource failed: ' + ErrorMsg);
  CheckEquals(Expected, RenamedSource, 'Renamed source mismatch');
end;

procedure TTestRenameLocalVar.TestRenameLocalVar;
begin
  DoRename(
    'procedure Foo;'#13#10 +
    'var'#13#10 +
    '  {|}X: Integer;'#13#10 +
    'begin'#13#10 +
    '  X := 1;'#13#10 +
    '  WriteLn(X);'#13#10 +
    'end;'#13#10,
    'Y',
    'procedure Foo;'#13#10 +
    'var'#13#10 +
    '  Y: Integer;'#13#10 +
    'begin'#13#10 +
    '  Y := 1;'#13#10 +
    '  WriteLn(Y);'#13#10 +
    'end;'#13#10
  );
end;

procedure TTestRenameLocalVar.TestSameNameDifferentProcs;
begin
  // Rename X in Foo — should not affect X in Bar
  DoRename(
    'procedure Foo;'#13#10 +
    'var'#13#10 +
    '  {|}X: Integer;'#13#10 +
    'begin'#13#10 +
    '  X := 1;'#13#10 +
    'end;'#13#10 +
    #13#10 +
    'procedure Bar;'#13#10 +
    'var'#13#10 +
    '  X: Integer;'#13#10 +
    'begin'#13#10 +
    '  X := 2;'#13#10 +
    'end;'#13#10,
    'Y',
    'procedure Foo;'#13#10 +
    'var'#13#10 +
    '  Y: Integer;'#13#10 +
    'begin'#13#10 +
    '  Y := 1;'#13#10 +
    'end;'#13#10 +
    #13#10 +
    'procedure Bar;'#13#10 +
    'var'#13#10 +
    '  X: Integer;'#13#10 +
    'begin'#13#10 +
    '  X := 2;'#13#10 +
    'end;'#13#10
  );
end;

procedure TTestRenameLocalVar.TestNestedProcInnerVar;
begin
  // Rename X in Inner — should not affect outer's X
  DoRename(
    'procedure Outer;'#13#10 +
    'var'#13#10 +
    '  X: Integer;'#13#10 +
    ''#13#10 +
    '  procedure Inner;'#13#10 +
    '  var'#13#10 +
    '    {|}X: Integer;'#13#10 +
    '  begin'#13#10 +
    '    X := 10;'#13#10 +
    '  end;'#13#10 +
    ''#13#10 +
    'begin'#13#10 +
    '  X := 5;'#13#10 +
    'end;'#13#10,
    'Y',
    'procedure Outer;'#13#10 +
    'var'#13#10 +
    '  X: Integer;'#13#10 +
    ''#13#10 +
    '  procedure Inner;'#13#10 +
    '  var'#13#10 +
    '    Y: Integer;'#13#10 +
    '  begin'#13#10 +
    '    Y := 10;'#13#10 +
    '  end;'#13#10 +
    ''#13#10 +
    'begin'#13#10 +
    '  X := 5;'#13#10 +
    'end;'#13#10
  );
end;

procedure TTestRenameLocalVar.TestNestedProcOuterVar;
begin
  // Rename X in Outer — cursor on outer's X declaration
  // Inner does NOT shadow X, so inner's usage should also be renamed
  DoRename(
    'procedure Outer;'#13#10 +
    'var'#13#10 +
    '  {|}X: Integer;'#13#10 +
    ''#13#10 +
    '  procedure Inner;'#13#10 +
    '  begin'#13#10 +
    '    X := 10;'#13#10 +
    '  end;'#13#10 +
    ''#13#10 +
    'begin'#13#10 +
    '  X := 5;'#13#10 +
    'end;'#13#10,
    'Y',
    'procedure Outer;'#13#10 +
    'var'#13#10 +
    '  Y: Integer;'#13#10 +
    ''#13#10 +
    '  procedure Inner;'#13#10 +
    '  begin'#13#10 +
    '    Y := 10;'#13#10 +
    '  end;'#13#10 +
    ''#13#10 +
    'begin'#13#10 +
    '  Y := 5;'#13#10 +
    'end;'#13#10
  );
end;

procedure TTestRenameLocalVar.TestMultipleVarsOnOneLine;
begin
  // Multiple vars on one line: var A, X, B: Integer;
  // Only X should be renamed
  DoRename(
    'procedure Foo;'#13#10 +
    'var'#13#10 +
    '  A, {|}X, B: Integer;'#13#10 +
    'begin'#13#10 +
    '  A := 1;'#13#10 +
    '  X := 2;'#13#10 +
    '  B := 3;'#13#10 +
    'end;'#13#10,
    'Y',
    'procedure Foo;'#13#10 +
    'var'#13#10 +
    '  A, Y, B: Integer;'#13#10 +
    'begin'#13#10 +
    '  A := 1;'#13#10 +
    '  Y := 2;'#13#10 +
    '  B := 3;'#13#10 +
    'end;'#13#10
  );
end;

procedure TTestRenameLocalVar.TestForLoopVariable;
begin
  // Rename for-loop variable (declared in var section)
  DoRename(
    'procedure Foo;'#13#10 +
    'var'#13#10 +
    '  {|}I: Integer;'#13#10 +
    'begin'#13#10 +
    '  for I := 0 to 10 do'#13#10 +
    '    WriteLn(I);'#13#10 +
    'end;'#13#10,
    'J',
    'procedure Foo;'#13#10 +
    'var'#13#10 +
    '  J: Integer;'#13#10 +
    'begin'#13#10 +
    '  for J := 0 to 10 do'#13#10 +
    '    WriteLn(J);'#13#10 +
    'end;'#13#10
  );
end;

procedure TTestRenameLocalVar.TestForwardDeclaredProc;
begin
  // Forward-declared proc — rename local var in the implementation
  DoRename(
    'procedure Foo; forward;'#13#10 +
    #13#10 +
    'procedure Foo;'#13#10 +
    'var'#13#10 +
    '  {|}X: Integer;'#13#10 +
    'begin'#13#10 +
    '  X := 1;'#13#10 +
    'end;'#13#10,
    'Y',
    'procedure Foo; forward;'#13#10 +
    #13#10 +
    'procedure Foo;'#13#10 +
    'var'#13#10 +
    '  Y: Integer;'#13#10 +
    'begin'#13#10 +
    '  Y := 1;'#13#10 +
    'end;'#13#10
  );
end;

procedure TTestRenameLocalVar.TestDeeplyNestedProcs;
begin
  // Three levels deep — rename innermost X, don't affect outer levels
  DoRename(
    'procedure Level1;'#13#10 +
    'var'#13#10 +
    '  X: Integer;'#13#10 +
    #13#10 +
    '  procedure Level2;'#13#10 +
    '  var'#13#10 +
    '    X: Integer;'#13#10 +
    #13#10 +
    '    procedure Level3;'#13#10 +
    '    var'#13#10 +
    '      {|}X: Integer;'#13#10 +
    '    begin'#13#10 +
    '      X := 30;'#13#10 +
    '    end;'#13#10 +
    #13#10 +
    '  begin'#13#10 +
    '    X := 20;'#13#10 +
    '  end;'#13#10 +
    #13#10 +
    'begin'#13#10 +
    '  X := 10;'#13#10 +
    'end;'#13#10,
    'Y',
    'procedure Level1;'#13#10 +
    'var'#13#10 +
    '  X: Integer;'#13#10 +
    #13#10 +
    '  procedure Level2;'#13#10 +
    '  var'#13#10 +
    '    X: Integer;'#13#10 +
    #13#10 +
    '    procedure Level3;'#13#10 +
    '    var'#13#10 +
    '      Y: Integer;'#13#10 +
    '    begin'#13#10 +
    '      Y := 30;'#13#10 +
    '    end;'#13#10 +
    #13#10 +
    '  begin'#13#10 +
    '    X := 20;'#13#10 +
    '  end;'#13#10 +
    #13#10 +
    'begin'#13#10 +
    '  X := 10;'#13#10 +
    'end;'#13#10
  );
end;

{ TTestRenameParam }

procedure TTestRenameParam.DoRename(const Source, NewName, Expected: string);
var
  Src, RenamedSource, OldName, ScopeDesc, ErrorMsg: string;
  CursorPos, OccurrenceCount: Integer;
  IdentInfo: TIdentifierInfo;
begin
  Src := Source;
  CursorPos := FindAndRemoveMarker(Src);
  CheckTrue(RenameIdentifierInSource(Src, CursorPos, NewName,
    RenamedSource, OccurrenceCount, OldName, ScopeDesc, IdentInfo, ErrorMsg),
    'RenameIdentifierInSource failed: ' + ErrorMsg);
  CheckEquals(Expected, RenamedSource, 'Renamed source mismatch');
end;

procedure TTestRenameParam.TestRenameParam;
begin
  DoRename(
    'procedure Foo({|}AValue: Integer);'#13#10 +
    'begin'#13#10 +
    '  WriteLn(AValue);'#13#10 +
    'end;'#13#10,
    'ANewValue',
    'procedure Foo(ANewValue: Integer);'#13#10 +
    'begin'#13#10 +
    '  WriteLn(ANewValue);'#13#10 +
    'end;'#13#10
  );
end;

procedure TTestRenameParam.TestRenameConstParam;
begin
  DoRename(
    'procedure Foo(const {|}AValue: Integer);'#13#10 +
    'begin'#13#10 +
    '  WriteLn(AValue);'#13#10 +
    'end;'#13#10,
    'ANewValue',
    'procedure Foo(const ANewValue: Integer);'#13#10 +
    'begin'#13#10 +
    '  WriteLn(ANewValue);'#13#10 +
    'end;'#13#10
  );
end;

procedure TTestRenameParam.TestRenameVarParam;
begin
  DoRename(
    'procedure Foo(var {|}AValue: Integer);'#13#10 +
    'begin'#13#10 +
    '  AValue := 42;'#13#10 +
    'end;'#13#10,
    'ANewValue',
    'procedure Foo(var ANewValue: Integer);'#13#10 +
    'begin'#13#10 +
    '  ANewValue := 42;'#13#10 +
    'end;'#13#10
  );
end;

procedure TTestRenameParam.TestRenameOutParam;
begin
  DoRename(
    'procedure Foo(out {|}AResult: Integer);'#13#10 +
    'begin'#13#10 +
    '  AResult := 42;'#13#10 +
    'end;'#13#10,
    'AOutput',
    'procedure Foo(out AOutput: Integer);'#13#10 +
    'begin'#13#10 +
    '  AOutput := 42;'#13#10 +
    'end;'#13#10
  );
end;

procedure TTestRenameParam.TestRenameMethodParam;
begin
  // Rename parameter in class method declaration — should also rename
  // in implementation signature and body
  DoRename(
    'unit TestUnit;'#13#10 +
    #13#10 +
    'interface'#13#10 +
    #13#10 +
    'type'#13#10 +
    '  TMyClass = class'#13#10 +
    '    procedure DoWork({|}AValue: Integer);'#13#10 +
    '  end;'#13#10 +
    #13#10 +
    'implementation'#13#10 +
    #13#10 +
    'procedure TMyClass.DoWork(AValue: Integer);'#13#10 +
    'begin'#13#10 +
    '  WriteLn(AValue);'#13#10 +
    'end;'#13#10 +
    #13#10 +
    'end.'#13#10,
    'ANewValue',
    'unit TestUnit;'#13#10 +
    #13#10 +
    'interface'#13#10 +
    #13#10 +
    'type'#13#10 +
    '  TMyClass = class'#13#10 +
    '    procedure DoWork(ANewValue: Integer);'#13#10 +
    '  end;'#13#10 +
    #13#10 +
    'implementation'#13#10 +
    #13#10 +
    'procedure TMyClass.DoWork(ANewValue: Integer);'#13#10 +
    'begin'#13#10 +
    '  WriteLn(ANewValue);'#13#10 +
    'end;'#13#10 +
    #13#10 +
    'end.'#13#10
  );
end;

procedure TTestRenameParam.TestRenameMethodParamFromImpl;
begin
  // Cursor on parameter in implementation method signature — should also rename
  // in class declaration and body
  DoRename(
    'unit TestUnit;'#13#10 +
    #13#10 +
    'interface'#13#10 +
    #13#10 +
    'type'#13#10 +
    '  TMyClass = class'#13#10 +
    '    procedure DoWork(AValue: Integer);'#13#10 +
    '  end;'#13#10 +
    #13#10 +
    'implementation'#13#10 +
    #13#10 +
    'procedure TMyClass.DoWork({|}AValue: Integer);'#13#10 +
    'begin'#13#10 +
    '  WriteLn(AValue);'#13#10 +
    'end;'#13#10 +
    #13#10 +
    'end.'#13#10,
    'ANewValue',
    'unit TestUnit;'#13#10 +
    #13#10 +
    'interface'#13#10 +
    #13#10 +
    'type'#13#10 +
    '  TMyClass = class'#13#10 +
    '    procedure DoWork(ANewValue: Integer);'#13#10 +
    '  end;'#13#10 +
    #13#10 +
    'implementation'#13#10 +
    #13#10 +
    'procedure TMyClass.DoWork(ANewValue: Integer);'#13#10 +
    'begin'#13#10 +
    '  WriteLn(ANewValue);'#13#10 +
    'end;'#13#10 +
    #13#10 +
    'end.'#13#10
  );
end;

{ TTestRenameClassMember }

procedure TTestRenameClassMember.DoRename(const Source, NewName, Expected: string);
var
  Src, RenamedSource, OldName, ScopeDesc, ErrorMsg: string;
  CursorPos, OccurrenceCount: Integer;
  IdentInfo: TIdentifierInfo;
begin
  Src := Source;
  CursorPos := FindAndRemoveMarker(Src);
  CheckTrue(RenameIdentifierInSource(Src, CursorPos, NewName,
    RenamedSource, OccurrenceCount, OldName, ScopeDesc, IdentInfo, ErrorMsg),
    'RenameIdentifierInSource failed: ' + ErrorMsg);
  CheckEquals(Expected, RenamedSource, 'Renamed source mismatch');
end;

procedure TTestRenameClassMember.TestRenameField;
begin
  DoRename(
    'unit TestUnit;'#13#10 +
    #13#10 +
    'interface'#13#10 +
    #13#10 +
    'type'#13#10 +
    '  TMyClass = class'#13#10 +
    '  private'#13#10 +
    '    {|}FValue: Integer;'#13#10 +
    '  end;'#13#10 +
    #13#10 +
    'implementation'#13#10 +
    #13#10 +
    'procedure TMyClass.DoSomething;'#13#10 +
    'begin'#13#10 +
    '  FValue := 42;'#13#10 +
    'end;'#13#10 +
    #13#10 +
    'end.'#13#10,
    'FData',
    'unit TestUnit;'#13#10 +
    #13#10 +
    'interface'#13#10 +
    #13#10 +
    'type'#13#10 +
    '  TMyClass = class'#13#10 +
    '  private'#13#10 +
    '    FData: Integer;'#13#10 +
    '  end;'#13#10 +
    #13#10 +
    'implementation'#13#10 +
    #13#10 +
    'procedure TMyClass.DoSomething;'#13#10 +
    'begin'#13#10 +
    '  FData := 42;'#13#10 +
    'end;'#13#10 +
    #13#10 +
    'end.'#13#10
  );
end;

procedure TTestRenameClassMember.TestRenameProperty;
begin
  DoRename(
    'unit TestUnit;'#13#10 +
    #13#10 +
    'interface'#13#10 +
    #13#10 +
    'type'#13#10 +
    '  TMyClass = class'#13#10 +
    '  private'#13#10 +
    '    FValue: Integer;'#13#10 +
    '  public'#13#10 +
    '    property {|}Value: Integer read FValue;'#13#10 +
    '  end;'#13#10 +
    #13#10 +
    'implementation'#13#10 +
    #13#10 +
    'end.'#13#10,
    'Data',
    'unit TestUnit;'#13#10 +
    #13#10 +
    'interface'#13#10 +
    #13#10 +
    'type'#13#10 +
    '  TMyClass = class'#13#10 +
    '  private'#13#10 +
    '    FValue: Integer;'#13#10 +
    '  public'#13#10 +
    '    property Data: Integer read FValue;'#13#10 +
    '  end;'#13#10 +
    #13#10 +
    'implementation'#13#10 +
    #13#10 +
    'end.'#13#10
  );
end;

procedure TTestRenameClassMember.TestRenameMethodName;
begin
  DoRename(
    'unit TestUnit;'#13#10 +
    #13#10 +
    'interface'#13#10 +
    #13#10 +
    'type'#13#10 +
    '  TMyClass = class'#13#10 +
    '    procedure {|}DoWork;'#13#10 +
    '  end;'#13#10 +
    #13#10 +
    'implementation'#13#10 +
    #13#10 +
    'procedure TMyClass.DoWork;'#13#10 +
    'begin'#13#10 +
    '  DoWork;'#13#10 +
    'end;'#13#10 +
    #13#10 +
    'end.'#13#10,
    'Execute',
    'unit TestUnit;'#13#10 +
    #13#10 +
    'interface'#13#10 +
    #13#10 +
    'type'#13#10 +
    '  TMyClass = class'#13#10 +
    '    procedure Execute;'#13#10 +
    '  end;'#13#10 +
    #13#10 +
    'implementation'#13#10 +
    #13#10 +
    'procedure TMyClass.Execute;'#13#10 +
    'begin'#13#10 +
    '  Execute;'#13#10 +
    'end;'#13#10 +
    #13#10 +
    'end.'#13#10
  );
end;

procedure TTestRenameClassMember.TestRenameRecordField;
begin
  // Record with a method — rename field in declaration + method implementation
  DoRename(
    'unit TestUnit;'#13#10 +
    #13#10 +
    'interface'#13#10 +
    #13#10 +
    'type'#13#10 +
    '  TMyRecord = record'#13#10 +
    '    {|}FValue: Integer;'#13#10 +
    '    procedure SetValue(AValue: Integer);'#13#10 +
    '  end;'#13#10 +
    #13#10 +
    'implementation'#13#10 +
    #13#10 +
    'procedure TMyRecord.SetValue(AValue: Integer);'#13#10 +
    'begin'#13#10 +
    '  FValue := AValue;'#13#10 +
    'end;'#13#10 +
    #13#10 +
    'end.'#13#10,
    'FData',
    'unit TestUnit;'#13#10 +
    #13#10 +
    'interface'#13#10 +
    #13#10 +
    'type'#13#10 +
    '  TMyRecord = record'#13#10 +
    '    FData: Integer;'#13#10 +
    '    procedure SetValue(AValue: Integer);'#13#10 +
    '  end;'#13#10 +
    #13#10 +
    'implementation'#13#10 +
    #13#10 +
    'procedure TMyRecord.SetValue(AValue: Integer);'#13#10 +
    'begin'#13#10 +
    '  FData := AValue;'#13#10 +
    'end;'#13#10 +
    #13#10 +
    'end.'#13#10
  );
end;

procedure TTestRenameClassMember.TestRenameFieldFromMethodBody;
begin
  // Cursor on FValue inside method body — scope detection should find class scope
  DoRename(
    'unit TestUnit;'#13#10 +
    #13#10 +
    'interface'#13#10 +
    #13#10 +
    'type'#13#10 +
    '  TMyClass = class'#13#10 +
    '  private'#13#10 +
    '    FValue: Integer;'#13#10 +
    '    procedure DoSomething;'#13#10 +
    '  end;'#13#10 +
    #13#10 +
    'implementation'#13#10 +
    #13#10 +
    'procedure TMyClass.DoSomething;'#13#10 +
    'begin'#13#10 +
    '  {|}FValue := 42;'#13#10 +
    'end;'#13#10 +
    #13#10 +
    'end.'#13#10,
    'FData',
    'unit TestUnit;'#13#10 +
    #13#10 +
    'interface'#13#10 +
    #13#10 +
    'type'#13#10 +
    '  TMyClass = class'#13#10 +
    '  private'#13#10 +
    '    FData: Integer;'#13#10 +
    '    procedure DoSomething;'#13#10 +
    '  end;'#13#10 +
    #13#10 +
    'implementation'#13#10 +
    #13#10 +
    'procedure TMyClass.DoSomething;'#13#10 +
    'begin'#13#10 +
    '  FData := 42;'#13#10 +
    'end;'#13#10 +
    #13#10 +
    'end.'#13#10
  );
end;

procedure TTestRenameClassMember.TestMultipleClassesSameMemberName;
begin
  // Two classes with FValue — rename only TClassA's, leave TClassB unchanged
  DoRename(
    'unit TestUnit;'#13#10 +
    #13#10 +
    'interface'#13#10 +
    #13#10 +
    'type'#13#10 +
    '  TClassA = class'#13#10 +
    '    {|}FValue: Integer;'#13#10 +
    '    procedure DoA;'#13#10 +
    '  end;'#13#10 +
    #13#10 +
    '  TClassB = class'#13#10 +
    '    FValue: Integer;'#13#10 +
    '    procedure DoB;'#13#10 +
    '  end;'#13#10 +
    #13#10 +
    'implementation'#13#10 +
    #13#10 +
    'procedure TClassA.DoA;'#13#10 +
    'begin'#13#10 +
    '  FValue := 1;'#13#10 +
    'end;'#13#10 +
    #13#10 +
    'procedure TClassB.DoB;'#13#10 +
    'begin'#13#10 +
    '  FValue := 2;'#13#10 +
    'end;'#13#10 +
    #13#10 +
    'end.'#13#10,
    'FData',
    'unit TestUnit;'#13#10 +
    #13#10 +
    'interface'#13#10 +
    #13#10 +
    'type'#13#10 +
    '  TClassA = class'#13#10 +
    '    FData: Integer;'#13#10 +
    '    procedure DoA;'#13#10 +
    '  end;'#13#10 +
    #13#10 +
    '  TClassB = class'#13#10 +
    '    FValue: Integer;'#13#10 +
    '    procedure DoB;'#13#10 +
    '  end;'#13#10 +
    #13#10 +
    'implementation'#13#10 +
    #13#10 +
    'procedure TClassA.DoA;'#13#10 +
    'begin'#13#10 +
    '  FData := 1;'#13#10 +
    'end;'#13#10 +
    #13#10 +
    'procedure TClassB.DoB;'#13#10 +
    'begin'#13#10 +
    '  FValue := 2;'#13#10 +
    'end;'#13#10 +
    #13#10 +
    'end.'#13#10
  );
end;

procedure TTestRenameClassMember.TestClassProcedure;
begin
  // Class method — 'class procedure' in implementation should be tracked
  DoRename(
    'unit TestUnit;'#13#10 +
    #13#10 +
    'interface'#13#10 +
    #13#10 +
    'type'#13#10 +
    '  TMyClass = class'#13#10 +
    '    class procedure {|}DoWork;'#13#10 +
    '  end;'#13#10 +
    #13#10 +
    'implementation'#13#10 +
    #13#10 +
    'class procedure TMyClass.DoWork;'#13#10 +
    'begin'#13#10 +
    'end;'#13#10 +
    #13#10 +
    'end.'#13#10,
    'Execute',
    'unit TestUnit;'#13#10 +
    #13#10 +
    'interface'#13#10 +
    #13#10 +
    'type'#13#10 +
    '  TMyClass = class'#13#10 +
    '    class procedure Execute;'#13#10 +
    '  end;'#13#10 +
    #13#10 +
    'implementation'#13#10 +
    #13#10 +
    'class procedure TMyClass.Execute;'#13#10 +
    'begin'#13#10 +
    'end;'#13#10 +
    #13#10 +
    'end.'#13#10
  );
end;

{ TTestRenameUnitLevel }

procedure TTestRenameUnitLevel.DoRename(const Source, NewName, Expected: string);
var
  Src, RenamedSource, OldName, ScopeDesc, ErrorMsg: string;
  CursorPos, OccurrenceCount: Integer;
  IdentInfo: TIdentifierInfo;
begin
  Src := Source;
  CursorPos := FindAndRemoveMarker(Src);
  CheckTrue(RenameIdentifierInSource(Src, CursorPos, NewName,
    RenamedSource, OccurrenceCount, OldName, ScopeDesc, IdentInfo, ErrorMsg),
    'RenameIdentifierInSource failed: ' + ErrorMsg);
  CheckEquals(Expected, RenamedSource, 'Renamed source mismatch');
end;

procedure TTestRenameUnitLevel.TestRenameUnitLevelType;
begin
  DoRename(
    'unit TestUnit;'#13#10 +
    #13#10 +
    'interface'#13#10 +
    #13#10 +
    'type'#13#10 +
    '  {|}TMyType = Integer;'#13#10 +
    #13#10 +
    'implementation'#13#10 +
    #13#10 +
    'procedure Foo;'#13#10 +
    'var'#13#10 +
    '  X: TMyType;'#13#10 +
    'begin'#13#10 +
    'end;'#13#10 +
    #13#10 +
    'end.'#13#10,
    'TNewType',
    'unit TestUnit;'#13#10 +
    #13#10 +
    'interface'#13#10 +
    #13#10 +
    'type'#13#10 +
    '  TNewType = Integer;'#13#10 +
    #13#10 +
    'implementation'#13#10 +
    #13#10 +
    'procedure Foo;'#13#10 +
    'var'#13#10 +
    '  X: TNewType;'#13#10 +
    'begin'#13#10 +
    'end;'#13#10 +
    #13#10 +
    'end.'#13#10
  );
end;

procedure TTestRenameUnitLevel.TestRenameUnitLevelConst;
begin
  DoRename(
    'unit TestUnit;'#13#10 +
    #13#10 +
    'interface'#13#10 +
    #13#10 +
    'const'#13#10 +
    '  {|}MaxCount = 100;'#13#10 +
    #13#10 +
    'implementation'#13#10 +
    #13#10 +
    'procedure Foo;'#13#10 +
    'begin'#13#10 +
    '  WriteLn(MaxCount);'#13#10 +
    'end;'#13#10 +
    #13#10 +
    'end.'#13#10,
    'MaxItems',
    'unit TestUnit;'#13#10 +
    #13#10 +
    'interface'#13#10 +
    #13#10 +
    'const'#13#10 +
    '  MaxItems = 100;'#13#10 +
    #13#10 +
    'implementation'#13#10 +
    #13#10 +
    'procedure Foo;'#13#10 +
    'begin'#13#10 +
    '  WriteLn(MaxItems);'#13#10 +
    'end;'#13#10 +
    #13#10 +
    'end.'#13#10
  );
end;

procedure TTestRenameUnitLevel.TestRenameUnitLevelVar;
begin
  DoRename(
    'unit TestUnit;'#13#10 +
    #13#10 +
    'interface'#13#10 +
    #13#10 +
    'var'#13#10 +
    '  {|}GlobalVar: Integer;'#13#10 +
    #13#10 +
    'implementation'#13#10 +
    #13#10 +
    'procedure Foo;'#13#10 +
    'begin'#13#10 +
    '  GlobalVar := 1;'#13#10 +
    'end;'#13#10 +
    #13#10 +
    'end.'#13#10,
    'GlobalData',
    'unit TestUnit;'#13#10 +
    #13#10 +
    'interface'#13#10 +
    #13#10 +
    'var'#13#10 +
    '  GlobalData: Integer;'#13#10 +
    #13#10 +
    'implementation'#13#10 +
    #13#10 +
    'procedure Foo;'#13#10 +
    'begin'#13#10 +
    '  GlobalData := 1;'#13#10 +
    'end;'#13#10 +
    #13#10 +
    'end.'#13#10
  );
end;

{ TTestRenameShadowing }

procedure TTestRenameShadowing.DoRename(const Source, NewName, Expected: string);
var
  Src, RenamedSource, OldName, ScopeDesc, ErrorMsg: string;
  CursorPos, OccurrenceCount: Integer;
  IdentInfo: TIdentifierInfo;
begin
  Src := Source;
  CursorPos := FindAndRemoveMarker(Src);
  CheckTrue(RenameIdentifierInSource(Src, CursorPos, NewName,
    RenamedSource, OccurrenceCount, OldName, ScopeDesc, IdentInfo, ErrorMsg),
    'RenameIdentifierInSource failed: ' + ErrorMsg);
  CheckEquals(Expected, RenamedSource, 'Renamed source mismatch');
end;

procedure TTestRenameShadowing.TestUnitVarShadowedByLocal;
begin
  // Rename unit-level X — local X in Bar should NOT be renamed
  DoRename(
    'unit TestUnit;'#13#10 +
    #13#10 +
    'interface'#13#10 +
    #13#10 +
    'var'#13#10 +
    '  {|}X: Integer;'#13#10 +
    #13#10 +
    'implementation'#13#10 +
    #13#10 +
    'procedure Foo;'#13#10 +
    'begin'#13#10 +
    '  X := 1;'#13#10 +
    'end;'#13#10 +
    #13#10 +
    'procedure Bar;'#13#10 +
    'var'#13#10 +
    '  X: Integer;'#13#10 +
    'begin'#13#10 +
    '  X := 2;'#13#10 +
    'end;'#13#10 +
    #13#10 +
    'end.'#13#10,
    'Y',
    'unit TestUnit;'#13#10 +
    #13#10 +
    'interface'#13#10 +
    #13#10 +
    'var'#13#10 +
    '  Y: Integer;'#13#10 +
    #13#10 +
    'implementation'#13#10 +
    #13#10 +
    'procedure Foo;'#13#10 +
    'begin'#13#10 +
    '  Y := 1;'#13#10 +
    'end;'#13#10 +
    #13#10 +
    'procedure Bar;'#13#10 +
    'var'#13#10 +
    '  X: Integer;'#13#10 +
    'begin'#13#10 +
    '  X := 2;'#13#10 +
    'end;'#13#10 +
    #13#10 +
    'end.'#13#10
  );
end;

procedure TTestRenameShadowing.TestClassFieldShadowedByLocal;
begin
  // Rename class field FValue — local FValue in DoSomething should NOT be renamed
  DoRename(
    'unit TestUnit;'#13#10 +
    #13#10 +
    'interface'#13#10 +
    #13#10 +
    'type'#13#10 +
    '  TMyClass = class'#13#10 +
    '  private'#13#10 +
    '    {|}FValue: Integer;'#13#10 +
    '    procedure DoSomething;'#13#10 +
    '    procedure DoOther;'#13#10 +
    '  end;'#13#10 +
    #13#10 +
    'implementation'#13#10 +
    #13#10 +
    'procedure TMyClass.DoSomething;'#13#10 +
    'var'#13#10 +
    '  FValue: Integer;'#13#10 +
    'begin'#13#10 +
    '  FValue := 10;'#13#10 +
    'end;'#13#10 +
    #13#10 +
    'procedure TMyClass.DoOther;'#13#10 +
    'begin'#13#10 +
    '  FValue := 20;'#13#10 +
    'end;'#13#10 +
    #13#10 +
    'end.'#13#10,
    'FData',
    'unit TestUnit;'#13#10 +
    #13#10 +
    'interface'#13#10 +
    #13#10 +
    'type'#13#10 +
    '  TMyClass = class'#13#10 +
    '  private'#13#10 +
    '    FData: Integer;'#13#10 +
    '    procedure DoSomething;'#13#10 +
    '    procedure DoOther;'#13#10 +
    '  end;'#13#10 +
    #13#10 +
    'implementation'#13#10 +
    #13#10 +
    'procedure TMyClass.DoSomething;'#13#10 +
    'var'#13#10 +
    '  FValue: Integer;'#13#10 +
    'begin'#13#10 +
    '  FValue := 10;'#13#10 +
    'end;'#13#10 +
    #13#10 +
    'procedure TMyClass.DoOther;'#13#10 +
    'begin'#13#10 +
    '  FData := 20;'#13#10 +
    'end;'#13#10 +
    #13#10 +
    'end.'#13#10
  );
end;

procedure TTestRenameShadowing.TestOutParamShadowsUnitVar;
begin
  // Unit-level X — Bar has out param X that shadows it
  DoRename(
    'unit TestUnit;'#13#10 +
    #13#10 +
    'interface'#13#10 +
    #13#10 +
    'var'#13#10 +
    '  {|}X: Integer;'#13#10 +
    #13#10 +
    'implementation'#13#10 +
    #13#10 +
    'procedure Foo;'#13#10 +
    'begin'#13#10 +
    '  X := 1;'#13#10 +
    'end;'#13#10 +
    #13#10 +
    'procedure Bar(out X: Integer);'#13#10 +
    'begin'#13#10 +
    '  X := 2;'#13#10 +
    'end;'#13#10 +
    #13#10 +
    'end.'#13#10,
    'Y',
    'unit TestUnit;'#13#10 +
    #13#10 +
    'interface'#13#10 +
    #13#10 +
    'var'#13#10 +
    '  Y: Integer;'#13#10 +
    #13#10 +
    'implementation'#13#10 +
    #13#10 +
    'procedure Foo;'#13#10 +
    'begin'#13#10 +
    '  Y := 1;'#13#10 +
    'end;'#13#10 +
    #13#10 +
    'procedure Bar(out X: Integer);'#13#10 +
    'begin'#13#10 +
    '  X := 2;'#13#10 +
    'end;'#13#10 +
    #13#10 +
    'end.'#13#10
  );
end;

{ TTestRenameEdgeCases }

procedure TTestRenameEdgeCases.DoRename(const Source, NewName, Expected: string);
var
  Src, RenamedSource, OldName, ScopeDesc, ErrorMsg: string;
  CursorPos, OccurrenceCount: Integer;
  IdentInfo: TIdentifierInfo;
begin
  Src := Source;
  CursorPos := FindAndRemoveMarker(Src);
  CheckTrue(RenameIdentifierInSource(Src, CursorPos, NewName,
    RenamedSource, OccurrenceCount, OldName, ScopeDesc, IdentInfo, ErrorMsg),
    'RenameIdentifierInSource failed: ' + ErrorMsg);
  CheckEquals(Expected, RenamedSource, 'Renamed source mismatch');
end;

procedure TTestRenameEdgeCases.DoRenameExpectError(const Source: string);
var
  Src, RenamedSource, OldName, ScopeDesc, ErrorMsg: string;
  CursorPos, OccurrenceCount: Integer;
  IdentInfo: TIdentifierInfo;
begin
  Src := Source;
  CursorPos := FindAndRemoveMarker(Src);
  CheckFalse(RenameIdentifierInSource(Src, CursorPos, '',
    RenamedSource, OccurrenceCount, OldName, ScopeDesc, IdentInfo, ErrorMsg),
    'Expected RenameIdentifierInSource to fail');
end;

procedure TTestRenameEdgeCases.TestIdentInStringNotRenamed;
begin
  // The string 'X' should not be renamed
  DoRename(
    'procedure Foo;'#13#10 +
    'var'#13#10 +
    '  {|}X: Integer;'#13#10 +
    'begin'#13#10 +
    '  X := 1;'#13#10 +
    '  WriteLn(''X'');'#13#10 +
    'end;'#13#10,
    'Y',
    'procedure Foo;'#13#10 +
    'var'#13#10 +
    '  Y: Integer;'#13#10 +
    'begin'#13#10 +
    '  Y := 1;'#13#10 +
    '  WriteLn(''X'');'#13#10 +
    'end;'#13#10
  );
end;

procedure TTestRenameEdgeCases.TestIdentInCommentNotRenamed;
begin
  // The comment {X} should not be renamed
  DoRename(
    'procedure Foo;'#13#10 +
    'var'#13#10 +
    '  {|}X: Integer;'#13#10 +
    'begin'#13#10 +
    '  X := 1; // X is a variable'#13#10 +
    '  {X := old value}'#13#10 +
    'end;'#13#10,
    'Y',
    'procedure Foo;'#13#10 +
    'var'#13#10 +
    '  Y: Integer;'#13#10 +
    'begin'#13#10 +
    '  Y := 1; // X is a variable'#13#10 +
    '  {X := old value}'#13#10 +
    'end;'#13#10
  );
end;

procedure TTestRenameEdgeCases.TestCaseInsensitiveUniformOutput;
begin
  // All occurrences should get exactly the new casing
  DoRename(
    'procedure Foo;'#13#10 +
    'var'#13#10 +
    '  {|}myVar: Integer;'#13#10 +
    'begin'#13#10 +
    '  MyVar := 1;'#13#10 +
    '  MYVAR := 2;'#13#10 +
    '  myvar := 3;'#13#10 +
    'end;'#13#10,
    'NewVar',
    'procedure Foo;'#13#10 +
    'var'#13#10 +
    '  NewVar: Integer;'#13#10 +
    'begin'#13#10 +
    '  NewVar := 1;'#13#10 +
    '  NewVar := 2;'#13#10 +
    '  NewVar := 3;'#13#10 +
    'end;'#13#10
  );
end;

procedure TTestRenameEdgeCases.TestCursorNotOnIdentifier;
begin
  DoRenameExpectError(
    'procedure Foo;'#13#10 +
    'begin'#13#10 +
    '  X {|}:= 1;'#13#10 +
    'end;'#13#10
  );
end;

procedure TTestRenameEdgeCases.TestNewNameSameAsOld;
var
  Src, RenamedSource, OldName, ScopeDesc, ErrorMsg: string;
  CursorPos, OccurrenceCount: Integer;
  IdentInfo: TIdentifierInfo;
begin
  Src :=
    'procedure Foo;'#13#10 +
    'var'#13#10 +
    '  {|}X: Integer;'#13#10 +
    'begin'#13#10 +
    'end;'#13#10;
  CursorPos := FindAndRemoveMarker(Src);
  CheckFalse(RenameIdentifierInSource(Src, CursorPos, 'X',
    RenamedSource, OccurrenceCount, OldName, ScopeDesc, IdentInfo, ErrorMsg),
    'Expected failure for same name');
end;

procedure TTestRenameEdgeCases.TestContextDirectiveAsIdentifier;
begin
  // 'Name' is tokenized as tkName (context-dependent directive)
  // but should still be renameable via IdentDirect
  DoRename(
    'procedure Foo;'#13#10 +
    'var'#13#10 +
    '  {|}Name: string;'#13#10 +
    'begin'#13#10 +
    '  Name := ''test'';'#13#10 +
    '  WriteLn(Name);'#13#10 +
    'end;'#13#10,
    'ItemName',
    'procedure Foo;'#13#10 +
    'var'#13#10 +
    '  ItemName: string;'#13#10 +
    'begin'#13#10 +
    '  ItemName := ''test'';'#13#10 +
    '  WriteLn(ItemName);'#13#10 +
    'end;'#13#10
  );
end;

procedure TTestRenameEdgeCases.TestNestedRecordInClass;
begin
  // Record nested inside a class — field rename works via outer class scope
  DoRename(
    'unit TestUnit;'#13#10 +
    #13#10 +
    'interface'#13#10 +
    #13#10 +
    'type'#13#10 +
    '  TMyClass = class'#13#10 +
    '  type'#13#10 +
    '    TPoint = record'#13#10 +
    '      {|}X: Integer;'#13#10 +
    '      Y: Integer;'#13#10 +
    '    end;'#13#10 +
    '  private'#13#10 +
    '    FPos: TPoint;'#13#10 +
    '    procedure SetPos;'#13#10 +
    '  end;'#13#10 +
    #13#10 +
    'implementation'#13#10 +
    #13#10 +
    'procedure TMyClass.SetPos;'#13#10 +
    'begin'#13#10 +
    '  FPos.X := 10;'#13#10 +
    '  FPos.Y := 20;'#13#10 +
    'end;'#13#10 +
    #13#10 +
    'end.'#13#10,
    'Left',
    'unit TestUnit;'#13#10 +
    #13#10 +
    'interface'#13#10 +
    #13#10 +
    'type'#13#10 +
    '  TMyClass = class'#13#10 +
    '  type'#13#10 +
    '    TPoint = record'#13#10 +
    '      Left: Integer;'#13#10 +
    '      Y: Integer;'#13#10 +
    '    end;'#13#10 +
    '  private'#13#10 +
    '    FPos: TPoint;'#13#10 +
    '    procedure SetPos;'#13#10 +
    '  end;'#13#10 +
    #13#10 +
    'implementation'#13#10 +
    #13#10 +
    'procedure TMyClass.SetPos;'#13#10 +
    'begin'#13#10 +
    '  FPos.Left := 10;'#13#10 +
    '  FPos.Y := 20;'#13#10 +
    'end;'#13#10 +
    #13#10 +
    'end.'#13#10
  );
end;

procedure TTestRenameEdgeCases.TestInterfaceSectionOnly;
begin
  // Unit with only interface-section usage, empty implementation
  DoRename(
    'unit TestUnit;'#13#10 +
    #13#10 +
    'interface'#13#10 +
    #13#10 +
    'type'#13#10 +
    '  {|}TMyType = Integer;'#13#10 +
    #13#10 +
    'var'#13#10 +
    '  X: TMyType;'#13#10 +
    #13#10 +
    'implementation'#13#10 +
    #13#10 +
    'end.'#13#10,
    'TNewType',
    'unit TestUnit;'#13#10 +
    #13#10 +
    'interface'#13#10 +
    #13#10 +
    'type'#13#10 +
    '  TNewType = Integer;'#13#10 +
    #13#10 +
    'var'#13#10 +
    '  X: TNewType;'#13#10 +
    #13#10 +
    'implementation'#13#10 +
    #13#10 +
    'end.'#13#10
  );
end;

procedure TTestRenameEdgeCases.TestTypeNameAndConstructorCall;
begin
  // Rename type — also renames constructor call TFoo.Create
  DoRename(
    'unit TestUnit;'#13#10 +
    #13#10 +
    'interface'#13#10 +
    #13#10 +
    'type'#13#10 +
    '  {|}TFoo = class'#13#10 +
    '    constructor Create;'#13#10 +
    '  end;'#13#10 +
    #13#10 +
    'implementation'#13#10 +
    #13#10 +
    'constructor TFoo.Create;'#13#10 +
    'begin'#13#10 +
    'end;'#13#10 +
    #13#10 +
    'procedure Bar;'#13#10 +
    'var'#13#10 +
    '  Obj: TFoo;'#13#10 +
    'begin'#13#10 +
    '  Obj := TFoo.Create;'#13#10 +
    'end;'#13#10 +
    #13#10 +
    'end.'#13#10,
    'TBar2',
    'unit TestUnit;'#13#10 +
    #13#10 +
    'interface'#13#10 +
    #13#10 +
    'type'#13#10 +
    '  TBar2 = class'#13#10 +
    '    constructor Create;'#13#10 +
    '  end;'#13#10 +
    #13#10 +
    'implementation'#13#10 +
    #13#10 +
    'constructor TBar2.Create;'#13#10 +
    'begin'#13#10 +
    'end;'#13#10 +
    #13#10 +
    'procedure Bar;'#13#10 +
    'var'#13#10 +
    '  Obj: TBar2;'#13#10 +
    'begin'#13#10 +
    '  Obj := TBar2.Create;'#13#10 +
    'end;'#13#10 +
    #13#10 +
    'end.'#13#10
  );
end;

procedure TTestRenameEdgeCases.TestCursorAfterIdentifier;
begin
  // Cursor right after identifier (e.g. after selecting it) — marker is after 'X'
  DoRename(
    'procedure Foo;'#13#10 +
    'var'#13#10 +
    '  X: Integer;'#13#10 +
    'begin'#13#10 +
    '  X{|} := 1;'#13#10 +
    'end;'#13#10,
    'Y',
    'procedure Foo;'#13#10 +
    'var'#13#10 +
    '  Y: Integer;'#13#10 +
    'begin'#13#10 +
    '  Y := 1;'#13#10 +
    'end;'#13#10
  );
end;

procedure TTestRenameEdgeCases.TestCursorOnSemicolonAfterIdent;
begin
  // Cursor on semicolon directly after identifier: X;
  DoRename(
    'procedure Foo;'#13#10 +
    'var'#13#10 +
    '  X: Integer;'#13#10 +
    'begin'#13#10 +
    '  X{|};'#13#10 +
    'end;'#13#10,
    'Y',
    'procedure Foo;'#13#10 +
    'var'#13#10 +
    '  Y: Integer;'#13#10 +
    'begin'#13#10 +
    '  Y;'#13#10 +
    'end;'#13#10
  );
end;

procedure TTestRenameEdgeCases.TestCursorOnParenAfterIdent;
begin
  // Cursor on opening paren right after identifier
  DoRename(
    'procedure Foo;'#13#10 +
    'var'#13#10 +
    '  X: Integer;'#13#10 +
    'begin'#13#10 +
    '  WriteLn{|}(X);'#13#10 +
    'end;'#13#10,
    'DoWrite',
    'procedure Foo;'#13#10 +
    'var'#13#10 +
    '  X: Integer;'#13#10 +
    'begin'#13#10 +
    '  DoWrite(X);'#13#10 +
    'end;'#13#10
  );
end;

{ TTestRenameAllTokenOccurrences }

procedure TTestRenameAllTokenOccurrences.TestBasicRename;
var
  Src, Result: string;
  Count: Integer;
begin
  Src :=
    'procedure Foo;'#13#10 +
    'begin'#13#10 +
    '  Button1.Enabled := True;'#13#10 +
    '  Button1.Caption := ''OK'';'#13#10 +
    'end;'#13#10;
  Result := RenameAllTokenOccurrences(Src, 'Button1', 'btnOK', Count);
  CheckEquals(2, Count, 'Occurrence count');
  CheckEquals(
    'procedure Foo;'#13#10 +
    'begin'#13#10 +
    '  btnOK.Enabled := True;'#13#10 +
    '  btnOK.Caption := ''OK'';'#13#10 +
    'end;'#13#10,
    Result, 'Renamed source');
end;

procedure TTestRenameAllTokenOccurrences.TestNoFalsePositiveInStrings;
var
  Src, Result: string;
  Count: Integer;
begin
  Src :=
    'procedure Foo;'#13#10 +
    'begin'#13#10 +
    '  X := 1;'#13#10 +
    '  WriteLn(''X'');'#13#10 +
    'end;'#13#10;
  Result := RenameAllTokenOccurrences(Src, 'X', 'Y', Count);
  CheckEquals(1, Count, 'Only identifier X should be renamed, not string');
  Check(Pos('''X''', Result) > 0, 'String ''X'' should be preserved');
end;

procedure TTestRenameAllTokenOccurrences.TestNoFalsePositiveInComments;
var
  Src, Result: string;
  Count: Integer;
begin
  Src :=
    'procedure Foo;'#13#10 +
    'begin'#13#10 +
    '  X := 1; // X is a variable'#13#10 +
    '  {X := old value}'#13#10 +
    '  (* X in block *)'#13#10 +
    'end;'#13#10;
  Result := RenameAllTokenOccurrences(Src, 'X', 'Y', Count);
  CheckEquals(1, Count, 'Only the identifier X should be renamed');
  Check(Pos('// X is a variable', Result) > 0, 'Line comment should be preserved');
  Check(Pos('{X := old value}', Result) > 0, 'Brace comment should be preserved');
  Check(Pos('(* X in block *)', Result) > 0, 'Block comment should be preserved');
end;

procedure TTestRenameAllTokenOccurrences.TestCaseInsensitiveMatch;
var
  Src, Result: string;
  Count: Integer;
begin
  Src :=
    'procedure Foo;'#13#10 +
    'begin'#13#10 +
    '  button1.Enabled := True;'#13#10 +
    '  Button1.Caption := ''OK'';'#13#10 +
    '  BUTTON1.Visible := False;'#13#10 +
    'end;'#13#10;
  Result := RenameAllTokenOccurrences(Src, 'Button1', 'btnOK', Count);
  CheckEquals(3, Count, 'All case variants should be renamed');
  Check(Pos('button1', Result) = 0, 'No lowercase variant should remain');
  Check(Pos('BUTTON1', Result) = 0, 'No uppercase variant should remain');
end;

procedure TTestRenameAllTokenOccurrences.TestNoMatchReturnsSameSource;
var
  Src, Result: string;
  Count: Integer;
begin
  Src :=
    'procedure Foo;'#13#10 +
    'begin'#13#10 +
    '  X := 1;'#13#10 +
    'end;'#13#10;
  Result := RenameAllTokenOccurrences(Src, 'Button1', 'btnOK', Count);
  CheckEquals(0, Count, 'No occurrences should be found');
  CheckEquals(Src, Result, 'Source should be unchanged');
end;

procedure TTestRenameAllTokenOccurrences.TestSameNameReturnsSameSource;
var
  Src, Result: string;
  Count: Integer;
begin
  Src :=
    'procedure Foo;'#13#10 +
    'begin'#13#10 +
    '  X := 1;'#13#10 +
    'end;'#13#10;
  Result := RenameAllTokenOccurrences(Src, 'X', 'X', Count);
  CheckEquals(0, Count, 'Same name should produce zero count');
  CheckEquals(Src, Result, 'Source should be unchanged');
end;

procedure TTestRenameAllTokenOccurrences.TestMultipleOccurrences;
var
  Src, Result: string;
  Count: Integer;
begin
  Src :=
    'unit TestUnit;'#13#10 +
    #13#10 +
    'interface'#13#10 +
    #13#10 +
    'type'#13#10 +
    '  TMyForm = class(TForm)'#13#10 +
    '    Edit1: TEdit;'#13#10 +
    '    procedure FormCreate(Sender: TObject);'#13#10 +
    '  end;'#13#10 +
    #13#10 +
    'implementation'#13#10 +
    #13#10 +
    'procedure TMyForm.FormCreate(Sender: TObject);'#13#10 +
    'begin'#13#10 +
    '  Edit1.Text := ''Hello'';'#13#10 +
    '  Edit1.Enabled := True;'#13#10 +
    '  Edit1.Color := clRed;'#13#10 +
    'end;'#13#10 +
    #13#10 +
    'end.'#13#10;
  Result := RenameAllTokenOccurrences(Src, 'Edit1', 'edtName', Count);
  CheckEquals(4, Count, 'Field declaration + 3 body references');
end;

{ TTestIdentifierInfo }

procedure TTestIdentifierInfo.DoCheckInfo(const Source: string;
  ExpectedKind: TIdentifierKind; const ExpectedKindDesc, ExpectedTypeName: string);
var
  Src, RenamedSource, OldName, ScopeDesc, ErrorMsg: string;
  CursorPos, OccurrenceCount: Integer;
  IdentInfo: TIdentifierInfo;
begin
  Src := Source;
  CursorPos := FindAndRemoveMarker(Src);
  CheckTrue(RenameIdentifierInSource(Src, CursorPos, '',
    RenamedSource, OccurrenceCount, OldName, ScopeDesc, IdentInfo, ErrorMsg),
    'RenameIdentifierInSource failed: ' + ErrorMsg);
  CheckEquals(Ord(ExpectedKind), Ord(IdentInfo.Kind),
    'IdentifierKind mismatch: expected ' + ExpectedKindDesc);
  CheckEquals(ExpectedKindDesc, IdentInfo.KindDesc, 'KindDesc mismatch');
  CheckEquals(ExpectedTypeName, IdentInfo.TypeName, 'TypeName mismatch');
end;

procedure TTestIdentifierInfo.TestLocalVariable;
begin
  DoCheckInfo(
    'procedure Foo;'#13#10 +
    'var'#13#10 +
    '  {|}X: Integer;'#13#10 +
    'begin'#13#10 +
    '  X := 1;'#13#10 +
    'end;'#13#10,
    ikVariable, 'local variable', 'Integer');
end;

procedure TTestIdentifierInfo.TestLocalConstant;
begin
  DoCheckInfo(
    'procedure Foo;'#13#10 +
    'const'#13#10 +
    '  {|}MaxVal = 100;'#13#10 +
    'begin'#13#10 +
    'end;'#13#10,
    ikConstant, 'local constant', '');
end;

procedure TTestIdentifierInfo.TestLocalType;
begin
  DoCheckInfo(
    'procedure Foo;'#13#10 +
    'type'#13#10 +
    '  {|}TMyInt = Integer;'#13#10 +
    'begin'#13#10 +
    'end;'#13#10,
    ikType, 'local type', '');
end;

procedure TTestIdentifierInfo.TestParameter;
begin
  DoCheckInfo(
    'procedure Foo({|}AValue: Integer);'#13#10 +
    'begin'#13#10 +
    'end;'#13#10,
    ikParameter, 'parameter of Foo', 'Integer');
end;

procedure TTestIdentifierInfo.TestMethodParameter;
begin
  DoCheckInfo(
    'unit TestUnit;'#13#10 +
    'interface'#13#10 +
    'type'#13#10 +
    '  TMyClass = class'#13#10 +
    '    procedure DoWork(AValue: Integer);'#13#10 +
    '  end;'#13#10 +
    'implementation'#13#10 +
    'procedure TMyClass.DoWork({|}AValue: Integer);'#13#10 +
    'begin'#13#10 +
    'end;'#13#10 +
    'end.'#13#10,
    ikParameter, 'parameter of TMyClass.DoWork', 'Integer');
end;

procedure TTestIdentifierInfo.TestClassField;
begin
  DoCheckInfo(
    'unit TestUnit;'#13#10 +
    'interface'#13#10 +
    'type'#13#10 +
    '  TMyClass = class'#13#10 +
    '    {|}FValue: Integer;'#13#10 +
    '  end;'#13#10 +
    'implementation'#13#10 +
    'end.'#13#10,
    ikField, 'field of TMyClass', 'Integer');
end;

procedure TTestIdentifierInfo.TestClassProperty;
begin
  DoCheckInfo(
    'unit TestUnit;'#13#10 +
    'interface'#13#10 +
    'type'#13#10 +
    '  TMyClass = class'#13#10 +
    '    FValue: Integer;'#13#10 +
    '    property {|}Value: Integer read FValue;'#13#10 +
    '  end;'#13#10 +
    'implementation'#13#10 +
    'end.'#13#10,
    ikProperty, 'property of TMyClass', 'Integer');
end;

procedure TTestIdentifierInfo.TestClassMethod;
begin
  DoCheckInfo(
    'unit TestUnit;'#13#10 +
    'interface'#13#10 +
    'type'#13#10 +
    '  TMyClass = class'#13#10 +
    '    procedure {|}DoWork;'#13#10 +
    '  end;'#13#10 +
    'implementation'#13#10 +
    'procedure TMyClass.DoWork;'#13#10 +
    'begin'#13#10 +
    'end;'#13#10 +
    'end.'#13#10,
    ikProcedure, 'method of TMyClass', '');
end;

procedure TTestIdentifierInfo.TestUnitVariable;
begin
  DoCheckInfo(
    'unit TestUnit;'#13#10 +
    'interface'#13#10 +
    'var'#13#10 +
    '  {|}GlobalX: Integer;'#13#10 +
    'implementation'#13#10 +
    'end.'#13#10,
    ikVariable, 'unit variable (interface)', 'Integer');
end;

procedure TTestIdentifierInfo.TestUnitConstant;
begin
  DoCheckInfo(
    'unit TestUnit;'#13#10 +
    'interface'#13#10 +
    'const'#13#10 +
    '  {|}MaxItems = 50;'#13#10 +
    'implementation'#13#10 +
    'end.'#13#10,
    ikConstant, 'unit constant (interface)', '');
end;

procedure TTestIdentifierInfo.TestUnitType;
begin
  DoCheckInfo(
    'unit TestUnit;'#13#10 +
    'interface'#13#10 +
    'type'#13#10 +
    '  {|}TMyRecord = record'#13#10 +
    '    X: Integer;'#13#10 +
    '  end;'#13#10 +
    'implementation'#13#10 +
    'end.'#13#10,
    ikType, 'unit type (interface)', '');
end;

procedure TTestIdentifierInfo.TestUnitProcedure;
begin
  DoCheckInfo(
    'unit TestUnit;'#13#10 +
    'interface'#13#10 +
    'procedure {|}DoSomething;'#13#10 +
    'implementation'#13#10 +
    'procedure DoSomething;'#13#10 +
    'begin'#13#10 +
    'end;'#13#10 +
    'end.'#13#10,
    ikProcedure, 'procedure (interface)', '');
end;

procedure TTestIdentifierInfo.TestExternalIdent;
begin
  DoCheckInfo(
    'unit TestUnit;'#13#10 +
    'interface'#13#10 +
    'uses SysUtils;'#13#10 +
    'procedure Foo;'#13#10 +
    'implementation'#13#10 +
    'procedure Foo;'#13#10 +
    'begin'#13#10 +
    '  {|}IntToStr(42);'#13#10 +
    'end;'#13#10 +
    'end.'#13#10,
    ikExternalIdent, 'external identifier', '');
end;

procedure TTestIdentifierInfo.TestUnitVarImplSection;
begin
  DoCheckInfo(
    'unit TestUnit;'#13#10 +
    'interface'#13#10 +
    'implementation'#13#10 +
    'var'#13#10 +
    '  {|}ImplVar: string;'#13#10 +
    'end.'#13#10,
    ikVariable, 'unit variable (implementation)', 'string');
end;

{ TTestConflictDetection }

procedure TTestConflictDetection.DoCheckConflict(const Source, NewName: string;
  ExpectConflict: Boolean; const ExpectedSubstr: string);
var
  Src, ConflictDesc: string;
  CursorPos: Integer;
  HasConflict: Boolean;
begin
  Src := Source;
  CursorPos := FindAndRemoveMarker(Src);
  HasConflict := CheckNameConflict(Src, CursorPos, NewName, ConflictDesc);
  if ExpectConflict then begin
    CheckTrue(HasConflict, 'Expected conflict but none found');
    if ExpectedSubstr <> '' then
      Check(Pos(ExpectedSubstr, ConflictDesc) > 0,
        'Expected conflict description to contain "' + ExpectedSubstr +
        '" but got "' + ConflictDesc + '"');
  end else
    CheckFalse(HasConflict, 'Unexpected conflict: ' + ConflictDesc);
end;

procedure TTestConflictDetection.TestNoConflict;
begin
  DoCheckConflict(
    'procedure Foo;'#13#10 +
    'var'#13#10 +
    '  {|}X: Integer;'#13#10 +
    'begin'#13#10 +
    'end;'#13#10,
    'Y', False);
end;

procedure TTestConflictDetection.TestLocalVarConflict;
begin
  DoCheckConflict(
    'procedure Foo;'#13#10 +
    'var'#13#10 +
    '  {|}X: Integer;'#13#10 +
    '  Y: string;'#13#10 +
    'begin'#13#10 +
    'end;'#13#10,
    'Y', True, 'local variable');
end;

procedure TTestConflictDetection.TestParamConflict;
begin
  DoCheckConflict(
    'procedure Foo(A: Integer; {|}B: string);'#13#10 +
    'begin'#13#10 +
    'end;'#13#10,
    'A', True, 'parameter');
end;

procedure TTestConflictDetection.TestClassMemberConflict;
begin
  DoCheckConflict(
    'unit TestUnit;'#13#10 +
    'interface'#13#10 +
    'type'#13#10 +
    '  TMyClass = class'#13#10 +
    '    {|}FValue: Integer;'#13#10 +
    '    FName: string;'#13#10 +
    '  end;'#13#10 +
    'implementation'#13#10 +
    'end.'#13#10,
    'FName', True, 'member of TMyClass');
end;

procedure TTestConflictDetection.TestUnitLevelConflict;
begin
  DoCheckConflict(
    'unit TestUnit;'#13#10 +
    'interface'#13#10 +
    'var'#13#10 +
    '  {|}Foo: Integer;'#13#10 +
    'const'#13#10 +
    '  Bar = 1;'#13#10 +
    'implementation'#13#10 +
    'end.'#13#10,
    'Bar', True, 'unit level');
end;

procedure TTestConflictDetection.TestMethodSeesClassMember;
begin
  DoCheckConflict(
    'unit TestUnit;'#13#10 +
    'interface'#13#10 +
    'type'#13#10 +
    '  TMyClass = class'#13#10 +
    '    FValue: Integer;'#13#10 +
    '    procedure DoWork;'#13#10 +
    '  end;'#13#10 +
    'implementation'#13#10 +
    'procedure TMyClass.DoWork;'#13#10 +
    'var'#13#10 +
    '  {|}X: Integer;'#13#10 +
    'begin'#13#10 +
    'end;'#13#10 +
    'end.'#13#10,
    'FValue', True, 'member of TMyClass');
end;

{ TTestRenameResult }

procedure TTestRenameResult.DoRename(const Source, NewName, Expected: string);
var
  Src, RenamedSource, OldName, ScopeDesc, ErrorMsg: string;
  CursorPos, OccurrenceCount: Integer;
  IdentInfo: TIdentifierInfo;
begin
  Src := Source;
  CursorPos := FindAndRemoveMarker(Src);
  CheckTrue(RenameIdentifierInSource(Src, CursorPos, NewName,
    RenamedSource, OccurrenceCount, OldName, ScopeDesc, IdentInfo, ErrorMsg),
    'RenameIdentifierInSource failed: ' + ErrorMsg);
  CheckEquals(Expected, RenamedSource, 'Renamed source mismatch');
end;

procedure TTestRenameResult.DoCheckInfo(const Source: string;
  ExpectedKind: TIdentifierKind; const ExpectedKindDesc, ExpectedTypeName: string);
var
  Src, RenamedSource, OldName, ScopeDesc, ErrorMsg: string;
  CursorPos, OccurrenceCount: Integer;
  IdentInfo: TIdentifierInfo;
begin
  Src := Source;
  CursorPos := FindAndRemoveMarker(Src);
  CheckTrue(RenameIdentifierInSource(Src, CursorPos, '',
    RenamedSource, OccurrenceCount, OldName, ScopeDesc, IdentInfo, ErrorMsg),
    'RenameIdentifierInSource failed: ' + ErrorMsg);
  CheckEquals(Ord(ExpectedKind), Ord(IdentInfo.Kind), 'Kind mismatch');
  CheckEquals(ExpectedKindDesc, IdentInfo.KindDesc, 'KindDesc mismatch');
  CheckEquals(ExpectedTypeName, IdentInfo.TypeName, 'TypeName mismatch');
end;

procedure TTestRenameResult.TestResultInFunction;
begin
  DoRename(
    'function Foo: Integer;'#13#10 +
    'begin'#13#10 +
    '  {|}Result := 1;'#13#10 +
    'end;'#13#10 +
    #13#10 +
    'function Bar: Integer;'#13#10 +
    'begin'#13#10 +
    '  Result := 2;'#13#10 +
    'end;'#13#10,
    'Res',
    'function Foo: Integer;'#13#10 +
    'begin'#13#10 +
    '  Res := 1;'#13#10 +
    'end;'#13#10 +
    #13#10 +
    'function Bar: Integer;'#13#10 +
    'begin'#13#10 +
    '  Result := 2;'#13#10 +
    'end;'#13#10
  );
end;

procedure TTestRenameResult.TestResultInNestedProcedure;
begin
  // Result in a nested procedure (not a function) refers to the enclosing function's Result
  DoRename(
    'function Outer: Integer;'#13#10 +
    '  procedure Inner;'#13#10 +
    '  begin'#13#10 +
    '    {|}Result := 1;'#13#10 +
    '  end;'#13#10 +
    'begin'#13#10 +
    '  Result := 42;'#13#10 +
    'end;'#13#10,
    'Res',
    'function Outer: Integer;'#13#10 +
    '  procedure Inner;'#13#10 +
    '  begin'#13#10 +
    '    Res := 1;'#13#10 +
    '  end;'#13#10 +
    'begin'#13#10 +
    '  Res := 42;'#13#10 +
    'end;'#13#10
  );
end;

procedure TTestRenameResult.TestResultInNestedFunction;
begin
  // Result in a nested function refers to the nested function's own Result
  DoRename(
    'function Outer: Integer;'#13#10 +
    '  function Inner: string;'#13#10 +
    '  begin'#13#10 +
    '    {|}Result := ''x'';'#13#10 +
    '  end;'#13#10 +
    'begin'#13#10 +
    '  Result := 42;'#13#10 +
    'end;'#13#10,
    'Res',
    'function Outer: Integer;'#13#10 +
    '  function Inner: string;'#13#10 +
    '  begin'#13#10 +
    '    Res := ''x'';'#13#10 +
    '  end;'#13#10 +
    'begin'#13#10 +
    '  Result := 42;'#13#10 +
    'end;'#13#10
  );
end;

procedure TTestRenameResult.TestResultTwoFunctions;
begin
  // Renaming Result in one function should not affect another
  DoRename(
    'function Foo: Integer;'#13#10 +
    'begin'#13#10 +
    '  Result := 1;'#13#10 +
    'end;'#13#10 +
    #13#10 +
    'function Bar: string;'#13#10 +
    'begin'#13#10 +
    '  {|}Result := ''hello'';'#13#10 +
    'end;'#13#10,
    'Res',
    'function Foo: Integer;'#13#10 +
    'begin'#13#10 +
    '  Result := 1;'#13#10 +
    'end;'#13#10 +
    #13#10 +
    'function Bar: string;'#13#10 +
    'begin'#13#10 +
    '  Res := ''hello'';'#13#10 +
    'end;'#13#10
  );
end;

procedure TTestRenameResult.TestResultInfoInFunction;
begin
  DoCheckInfo(
    'function Foo: Integer;'#13#10 +
    'begin'#13#10 +
    '  {|}Result := 1;'#13#10 +
    'end;'#13#10,
    ikVariable, 'implicit function result', 'Integer');
end;

procedure TTestRenameResult.TestResultInfoInConstructor;
begin
  DoCheckInfo(
    'unit TestUnit;'#13#10 +
    'interface'#13#10 +
    'type'#13#10 +
    '  TMyClass = class'#13#10 +
    '    constructor Create;'#13#10 +
    '  end;'#13#10 +
    'implementation'#13#10 +
    'constructor TMyClass.Create;'#13#10 +
    'begin'#13#10 +
    '  {|}Result := inherited Create;'#13#10 +
    'end;'#13#10 +
    'end.'#13#10,
    ikVariable, 'implicit function result', '');
end;

initialization
  RegisterTest(TTestRenameLocalVar.Suite);
  RegisterTest(TTestRenameParam.Suite);
  RegisterTest(TTestRenameClassMember.Suite);
  RegisterTest(TTestRenameUnitLevel.Suite);
  RegisterTest(TTestRenameShadowing.Suite);
  RegisterTest(TTestRenameEdgeCases.Suite);
  RegisterTest(TTestRenameAllTokenOccurrences.Suite);
  RegisterTest(TTestIdentifierInfo.Suite);
  RegisterTest(TTestRenameResult.Suite);
  RegisterTest(TTestConflictDetection.Suite);

end.
