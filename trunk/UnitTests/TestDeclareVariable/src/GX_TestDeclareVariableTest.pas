unit GX_TestDeclareVariableTest;

interface

uses
  TestFramework,
  GX_DeclareVariable,
  GX_PascalTokenAnalyzer;

type
  TTestIsReservedWord = class(TTestCase)
  published
    procedure TestBeginIsReserved;
    procedure TestEndIsReserved;
    procedure TestVarIsReserved;
    procedure TestFooIsNotReserved;
    procedure TestIntegerIsNotReserved;
    procedure TestEmptyIsReserved;
  end;

  TTestInferType = class(TTestCase)
  private
    function DoInfer(const Source: string; const IdentName: string): string;
  published
    procedure TestInferFromIntegerLiteral;
    procedure TestInferFromFloatLiteral;
    procedure TestInferFromStringLiteral;
    procedure TestInferFromBooleanTrue;
    procedure TestInferFromBooleanFalse;
    procedure TestInferFromConstructorCall;
    procedure TestInferFromDeclaredVar;
    procedure TestInferFromAssignmentTarget;
    procedure TestInferFromParameter;
    procedure TestInferFromDivisionExpression;
    procedure TestInferFromMixedArithmetic;
    procedure TestInferIntegerDivision;
    procedure TestNoInference;
    procedure TestInferFromFunctionReturnType;
    procedure TestInferFromFunctionReturnTypeNoParens;
    procedure TestInferFromMethodReturnType;
    procedure TestInferFromParamType;
    procedure TestInferFromSecondParamType;
    procedure TestInferFromParamTypeMultiNames;
    procedure TestNoInferFromUnknownFunction;
  end;

  TTestFindInsertionPoints = class(TTestCase)
  private
    function DoAnalyze(const Source: string; CursorPos: Integer): TDeclareInfo;
  published
    procedure TestLocalVarInExistingVarSection;
    procedure TestLocalVarNewVarSection;
    procedure TestFieldInClass;
    procedure TestNoInsertionPointAtUnitLevel;
    procedure TestReservedWordError;
    procedure TestAlreadyDeclared;
  end;

  TTestBuildDeclarationText = class(TTestCase)
  published
    procedure TestLocalVar;
    procedure TestField;
    procedure TestFieldWithFPrefix;
    procedure TestProperty;
  end;

  TTestInsertDeclaration = class(TTestCase)
  private
    procedure DoTestInsert(const Source: string; CursorPos: Integer;
      const TypeName, Expected: string);
  published
    procedure TestInsertLocalVarInExistingSection;
    procedure TestInsertLocalVarNewSection;
    procedure TestInsertFieldInClass;
  end;

implementation

uses
  SysUtils;

{ TTestIsReservedWord }

procedure TTestIsReservedWord.TestBeginIsReserved;
begin
  CheckTrue(IsReservedWord('begin'), 'begin should be reserved');
end;

procedure TTestIsReservedWord.TestEndIsReserved;
begin
  CheckTrue(IsReservedWord('end'), 'end should be reserved');
end;

procedure TTestIsReservedWord.TestVarIsReserved;
begin
  CheckTrue(IsReservedWord('var'), 'var should be reserved');
end;

procedure TTestIsReservedWord.TestFooIsNotReserved;
begin
  CheckFalse(IsReservedWord('Foo'), 'Foo should not be reserved');
end;

procedure TTestIsReservedWord.TestIntegerIsNotReserved;
begin
  // Integer is a type keyword but treated as an identifier in the lexer
  CheckFalse(IsReservedWord('Integer'), 'Integer should not be reserved');
end;

procedure TTestIsReservedWord.TestEmptyIsReserved;
begin
  CheckTrue(IsReservedWord(''), 'empty string should be treated as reserved');
end;

{ TTestInferType }

function TTestInferType.DoInfer(const Source: string; const IdentName: string): string;
var
  Tokens: TTokenArray;
  Procs: TProcScopeArray;
  Classes: TClassScopeArray;
  I: Integer;
  IdentIdx: Integer;
begin
  Tokens := TokenizeSource(Source);
  Classes := BuildClassScopes(Tokens);
  Procs := BuildProcScopes(Tokens, Classes);

  IdentIdx := -1;
  for I := 0 to High(Tokens) do begin
    if SameText(Tokens[I].Text, IdentName) then begin
      IdentIdx := I;
      Break;
    end;
  end;

  if IdentIdx >= 0 then
    Result := InferTypeFromContext(Tokens, IdentIdx, IdentName, Procs, Classes)
  else
    Result := '';
end;

procedure TTestInferType.TestInferFromIntegerLiteral;
begin
  CheckEquals('Integer', DoInfer(
    'procedure Foo;'#13#10 +
    'begin'#13#10 +
    '  X := 42;'#13#10 +
    'end;', 'X'));
end;

procedure TTestInferType.TestInferFromFloatLiteral;
begin
  CheckEquals('Double', DoInfer(
    'procedure Foo;'#13#10 +
    'begin'#13#10 +
    '  X := 3.14;'#13#10 +
    'end;', 'X'));
end;

procedure TTestInferType.TestInferFromStringLiteral;
begin
  CheckEquals('string', DoInfer(
    'procedure Foo;'#13#10 +
    'begin'#13#10 +
    '  X := ''hello'';'#13#10 +
    'end;', 'X'));
end;

procedure TTestInferType.TestInferFromBooleanTrue;
begin
  CheckEquals('Boolean', DoInfer(
    'procedure Foo;'#13#10 +
    'begin'#13#10 +
    '  X := True;'#13#10 +
    'end;', 'X'));
end;

procedure TTestInferType.TestInferFromBooleanFalse;
begin
  CheckEquals('Boolean', DoInfer(
    'procedure Foo;'#13#10 +
    'begin'#13#10 +
    '  X := False;'#13#10 +
    'end;', 'X'));
end;

procedure TTestInferType.TestInferFromConstructorCall;
begin
  CheckEquals('TStringList', DoInfer(
    'procedure Foo;'#13#10 +
    'begin'#13#10 +
    '  X := TStringList.Create;'#13#10 +
    'end;', 'X'));
end;

procedure TTestInferType.TestInferFromDeclaredVar;
begin
  CheckEquals('Integer', DoInfer(
    'procedure Foo;'#13#10 +
    'var'#13#10 +
    '  Y: Integer;'#13#10 +
    'begin'#13#10 +
    '  X := Y;'#13#10 +
    'end;', 'X'));
end;

procedure TTestInferType.TestInferFromAssignmentTarget;
begin
  CheckEquals('string', DoInfer(
    'procedure Foo;'#13#10 +
    'var'#13#10 +
    '  S: string;'#13#10 +
    'begin'#13#10 +
    '  S := X;'#13#10 +
    'end;', 'X'));
end;

procedure TTestInferType.TestInferFromParameter;
begin
  CheckEquals('TObject', DoInfer(
    'procedure Foo(Sender: TObject);'#13#10 +
    'begin'#13#10 +
    '  X := Sender;'#13#10 +
    'end;', 'X'));
end;

procedure TTestInferType.TestInferFromDivisionExpression;
begin
  // Delphi '/' always returns floating point
  CheckEquals('Double', DoInfer(
    'procedure Foo;'#13#10 +
    'begin'#13#10 +
    '  X := 32 * 7 / 3;'#13#10 +
    'end;', 'X'));
end;

procedure TTestInferType.TestInferFromMixedArithmetic;
begin
  // Expression starts with integer but has '/' later
  CheckEquals('Double', DoInfer(
    'procedure Foo;'#13#10 +
    'begin'#13#10 +
    '  X := A / B;'#13#10 +
    'end;', 'X'));
end;

procedure TTestInferType.TestInferIntegerDivision;
begin
  // 'div' is integer division — result should be Integer
  CheckEquals('Integer', DoInfer(
    'procedure Foo;'#13#10 +
    'begin'#13#10 +
    '  X := 32 * 7 div 3;'#13#10 +
    'end;', 'X'));
end;

procedure TTestInferType.TestNoInference;
begin
  CheckEquals('', DoInfer(
    'procedure Foo;'#13#10 +
    'begin'#13#10 +
    '  Bar(X);'#13#10 +
    'end;', 'X'));
end;

procedure TTestInferType.TestInferFromFunctionReturnType;
begin
  // X := GetCount(...) where GetCount is a local function returning Integer
  CheckEquals('Integer', DoInfer(
    'function GetCount(S: string): Integer;'#13#10 +
    'begin Result := 0; end;'#13#10 +
    'procedure Foo;'#13#10 +
    'begin'#13#10 +
    '  X := GetCount(''test'');'#13#10 +
    'end;', 'X'));
end;

procedure TTestInferType.TestInferFromFunctionReturnTypeNoParens;
begin
  // X := GetName where GetName is a function returning string (called without parens)
  CheckEquals('string', DoInfer(
    'function GetName: string;'#13#10 +
    'begin Result := ''''; end;'#13#10 +
    'procedure Foo;'#13#10 +
    'begin'#13#10 +
    '  X := GetName;'#13#10 +
    'end;', 'X'));
end;

procedure TTestInferType.TestInferFromMethodReturnType;
begin
  // X := Obj.GetValue where Obj is declared as TMyObj and GetValue returns Double
  CheckEquals('Double', DoInfer(
    'type'#13#10 +
    '  TMyObj = class'#13#10 +
    '    function GetValue: Double;'#13#10 +
    '  end;'#13#10 +
    'function TMyObj.GetValue: Double;'#13#10 +
    'begin Result := 0.0; end;'#13#10 +
    'procedure Foo;'#13#10 +
    'var'#13#10 +
    '  Obj: TMyObj;'#13#10 +
    'begin'#13#10 +
    '  X := Obj.GetValue;'#13#10 +
    'end;', 'X'));
end;

procedure TTestInferType.TestInferFromParamType;
begin
  // DoSomething(X) where DoSomething takes a string parameter
  CheckEquals('string', DoInfer(
    'procedure DoSomething(Name: string);'#13#10 +
    'begin end;'#13#10 +
    'procedure Foo;'#13#10 +
    'begin'#13#10 +
    '  DoSomething(X);'#13#10 +
    'end;', 'X'));
end;

procedure TTestInferType.TestInferFromSecondParamType;
begin
  // Func(A, X) where X is the second parameter
  CheckEquals('Integer', DoInfer(
    'procedure Func(Name: string; Count: Integer);'#13#10 +
    'begin end;'#13#10 +
    'procedure Foo;'#13#10 +
    'begin'#13#10 +
    '  Func(''hello'', X);'#13#10 +
    'end;', 'X'));
end;

procedure TTestInferType.TestInferFromParamTypeMultiNames;
begin
  // Func(A, B, X) where params are (A, B: string; C: Integer) — X is at index 2
  CheckEquals('Integer', DoInfer(
    'procedure Func(A, B: string; C: Integer);'#13#10 +
    'begin end;'#13#10 +
    'procedure Foo;'#13#10 +
    'begin'#13#10 +
    '  Func(''a'', ''b'', X);'#13#10 +
    'end;', 'X'));
end;

procedure TTestInferType.TestNoInferFromUnknownFunction;
begin
  // X := UnknownFunc(...) where UnknownFunc is not declared in source
  CheckEquals('', DoInfer(
    'procedure Foo;'#13#10 +
    'begin'#13#10 +
    '  X := UnknownFunc(42);'#13#10 +
    'end;', 'X'));
end;

{ TTestFindInsertionPoints }

function TTestFindInsertionPoints.DoAnalyze(const Source: string; CursorPos: Integer): TDeclareInfo;
begin
  AnalyzeForDeclaration(Source, CursorPos, Result);
end;

procedure TTestFindInsertionPoints.TestLocalVarInExistingVarSection;
var
  Source: string;
  Info: TDeclareInfo;
  HasExistingVar: Boolean;
  I: Integer;
begin
  Source :=
    'procedure Foo;'#13#10 +   // 1-16
    'var'#13#10 +               // 17-21
    '  Y: Integer;'#13#10 +    // 22-36
    'begin'#13#10 +             // 37-43
    '  X := 42;'#13#10 +       // 44-55
    'end;';                     // 56-60
  // Cursor on X at position 46
  Info := DoAnalyze(Source, 46);
  CheckTrue(Length(Info.InsertionPoints) > 0, 'Should have insertion points');
  HasExistingVar := False;
  for I := 0 to High(Info.InsertionPoints) do begin
    if (Info.InsertionPoints[I].Kind = dkLocalVar) and
       not Info.InsertionPoints[I].NeedsVarKeyword then begin
      HasExistingVar := True;
      Break;
    end;
  end;
  CheckTrue(HasExistingVar, 'Should find existing var section');
  CheckEquals('Integer', Info.InferredType);
end;

procedure TTestFindInsertionPoints.TestLocalVarNewVarSection;
var
  Source: string;
  Info: TDeclareInfo;
  HasNewVar: Boolean;
  I, XPos: Integer;
begin
  Source :=
    'procedure Foo;'#13#10 +
    'begin'#13#10 +
    '  X := 42;'#13#10 +
    'end;';
  // Cursor on X — use Pos to find it reliably
  XPos := Pos('X := 42', Source);
  Info := DoAnalyze(Source, XPos);
  CheckTrue(Length(Info.InsertionPoints) > 0, 'Should have insertion points');
  HasNewVar := False;
  for I := 0 to High(Info.InsertionPoints) do begin
    if (Info.InsertionPoints[I].Kind = dkLocalVar) and
       Info.InsertionPoints[I].NeedsVarKeyword then begin
      HasNewVar := True;
      Break;
    end;
  end;
  CheckTrue(HasNewVar, 'Should offer to create new var section');
end;

procedure TTestFindInsertionPoints.TestFieldInClass;
var
  Source: string;
  Info: TDeclareInfo;
  HasField: Boolean;
  I: Integer;
begin
  Source :=
    'type'#13#10 +
    '  TMyClass = class'#13#10 +
    '  private'#13#10 +
    '    FValue: Integer;'#13#10 +
    '  public'#13#10 +
    '    procedure DoStuff;'#13#10 +
    '  end;'#13#10 +
    #13#10 +
    'procedure TMyClass.DoStuff;'#13#10 +
    'begin'#13#10 +
    '  X := FValue;'#13#10 +
    'end;';
  // Find cursor position for X in DoStuff body
  // "  X := FValue;" — find the X
  I := Pos('X := FValue', Source);
  Info := DoAnalyze(Source, I);
  CheckTrue(Length(Info.InsertionPoints) > 0, 'Should have insertion points');
  HasField := False;
  for I := 0 to High(Info.InsertionPoints) do begin
    if Info.InsertionPoints[I].Kind = dkField then begin
      HasField := True;
      Break;
    end;
  end;
  CheckTrue(HasField, 'Should offer field insertion');
end;

procedure TTestFindInsertionPoints.TestNoInsertionPointAtUnitLevel;
var
  Source: string;
  Info: TDeclareInfo;
begin
  Source :=
    'unit Test;'#13#10 +
    'interface'#13#10 +
    'implementation'#13#10 +
    'end.';
  // Cursor on 'Test' - this is the unit name, not really testable for insertion
  Info := DoAnalyze(Source, 6);
  // Unit name itself — we should still get a result but might not have insertion points
  // since cursor is not inside a procedure
  CheckTrue(Length(Info.InsertionPoints) = 0, 'No insertion points at unit level');
end;

procedure TTestFindInsertionPoints.TestReservedWordError;
begin
  // Reserved words are not identifiers so the cursor won't land on them.
  // Instead, verify IsReservedWord directly for common keywords.
  CheckTrue(IsReservedWord('begin'), 'begin should be reserved');
  CheckTrue(IsReservedWord('procedure'), 'procedure should be reserved');
  CheckTrue(IsReservedWord('if'), 'if should be reserved');
  CheckFalse(IsReservedWord('MyVar'), 'MyVar should not be reserved');
end;

procedure TTestFindInsertionPoints.TestAlreadyDeclared;
var
  Source: string;
  Info: TDeclareInfo;
begin
  Source :=
    'procedure Foo;'#13#10 +
    'var'#13#10 +
    '  X: Integer;'#13#10 +
    'begin'#13#10 +
    '  X := 42;'#13#10 +
    'end;';
  // Cursor on X in body
  Info := DoAnalyze(Source, Pos('X := 42', Source));
  CheckTrue(Info.ExistingDecl <> '', 'Should detect existing declaration');
end;

{ TTestBuildDeclarationText }

procedure TTestBuildDeclarationText.TestLocalVar;
begin
  CheckEquals('  Count: Integer;'#13#10,
    BuildDeclarationText('Count', 'Integer', dkLocalVar, '  ', '', ''));
end;

procedure TTestBuildDeclarationText.TestField;
begin
  CheckEquals('    FCount: Integer;'#13#10,
    BuildDeclarationText('Count', 'Integer', dkField, '    ', '', ''));
end;

procedure TTestBuildDeclarationText.TestFieldWithFPrefix;
begin
  // If ident already starts with F, don't add another F
  CheckEquals('    FCount: Integer;'#13#10,
    BuildDeclarationText('FCount', 'Integer', dkField, '    ', '', ''));
end;

procedure TTestBuildDeclarationText.TestProperty;
begin
  CheckEquals('    property Name: string read FName write FName;'#13#10,
    BuildDeclarationText('Name', 'string', dkProperty, '    ', 'FName', 'FName'));
end;

{ TTestInsertDeclaration }

procedure TTestInsertDeclaration.DoTestInsert(const Source: string;
  CursorPos: Integer; const TypeName, Expected: string);
var
  Info: TDeclareInfo;
  DeclText, ModifiedSource: string;
  I: Integer;
  InsertPt: TInsertionPoint;
  FoundLocal: Boolean;
begin
  CheckTrue(AnalyzeForDeclaration(Source, CursorPos, Info),
    'AnalyzeForDeclaration should succeed');
  CheckTrue(Length(Info.InsertionPoints) > 0, 'Should have insertion points');

  // Find first local var insertion point
  FoundLocal := False;
  for I := 0 to High(Info.InsertionPoints) do begin
    if Info.InsertionPoints[I].Kind = dkLocalVar then begin
      InsertPt := Info.InsertionPoints[I];
      FoundLocal := True;
      Break;
    end;
  end;
  CheckTrue(FoundLocal, 'Should have local var insertion point');

  DeclText := BuildDeclarationText(Info.IdentName, TypeName, dkLocalVar,
    InsertPt.Indent, '', '');
  ModifiedSource := InsertDeclaration(Source, InsertPt, DeclText);
  CheckEquals(Expected, ModifiedSource);
end;

procedure TTestInsertDeclaration.TestInsertLocalVarInExistingSection;
begin
  DoTestInsert(
    'procedure Foo;'#13#10 +
    'var'#13#10 +
    '  Y: Integer;'#13#10 +
    'begin'#13#10 +
    '  X := 42;'#13#10 +
    'end;',
    // Cursor on X
    46,
    'Integer',
    'procedure Foo;'#13#10 +
    'var'#13#10 +
    '  Y: Integer;'#13#10 +
    '  X: Integer;'#13#10 +
    'begin'#13#10 +
    '  X := 42;'#13#10 +
    'end;'
  );
end;

procedure TTestInsertDeclaration.TestInsertLocalVarNewSection;
var
  Source, Expected: string;
  Info: TDeclareInfo;
  DeclText, ModifiedSource: string;
  I: Integer;
  InsertPt: TInsertionPoint;
  FoundNew: Boolean;
begin
  Source :=
    'procedure Foo;'#13#10 +
    'begin'#13#10 +
    '  X := 42;'#13#10 +
    'end;';
  Expected :=
    'procedure Foo;'#13#10 +
    'var'#13#10 +
    '  X: Integer;'#13#10 +
    'begin'#13#10 +
    '  X := 42;'#13#10 +
    'end;';

  CheckTrue(AnalyzeForDeclaration(Source, Pos('X := 42', Source), Info),
    'AnalyzeForDeclaration should succeed');

  // Find the "new var section" insertion point
  FoundNew := False;
  for I := 0 to High(Info.InsertionPoints) do begin
    if (Info.InsertionPoints[I].Kind = dkLocalVar) and
       Info.InsertionPoints[I].NeedsVarKeyword then begin
      InsertPt := Info.InsertionPoints[I];
      FoundNew := True;
      Break;
    end;
  end;
  CheckTrue(FoundNew, 'Should have new var section option');

  DeclText := BuildDeclarationText('X', 'Integer', dkLocalVar,
    InsertPt.Indent, '', '');
  ModifiedSource := InsertDeclaration(Source, InsertPt, DeclText);
  CheckEquals(Expected, ModifiedSource);
end;

procedure TTestInsertDeclaration.TestInsertFieldInClass;
var
  Source: string;
  Info: TDeclareInfo;
  DeclText, ModifiedSource: string;
  I, XPos: Integer;
  InsertPt: TInsertionPoint;
  FoundField: Boolean;
begin
  Source :=
    'type'#13#10 +
    '  TMyClass = class'#13#10 +
    '  private'#13#10 +
    '    FValue: Integer;'#13#10 +
    '  public'#13#10 +
    '    procedure DoStuff;'#13#10 +
    '  end;'#13#10 +
    #13#10 +
    'procedure TMyClass.DoStuff;'#13#10 +
    'begin'#13#10 +
    '  X := FValue;'#13#10 +
    'end;';

  XPos := Pos('X := FValue', Source);
  CheckTrue(AnalyzeForDeclaration(Source, XPos, Info),
    'AnalyzeForDeclaration should succeed');

  // Find first field insertion point
  FoundField := False;
  for I := 0 to High(Info.InsertionPoints) do begin
    if Info.InsertionPoints[I].Kind = dkField then begin
      InsertPt := Info.InsertionPoints[I];
      FoundField := True;
      Break;
    end;
  end;
  CheckTrue(FoundField, 'Should have field insertion point');

  DeclText := BuildDeclarationText('X', 'Integer', dkField,
    InsertPt.Indent, '', '');
  ModifiedSource := InsertDeclaration(Source, InsertPt, DeclText);

  // Verify the field was inserted in the private section
  CheckTrue(Pos('FX: Integer;', ModifiedSource) > 0,
    'Should have inserted FX field');
  CheckTrue(Pos('FX: Integer;', ModifiedSource) < Pos('public', ModifiedSource),
    'Field should be before public section');
end;

initialization
  RegisterTest('IsReservedWord', TTestIsReservedWord.Suite);
  RegisterTest('InferType', TTestInferType.Suite);
  RegisterTest('FindInsertionPoints', TTestFindInsertionPoints.Suite);
  RegisterTest('BuildDeclarationText', TTestBuildDeclarationText.Suite);
  RegisterTest('InsertDeclaration', TTestInsertDeclaration.Suite);

end.
