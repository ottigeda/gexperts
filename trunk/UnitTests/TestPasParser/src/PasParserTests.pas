unit PasParserTests;

interface

uses
  TestFramework;

type
  TTestNextObjectLine = class(TTestCase)
  private
    procedure CheckObjectLine(const Source: string; const ExpectedName: string;
      ExpectedLine: Integer);
  published
    procedure TestSimpleClass;
    procedure TestSimpleInterface;
    procedure TestDispInterface;
    procedure TestGenericClass;
    procedure TestGenericClassWithConstraint;
    procedure TestGenericInterface;
    procedure TestGenericWithMultipleParams;
    procedure TestGenericWithNestedParams;
    procedure TestClassAfterGenericClass;
    procedure TestMultipleGenerics;
    procedure TestGenericRecord;
  end;

  TTestNextClassLine = class(TTestCase)
  published
    procedure TestSimpleClass;
    procedure TestGenericClass;
    procedure TestGenericClassWithConstraint;
    procedure TestGenericClassNotConfusedByInterface;
  end;

  TTestNestedTypes = class(TTestCase)
  private
    procedure CollectAllObjects(const Source: string; Names: array of string;
      Lines: array of Integer);
  published
    // NextObjectLine should find nested classes
    procedure TestNestedClassFound;
    // Multiple nested types: class and record
    procedure TestMultipleNestedTypes;
    // Nested class inside nested class (two levels deep)
    procedure TestDoublyNestedClass;
    // Nested record inside class
    procedure TestNestedRecord;
    // Members after nested type section should not confuse parser
    procedure TestMembersAfterNestedType;
    // Nested class with generic parameters
    procedure TestNestedGenericClass;
    // SkipNestedTypeSection: skip a simple nested class
    procedure TestSkipSimpleNestedClass;
    // SkipNestedTypeSection: skip multiple nested types
    procedure TestSkipMultipleNestedTypes;
    // SkipNestedTypeSection: skip nested record
    procedure TestSkipNestedRecord;
    // SkipNestedTypeSection: skip nested type with class var after
    procedure TestSkipNestedTypeWithMembersAfter;
    // SkipNestedTypeSection: empty type section (type followed by visibility)
    procedure TestSkipEmptyTypeSection;
    // SkipNestedTypeSection: nested type with generic class
    procedure TestSkipNestedGenericClass;
  end;

implementation

uses
  SysUtils,
  mwPasParser,
  mwPasParserTypes;

{ TTestNextObjectLine }

procedure TTestNextObjectLine.CheckObjectLine(const Source: string;
  const ExpectedName: string; ExpectedLine: Integer);
var
  Parser: TmPasParser;
  IdentPos, IdentLine: Integer;
  FoundName: string;
begin
  Parser := TmPasParser.Create;
  try
    Parser.Origin := PChar(Source);
    Parser.NextObjectLine(IdentPos, IdentLine);
    if Parser.Token.ID = tkNull then
      Fail('NextObjectLine did not find any class/interface declaration');
    Parser.SetRunPos(IdentPos);
    FoundName := Parser.Token.Data;
    CheckEqualsString(ExpectedName, FoundName, 'Class/interface name');
    CheckEquals(ExpectedLine, IdentLine, 'Line number');
  finally
    Parser.Free;
  end;
end;

procedure TTestNextObjectLine.TestSimpleClass;
begin
  CheckObjectLine(
    'unit Test;' + #13#10 +
    'type' + #13#10 +
    '  TMyClass = class' + #13#10 +
    '  end;',
    'TMyClass', 2);
end;

procedure TTestNextObjectLine.TestSimpleInterface;
begin
  CheckObjectLine(
    'unit Test;' + #13#10 +
    'type' + #13#10 +
    '  IMyIntf = interface' + #13#10 +
    '  end;',
    'IMyIntf', 2);
end;

procedure TTestNextObjectLine.TestDispInterface;
begin
  CheckObjectLine(
    'unit Test;' + #13#10 +
    'type' + #13#10 +
    '  IMyDisp = dispinterface' + #13#10 +
    '  end;',
    'IMyDisp', 2);
end;

procedure TTestNextObjectLine.TestGenericClass;
begin
  CheckObjectLine(
    'unit Test;' + #13#10 +
    'type' + #13#10 +
    '  TList<T> = class' + #13#10 +
    '  end;',
    'TList', 2);
end;

procedure TTestNextObjectLine.TestGenericClassWithConstraint;
begin
  CheckObjectLine(
    'unit Test;' + #13#10 +
    'type' + #13#10 +
    '  TList<T: IInterface> = class' + #13#10 +
    '  end;',
    'TList', 2);
end;

procedure TTestNextObjectLine.TestGenericInterface;
begin
  CheckObjectLine(
    'unit Test;' + #13#10 +
    'type' + #13#10 +
    '  IEnumerable<T> = interface' + #13#10 +
    '  end;',
    'IEnumerable', 2);
end;

procedure TTestNextObjectLine.TestGenericWithMultipleParams;
begin
  CheckObjectLine(
    'unit Test;' + #13#10 +
    'type' + #13#10 +
    '  TDictionary<TKey, TValue> = class' + #13#10 +
    '  end;',
    'TDictionary', 2);
end;

procedure TTestNextObjectLine.TestGenericWithNestedParams;
begin
  CheckObjectLine(
    'unit Test;' + #13#10 +
    'type' + #13#10 +
    '  TFoo<T: IComparable<T>> = class' + #13#10 +
    '  end;',
    'TFoo', 2);
end;

procedure TTestNextObjectLine.TestClassAfterGenericClass;
const
  Source =
    'unit Test;' + #13#10 +
    'type' + #13#10 +
    '  TGeneric<T> = class' + #13#10 +
    '  end;' + #13#10 +
    '  TPlain = class' + #13#10 +
    '  end;';
var
  Parser: TmPasParser;
  IdentPos, IdentLine: Integer;
begin
  Parser := TmPasParser.Create;
  try
    Parser.Origin := PChar(Source);
    // Find first (generic)
    Parser.NextObjectLine(IdentPos, IdentLine);
    Check(Parser.Token.ID <> tkNull, 'Should find first class');
    Parser.SetRunPos(IdentPos);
    CheckEqualsString('TGeneric', Parser.Token.Data, 'First class name');
    CheckEquals(2, IdentLine, 'First class line');
    // Skip past to find next
    Parser.NextNonJunk;
    Parser.NextObjectLine(IdentPos, IdentLine);
    Check(Parser.Token.ID <> tkNull, 'Should find second class');
    Parser.SetRunPos(IdentPos);
    CheckEqualsString('TPlain', Parser.Token.Data, 'Second class name');
    CheckEquals(4, IdentLine, 'Second class line');
  finally
    Parser.Free;
  end;
end;

procedure TTestNextObjectLine.TestMultipleGenerics;
const
  Source =
    'unit Test;' + #13#10 +
    'type' + #13#10 +
    '  TFirst<T: IFoo> = class' + #13#10 +
    '  end;' + #13#10 +
    '  TSecond<K, V: class> = interface' + #13#10 +
    '  end;';
var
  Parser: TmPasParser;
  IdentPos, IdentLine: Integer;
begin
  Parser := TmPasParser.Create;
  try
    Parser.Origin := PChar(Source);
    Parser.NextObjectLine(IdentPos, IdentLine);
    Check(Parser.Token.ID <> tkNull, 'Should find first');
    Parser.SetRunPos(IdentPos);
    CheckEqualsString('TFirst', Parser.Token.Data);
    CheckEquals(2, IdentLine);
    Parser.NextNonJunk;
    Parser.NextObjectLine(IdentPos, IdentLine);
    Check(Parser.Token.ID <> tkNull, 'Should find second');
    Parser.SetRunPos(IdentPos);
    CheckEqualsString('TSecond', Parser.Token.Data);
    CheckEquals(4, IdentLine);
  finally
    Parser.Free;
  end;
end;

procedure TTestNextObjectLine.TestGenericRecord;
const
  // Records use tkRecord not tkClass/tkInterface, so NextObjectLine should skip past them
  Source =
    'unit Test;' + #13#10 +
    'type' + #13#10 +
    '  TRec<T> = record' + #13#10 +
    '  end;' + #13#10 +
    '  TMyClass<T> = class' + #13#10 +
    '  end;';
var
  Parser: TmPasParser;
  IdentPos, IdentLine: Integer;
begin
  Parser := TmPasParser.Create;
  try
    Parser.Origin := PChar(Source);
    Parser.NextObjectLine(IdentPos, IdentLine);
    Check(Parser.Token.ID <> tkNull, 'Should find class after record');
    Parser.SetRunPos(IdentPos);
    CheckEqualsString('TMyClass', Parser.Token.Data, 'Should skip record and find class');
    CheckEquals(4, IdentLine);
  finally
    Parser.Free;
  end;
end;

{ TTestNextClassLine }

procedure TTestNextClassLine.TestSimpleClass;
var
  Parser: TmPasParser;
begin
  Parser := TmPasParser.Create;
  try
    Parser.Origin := PChar(
      'unit Test;' + #13#10 +
      'type' + #13#10 +
      '  TMyClass = class' + #13#10 +
      '  end;');
    Parser.NextClassLine;
    Check(Parser.Token.ID = tkClass, 'Should find tkClass');
  finally
    Parser.Free;
  end;
end;

procedure TTestNextClassLine.TestGenericClass;
var
  Parser: TmPasParser;
begin
  Parser := TmPasParser.Create;
  try
    Parser.Origin := PChar(
      'unit Test;' + #13#10 +
      'type' + #13#10 +
      '  TList<T> = class' + #13#10 +
      '  end;');
    Parser.NextClassLine;
    Check(Parser.Token.ID = tkClass, 'Should find tkClass for generic');
  finally
    Parser.Free;
  end;
end;

procedure TTestNextClassLine.TestGenericClassWithConstraint;
var
  Parser: TmPasParser;
begin
  Parser := TmPasParser.Create;
  try
    Parser.Origin := PChar(
      'unit Test;' + #13#10 +
      'type' + #13#10 +
      '  TList<T: IInterface> = class' + #13#10 +
      '  end;');
    Parser.NextClassLine;
    Check(Parser.Token.ID = tkClass, 'Should find tkClass for constrained generic');
  finally
    Parser.Free;
  end;
end;

procedure TTestNextClassLine.TestGenericClassNotConfusedByInterface;
var
  Parser: TmPasParser;
begin
  // NextClassLine only looks for tkClass, not tkInterface
  Parser := TmPasParser.Create;
  try
    Parser.Origin := PChar(
      'unit Test;' + #13#10 +
      'type' + #13#10 +
      '  IFoo<T> = interface' + #13#10 +
      '  end;' + #13#10 +
      '  TBar<T> = class' + #13#10 +
      '  end;');
    Parser.NextClassLine;
    Check(Parser.Token.ID = tkClass, 'Should skip interface and find class');
  finally
    Parser.Free;
  end;
end;

{ TTestNestedTypes }

procedure TTestNestedTypes.CollectAllObjects(const Source: string;
  Names: array of string; Lines: array of Integer);
var
  Parser: TmPasParser;
  IdentPos, IdentLine: Integer;
  i: Integer;
begin
  Check(Length(Names) = Length(Lines), 'Names and Lines arrays must have same length');
  Parser := TmPasParser.Create;
  try
    Parser.Origin := PChar(Source);
    for i := 0 to High(Names) do
    begin
      Parser.NextObjectLine(IdentPos, IdentLine);
      Check(Parser.Token.ID <> tkNull,
        Format('Should find class #%d (%s)', [i + 1, Names[i]]));
      Parser.SetRunPos(IdentPos);
      CheckEqualsString(Names[i], Parser.Token.Data,
        Format('Class #%d name', [i + 1]));
      CheckEquals(Lines[i], IdentLine,
        Format('Class #%d line', [i + 1]));
      Parser.NextNonJunk;
    end;
  finally
    Parser.Free;
  end;
end;

procedure TTestNestedTypes.TestNestedClassFound;
const
  Source =
    'unit Test;' + #13#10 +          // line 0
    'type' + #13#10 +                // line 1
    '  TOuterClass = class' + #13#10 +  // line 2
    '  public' + #13#10 +
    '    FBlub: string;' + #13#10 +
    '  public' + #13#10 +
    '    type' + #13#10 +
    '      TInnerClass = class' + #13#10 + // line 7
    '      public' + #13#10 +
    '        FBla: string;' + #13#10 +
    '      end;' + #13#10 +
    '  end;';
begin
  CollectAllObjects(Source,
    ['TOuterClass', 'TInnerClass'],
    [2, 7]);
end;

procedure TTestNestedTypes.TestMultipleNestedTypes;
const
  Source =
    'unit Test;' + #13#10 +          // line 0
    'type' + #13#10 +                // line 1
    '  TOuterClass = class' + #13#10 +  // line 2
    '    type' + #13#10 +
    '      TInnerOne = class' + #13#10 + // line 4
    '        FA: Integer;' + #13#10 +
    '      end;' + #13#10 +
    '      TInnerTwo = class' + #13#10 + // line 7
    '        FB: Integer;' + #13#10 +
    '      end;' + #13#10 +
    '  end;';
begin
  CollectAllObjects(Source,
    ['TOuterClass', 'TInnerOne', 'TInnerTwo'],
    [2, 4, 7]);
end;

procedure TTestNestedTypes.TestDoublyNestedClass;
const
  Source =
    'unit Test;' + #13#10 +          // line 0
    'type' + #13#10 +                // line 1
    '  TLevel1 = class' + #13#10 +   // line 2
    '    type' + #13#10 +
    '      TLevel2 = class' + #13#10 +  // line 4
    '        type' + #13#10 +
    '          TLevel3 = class' + #13#10 +  // line 6
    '            FDeep: string;' + #13#10 +
    '          end;' + #13#10 +
    '      end;' + #13#10 +
    '  end;';
begin
  CollectAllObjects(Source,
    ['TLevel1', 'TLevel2', 'TLevel3'],
    [2, 4, 6]);
end;

procedure TTestNestedTypes.TestNestedRecord;
// NextObjectLine should skip nested records (tkRecord, not tkClass/tkInterface)
const
  Source =
    'unit Test;' + #13#10 +          // line 0
    'type' + #13#10 +                // line 1
    '  TOuterClass = class' + #13#10 +  // line 2
    '    type' + #13#10 +
    '      TInnerRec = record' + #13#10 +
    '        FField: Integer;' + #13#10 +
    '      end;' + #13#10 +
    '  end;' + #13#10 +
    '  TNextClass = class' + #13#10 +  // line 8
    '  end;';
begin
  // NextObjectLine should find TOuterClass, skip the record, then find TNextClass
  CollectAllObjects(Source,
    ['TOuterClass', 'TNextClass'],
    [2, 8]);
end;

procedure TTestNestedTypes.TestMembersAfterNestedType;
const
  Source =
    'unit Test;' + #13#10 +          // line 0
    'type' + #13#10 +                // line 1
    '  TMyClass = class' + #13#10 +  // line 2
    '    FBefore: Integer;' + #13#10 +
    '    type' + #13#10 +
    '      TInner = class' + #13#10 +  // line 5
    '        FInnerField: string;' + #13#10 +
    '      end;' + #13#10 +
    '    FAfter: Integer;' + #13#10 +
    '    procedure DoStuff;' + #13#10 +
    '  end;';
begin
  CollectAllObjects(Source,
    ['TMyClass', 'TInner'],
    [2, 5]);
end;

procedure TTestNestedTypes.TestNestedGenericClass;
const
  Source =
    'unit Test;' + #13#10 +          // line 0
    'type' + #13#10 +                // line 1
    '  TOuterClass = class' + #13#10 +  // line 2
    '    type' + #13#10 +
    '      TInnerList<T> = class' + #13#10 +  // line 4
    '        FItems: array of T;' + #13#10 +
    '      end;' + #13#10 +
    '  end;';
begin
  CollectAllObjects(Source,
    ['TOuterClass', 'TInnerList'],
    [2, 4]);
end;

// --- SkipNestedTypeSection tests ---
// These test a new parser method that skips past nested type declarations
// inside a class body. The parser should be positioned on the token AFTER
// the 'type' keyword when calling SkipNestedTypeSection.

procedure TTestNestedTypes.TestSkipSimpleNestedClass;
const
  Source =
    'TOuterClass = class' + #13#10 +
    '  type' + #13#10 +
    '    TInner = class' + #13#10 +
    '      FField: Integer;' + #13#10 +
    '    end;' + #13#10 +
    '  procedure DoStuff;' + #13#10 +
    'end;';
var
  Parser: TmPasParser;
begin
  Parser := TmPasParser.Create;
  try
    Parser.Origin := PChar(Source);
    // Advance to 'type' keyword
    while (Parser.Token.ID <> tkType) and (Parser.Token.ID <> tkNull) do
      Parser.NextNonJunk;
    Check(Parser.Token.ID = tkType, 'Should find type keyword');
    // Call SkipNestedTypeSection
    Parser.SkipNestedTypeSection;
    // Parser should now be positioned at or before 'procedure'
    while not (Parser.Token.ID in [tkProcedure, tkEnd, tkNull]) do
      Parser.NextNonJunk;
    Check(Parser.Token.ID = tkProcedure,
      'After skipping nested type, should find procedure. Got: ' + Parser.Token.Data);
  finally
    Parser.Free;
  end;
end;

procedure TTestNestedTypes.TestSkipMultipleNestedTypes;
const
  Source =
    'TOuterClass = class' + #13#10 +
    '  type' + #13#10 +
    '    TInnerA = class' + #13#10 +
    '      FA: Integer;' + #13#10 +
    '    end;' + #13#10 +
    '    TInnerB = class' + #13#10 +
    '      FB: Integer;' + #13#10 +
    '    end;' + #13#10 +
    '  procedure DoStuff;' + #13#10 +
    'end;';
var
  Parser: TmPasParser;
begin
  Parser := TmPasParser.Create;
  try
    Parser.Origin := PChar(Source);
    while (Parser.Token.ID <> tkType) and (Parser.Token.ID <> tkNull) do
      Parser.NextNonJunk;
    Check(Parser.Token.ID = tkType, 'Should find type keyword');
    Parser.SkipNestedTypeSection;
    while not (Parser.Token.ID in [tkProcedure, tkEnd, tkNull]) do
      Parser.NextNonJunk;
    Check(Parser.Token.ID = tkProcedure,
      'After skipping multiple nested types, should find procedure');
  finally
    Parser.Free;
  end;
end;

procedure TTestNestedTypes.TestSkipNestedRecord;
const
  Source =
    'TOuterClass = class' + #13#10 +
    '  type' + #13#10 +
    '    TInnerRec = record' + #13#10 +
    '      FField: Integer;' + #13#10 +
    '    end;' + #13#10 +
    '  function GetValue: Integer;' + #13#10 +
    'end;';
var
  Parser: TmPasParser;
begin
  Parser := TmPasParser.Create;
  try
    Parser.Origin := PChar(Source);
    while (Parser.Token.ID <> tkType) and (Parser.Token.ID <> tkNull) do
      Parser.NextNonJunk;
    Check(Parser.Token.ID = tkType, 'Should find type keyword');
    Parser.SkipNestedTypeSection;
    while not (Parser.Token.ID in [tkFunction, tkEnd, tkNull]) do
      Parser.NextNonJunk;
    Check(Parser.Token.ID = tkFunction,
      'After skipping nested record, should find function');
  finally
    Parser.Free;
  end;
end;

procedure TTestNestedTypes.TestSkipNestedTypeWithMembersAfter;
const
  Source =
    'TOuterClass = class' + #13#10 +
    '  FBefore: Integer;' + #13#10 +
    '  type' + #13#10 +
    '    TInner = class' + #13#10 +
    '      FInner: string;' + #13#10 +
    '    end;' + #13#10 +
    '  FAfter: string;' + #13#10 +
    'end;';
var
  Parser: TmPasParser;
begin
  Parser := TmPasParser.Create;
  try
    Parser.Origin := PChar(Source);
    while (Parser.Token.ID <> tkType) and (Parser.Token.ID <> tkNull) do
      Parser.NextNonJunk;
    Check(Parser.Token.ID = tkType, 'Should find type keyword');
    Parser.SkipNestedTypeSection;
    // After skipping, the next identifier should be FAfter (a field of the outer class)
    while not (Parser.Token.ID in [tkIdentifier, tkEnd, tkNull]) do
      Parser.NextNonJunk;
    Check(Parser.Token.ID = tkIdentifier,
      'After skipping nested type, should find identifier');
    CheckEqualsString('FAfter', Parser.Token.Data,
      'Should be at FAfter field, not inside nested type');
  finally
    Parser.Free;
  end;
end;

procedure TTestNestedTypes.TestSkipEmptyTypeSection;
// type keyword followed by visibility keyword (no actual nested type)
const
  Source =
    'TOuterClass = class' + #13#10 +
    '  type' + #13#10 +
    '  public' + #13#10 +
    '    FField: Integer;' + #13#10 +
    'end;';
var
  Parser: TmPasParser;
begin
  Parser := TmPasParser.Create;
  try
    Parser.Origin := PChar(Source);
    while (Parser.Token.ID <> tkType) and (Parser.Token.ID <> tkNull) do
      Parser.NextNonJunk;
    Check(Parser.Token.ID = tkType, 'Should find type keyword');
    Parser.SkipNestedTypeSection;
    // After skipping empty type section, should find 'public'
    while not (Parser.Token.ID in [tkPublic, tkEnd, tkNull]) do
      Parser.NextNonJunk;
    Check(Parser.Token.ID = tkPublic,
      'After skipping empty type section, should find public');
  finally
    Parser.Free;
  end;
end;

procedure TTestNestedTypes.TestSkipNestedGenericClass;
const
  Source =
    'TOuterClass = class' + #13#10 +
    '  type' + #13#10 +
    '    TInnerList<T: IInterface> = class' + #13#10 +
    '      FItems: array of T;' + #13#10 +
    '    end;' + #13#10 +
    '  procedure DoStuff;' + #13#10 +
    'end;';
var
  Parser: TmPasParser;
begin
  Parser := TmPasParser.Create;
  try
    Parser.Origin := PChar(Source);
    while (Parser.Token.ID <> tkType) and (Parser.Token.ID <> tkNull) do
      Parser.NextNonJunk;
    Check(Parser.Token.ID = tkType, 'Should find type keyword');
    Parser.SkipNestedTypeSection;
    while not (Parser.Token.ID in [tkProcedure, tkEnd, tkNull]) do
      Parser.NextNonJunk;
    Check(Parser.Token.ID = tkProcedure,
      'After skipping nested generic class, should find procedure');
  finally
    Parser.Free;
  end;
end;

initialization
  RegisterTest('NextObjectLine', TTestNextObjectLine.Suite);
  RegisterTest('NextClassLine', TTestNextClassLine.Suite);
  RegisterTest('NestedTypes', TTestNestedTypes.Suite);
end.
