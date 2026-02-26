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

initialization
  RegisterTest('NextObjectLine', TTestNextObjectLine.Suite);
  RegisterTest('NextClassLine', TTestNextClassLine.Suite);
end.
