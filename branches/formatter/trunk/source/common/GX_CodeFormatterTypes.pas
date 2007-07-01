// Simple types used in the code formatter
// Original Author:     Egbert van Nes (http://www.dow.wau.nl/aew/People/Egbert_van_Nes.html)
// Contributors:        Thomas Mueller (http://www.dummzeuch.de)
unit GX_CodeFormatterTypes;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils,
  GX_CollectionLikeLists;

type
  ECodeFormatter = class(Exception);

type
  TWordType = (wtLineFeed, wtSpaces, wtHalfComment, wtHalfStarComment,
    wtHalfOutComment, wtFullComment, wtFullOutComment, wtString, wtErrorString,
    wtOperator, wtWord, wtNumber, wtHexNumber, wtNothing, wtAsm, wtCompDirective);
  EFormatException = class(Exception);

const
  Maxline = 1024; // the maximum line length of the Delphi editor
//  CRLF = #13#10#0;
  NotPrintable = [#1..#8, #10..#14, #16..#19, #22..#31]; {not printable chars}
  Tab = #9;

type
  {: determines a word's casing:
     * rfLowerCase = all lowercase
     * rfUpperCase = all uppercase
     * rfFirstUp = first character upper case, all other lowercase
     * rfUnchanged = do not change anything }
  TCase = (rfLowerCase, rfUpperCase, rfFirstUp, rfUnchanged);

  TSpace = (spBefore, spAfter);
  TSpaceSet = set of TSpace;

function SpaceSetToInt(ASpaceSet: TSpaceSet): integer;
function IntToSpaceSet(AValue: integer): TSpaceSet;

const
  spBoth = [spBefore, spAfter];
  spNone = [];

type
  TReservedType = (rtNothing, rtReserved, rtOper, rtDirective,
    rtIf, rtDo, rtWhile, rtOn, rtVar, rtType, rtProcedure, rtAsm, rtTry,
    rtExcept,
    rtEnd, rtBegin, rtCase, rtOf, rtLineFeed, rtColon, rtSemiColon,
    rtThen, rtClass, rtClassDecl, rtProgram, rtRepeat, rtUntil, rtRecord,
    rtVisibility, rtElse, rtIfElse, rtInterface, rtImplementation,
    rtLeftBr, rtRightBr, rtLeftHook, rtRightHook, rtMathOper, rtEqualOper,
    rtMinus, rtPlus,
    rtLogOper, rtEquals, rtForward, rtDefault, rtInitialization, rtComma,
    rtUses, rtProcDeclare, rtFuncDirective, rtAbsolute, rtComment, rtRecCase, rtDot,
    rtCompIf, rtDotDot,
    rtCompElse, rtCompEndif);

const
  NoReservedTypes = [rtNothing, rtComma, rtColon, rtLineFeed, rtDefault,
    rtFuncDirective, rtAbsolute, rtComment, rtLeftBr, rtRightBr, rtForward,
    rtCompIf, rtCompElse, rtCompEndif, rtVisibility];

  StandardDirectives = [rtDefault, rtAbsolute, rtVisibility, rtFuncDirective,
    rtAbsolute, rtForward];

const
  NReservedWords = 109;
  {IndentProcedure = True;}

type
  {: stores a single reserved word with its associated type }
  TReservedRec = record
    ReservedType: TReservedType;
    Words: PChar;
  end;

  TReservedArray = array[0..NReservedWords - 1] of TReservedRec;

const
  {: stores all known reserved words in lower case with their associated type,
     must be ordered perfectly on words!!
     NOTE: This is for Delphi 2005, there are some words that aren't reserved
           in earlier Delphi versions, maybe that should be configurable?
           That could be done by converting this list into a oObjects.TStrCollection
           which only contains those words that are known for the configured Delphi
           version. }
  ReservedArray: TReservedArray = (
    (ReservedType: rtAbsolute; Words: 'absolute'),
    (ReservedType: rtFuncDirective; Words: 'abstract'),
    (ReservedType: rtOper; Words: 'and'),
    (ReservedType: rtReserved; Words: 'array'),
    (ReservedType: rtOper; Words: 'as'),
    (ReservedType: rtAsm; Words: 'asm'),
    (ReservedType: rtFuncDirective; Words: 'assembler'),
    (ReservedType: rtVisibility; Words: 'automated'),
    (ReservedType: rtBegin; Words: 'begin'),
    (ReservedType: rtCase; Words: 'case'),
    (ReservedType: rtFuncDirective; Words: 'cdecl'),
    (ReservedType: rtClass; Words: 'class'),
    (ReservedType: rtVar; Words: 'const'),
    (ReservedType: rtProcedure; Words: 'constructor'),
    (ReservedType: rtUses; Words: 'contains'),
    (ReservedType: rtDefault; Words: 'default'),
    (ReservedType: rtFuncDirective; Words: 'deprecated'),
    (ReservedType: rtProcedure; Words: 'destructor'),
    (ReservedType: rtFuncDirective; Words: 'dispid'),
    (ReservedType: rtInterface; Words: 'dispinterface'),
    (ReservedType: rtOper; Words: 'div'),
    (ReservedType: rtDo; Words: 'do'),
    (ReservedType: rtOper; Words: 'downto'),
    (ReservedType: rtFuncDirective; Words: 'dynamic'),
    (ReservedType: rtElse; Words: 'else'),
    (ReservedType: rtEnd; Words: 'end'),
    (ReservedType: rtExcept; Words: 'except'),
    (ReservedType: rtFuncDirective; Words: 'export'),
    (ReservedType: rtUses; Words: 'exports'),
    (ReservedType: rtForward; Words: 'external'),
    (ReservedType: rtFuncDirective; Words: 'far'),
    (ReservedType: rtReserved; Words: 'file'),
    (ReservedType: rtInitialization; Words: 'finalization'),
    (ReservedType: rtExcept; Words: 'finally'),
    (ReservedType: rtWhile; Words: 'for'),
    (ReservedType: rtForward; Words: 'forward'),
    (ReservedType: rtProcedure; Words: 'function'),
    (ReservedType: rtReserved; Words: 'goto'),
    (ReservedType: rtIf; Words: 'if'),
    (ReservedType: rtImplementation; Words: 'implementation'),
    (ReservedType: rtFuncDirective; Words: 'implements'),
    (ReservedType: rtOper; Words: 'in'),
    (ReservedType: rtFuncDirective; Words: 'index'),
    (ReservedType: rtReserved; Words: 'inherited'),
    (ReservedType: rtInitialization; Words: 'initialization'),
    (ReservedType: rtFuncDirective; Words: 'inline'), // was rtDirective for us in asm
    (ReservedType: rtInterface; Words: 'interface'),
    (ReservedType: rtOper; Words: 'is'),
    (ReservedType: rtVar; Words: 'label'),
    (ReservedType: rtFuncDirective; Words: 'library'),
    (ReservedType: rtFuncDirective; Words: 'message'),
    (ReservedType: rtOper; Words: 'mod'),
    (ReservedType: rtFuncDirective; Words: 'name'),
    (ReservedType: rtFuncDirective; Words: 'near'),
    (ReservedType: rtReserved; Words: 'nil'),
    (ReservedType: rtFuncDirective; Words: 'nodefault'),
    (ReservedType: rtOper; Words: 'not'),
    (ReservedType: rtClass; Words: 'object'),
    (ReservedType: rtOf; Words: 'of'),
    (ReservedType: rtOn; Words: 'on'),
    (ReservedType: rtProcedure; Words: 'operator'),
    (ReservedType: rtOper; Words: 'or'),
    (ReservedType: rtReserved; Words: 'out'),
    (ReservedType: rtFuncDirective; Words: 'overload'),
    (ReservedType: rtFuncDirective; Words: 'override'),
    (ReservedType: rtReserved; Words: 'packed'),
    (ReservedType: rtFuncDirective; Words: 'pascal'),
    (ReservedType: rtFuncDirective; Words: 'platform'),
    (ReservedType: rtVisibility; Words: 'private'),
    (ReservedType: rtProcedure; Words: 'procedure'),
    (ReservedType: rtProgram; Words: 'program'),
    (ReservedType: rtProcedure; Words: 'property'),
    (ReservedType: rtVisibility; Words: 'protected'),
    (ReservedType: rtVisibility; Words: 'public'),
    (ReservedType: rtVisibility; Words: 'published'),
    (ReservedType: rtReserved; Words: 'raise'),
    (ReservedType: rtFuncDirective; Words: 'read'),
    (ReservedType: rtFuncDirective; Words: 'readonly'),
    (ReservedType: rtRecord; Words: 'record'),
    (ReservedType: rtFuncDirective; Words: 'register'),
    (ReservedType: rtFuncDirective; Words: 'reintroduce'),
    (ReservedType: rtRepeat; Words: 'repeat'),
    (ReservedType: rtUses; Words: 'requires'),
    (ReservedType: rtFuncDirective; Words: 'resident'),
    (ReservedType: rtVar; Words: 'resourcestring'),
    (ReservedType: rtFuncDirective; Words: 'safecall'),
    (ReservedType: rtReserved; Words: 'set'),
    (ReservedType: rtOper; Words: 'shl'),
    (ReservedType: rtOper; Words: 'shr'),
    (ReservedType: rtFuncDirective; Words: 'static'),
    (ReservedType: rtFuncDirective; Words: 'stdcall'),
    (ReservedType: rtFuncDirective; Words: 'stored'),
    (ReservedType: rtVisibility; Words: 'strict'),
    (ReservedType: rtReserved; Words: 'string'),
    (ReservedType: rtThen; Words: 'then'),
    (ReservedType: rtVar; Words: 'threadvar'),
    (ReservedType: rtOper; Words: 'to'),
    (ReservedType: rtTry; Words: 'try'),
    (ReservedType: rtType; Words: 'type'),
    (ReservedType: rtProgram; Words: 'unit'),
    (ReservedType: rtUntil; Words: 'until'),
    (ReservedType: rtUses; Words: 'uses'),
    (ReservedType: rtVar; Words: 'var'),
    (ReservedType: rtFuncDirective; Words: 'virtual'),
    (ReservedType: rtWhile; Words: 'while'),
    (ReservedType: rtWhile; Words: 'with'),
    (ReservedType: rtFuncDirective; Words: 'write'),
    (ReservedType: rtFuncDirective; Words: 'writeonly'),
    (ReservedType: rtOper; Words: 'xor')
    );

//type
//  {: a TStrCollection that compares case insensitively }
//  TKeywordColl = class(TStrCollection)
//  public
//    {: compares Key1 and Key2 as strings, case insensitively }
//    function Compare(Key1, Key2: Pointer): Integer; override;
//  end;

{: changes the string case as specified in aCase
   @param aStr is the input string
   @param aCase is a TCase specifying the desired case
   @returns the modified string }
function AdjustCase(aStr: string; aCase: TCase): string;

implementation

function AdjustCase(aStr: string; aCase: TCase): string;
var
  i: Integer;
begin
  case aCase of
    rfUpperCase: Result := UpperCase(aStr);
    rfLowerCase: Result := LowerCase(aStr);
    rfFirstUp: begin
        Result := LowerCase(aStr);
        i := 1;
        while Result[i] in [' ', Tab] do
          Inc(i);
        Result[i] := UpCase(Result[i]);
      end;
  else
    Result := aStr;
  end;
end;

{ TKeywordColl }

//function TKeywordColl.Compare(Key1, Key2: Pointer): Integer;
//begin
//  Result := StrIComp(Key1, Key2);
//end;

function SpaceSetToInt(ASpaceSet: TSpaceSet): integer;
begin
  Result := 0;
  Move(ASpaceSet, Result, SizeOf(ASpaceSet));
end;

function IntToSpaceSet(AValue: integer): TSpaceSet;
begin
  Result := [];
  Move(AValue, Result, SizeOf(Result));
end;

end.

