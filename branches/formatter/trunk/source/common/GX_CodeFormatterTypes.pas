// Simple types used in the code formatter

// Original Author:     Egbert van Nes (http://www.dow.wau.nl/aew/People/Egbert_van_Nes.html)
// Contributors:        Thomas Mueller (http://www.dummzeuch.de)

unit GX_CodeFormatterTypes;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils,
  Classes,
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

function SpaceSetToInt(ASpaceSet: TSpaceSet): Integer;
function IntToSpaceSet(AValue: Integer): TSpaceSet;

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
    rtLeftBr, rtRightBr, rtLeftHook, rtRightHook, rtMathOper, rtAssignOper,
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

type
  {: stores all known reserved words in lower case with their associated type,
     must be ordered perfectly on words!!
     NOTE: This is for Delphi 2005, there are some words that aren't reserved
           in earlier Delphi versions, maybe that should be configurable?
           That could be done by converting this list into a oObjects.TStrCollection
           which only contains those words that are known for the configured Delphi
           version. }
  TReservedWordList = class
  private
    FWords: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    function FindWord(const _s: string; out _ReservedType: TReservedType): Boolean;
    procedure AddWord(const _s: string; _ReservedType: TReservedType);
  end;

var
  ReservedWordList: TReservedWordList = nil;

type
  {: Holds a special "option" for a tpascal token, some of then is just override a setting or behavior }
  TTokenOption = (toFeedNewLine);
  TTokenOptions = set of TTokenOption;

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

{$IFDEF GX_VER200_up} // delphi 2009
uses
  AnsiStrings;
{$ENDIF}

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
        while (Result[i] = ' ') or (Result[i] = Tab) do
          Inc(i);
        Result[i] := UpCase(Result[i]);
      end;
  else
    Result := aStr;
  end;
end;

{ TKeywordColl }

function SpaceSetToInt(ASpaceSet: TSpaceSet): Integer;
begin
  Result := 0;
  Move(ASpaceSet, Result, SizeOf(ASpaceSet));
end;

function IntToSpaceSet(AValue: Integer): TSpaceSet;
begin
  Result := [];
  Move(AValue, Result, SizeOf(Result));
end;

{ TReservedWordList }

constructor TReservedWordList.Create;
begin
  inherited Create;
  FWords := TStringList.Create;
  FWords.Sorted := True;
  FWords.Duplicates := dupError;

  AddWord('absolute', rtAbsolute);
  AddWord('abstract', rtFuncDirective);
  AddWord('and', rtOper);
  AddWord('array', rtReserved);
  AddWord('as', rtOper);
  AddWord('asm', rtAsm);
  AddWord('assembler', rtFuncDirective);
  AddWord('automated', rtVisibility);
  AddWord('begin', rtBegin);
  AddWord('case', rtCase);
  AddWord('cdecl', rtFuncDirective);
  AddWord('class', rtClass);
  AddWord('const', rtVar);
  AddWord('constructor', rtProcedure);
  AddWord('contains', rtUses);
  AddWord('default', rtDefault);
  AddWord('deprecated', rtFuncDirective);
  AddWord('destructor', rtProcedure);
  AddWord('dispid', rtFuncDirective);
  AddWord('dispinterface', rtInterface);
  AddWord('div', rtOper);
  AddWord('do', rtDo);
  AddWord('downto', rtOper);
  AddWord('dynamic', rtFuncDirective);
  AddWord('else', rtElse);
  AddWord('end', rtEnd);
  AddWord('except', rtExcept);
  AddWord('export', rtFuncDirective);
  AddWord('exports', rtUses);
  AddWord('external', rtForward);
  AddWord('far', rtFuncDirective);
  AddWord('file', rtReserved);
  AddWord('finalization', rtInitialization);
  AddWord('finally', rtExcept);
  AddWord('for', rtWhile);
  AddWord('forward', rtForward);
  AddWord('function', rtProcedure);
  AddWord('goto', rtReserved);
  AddWord('helper', rtReserved);
  AddWord('if', rtIf);
  AddWord('implementation', rtImplementation);
  AddWord('implements', rtFuncDirective);
  AddWord('in', rtOper);
  AddWord('index', rtFuncDirective);
  AddWord('inherited', rtReserved);
  AddWord('initialization', rtInitialization);
  AddWord('inline', rtFuncDirective);
  AddWord('interface', rtInterface);
  AddWord('is', rtOper);
  AddWord('label', rtVar);
  AddWord('library', rtFuncDirective);
  AddWord('message', rtFuncDirective);
  AddWord('mod', rtOper);
  AddWord('name', rtFuncDirective);
  AddWord('near', rtFuncDirective);
  AddWord('nil', rtReserved);
  AddWord('nodefault', rtFuncDirective);
  AddWord('not', rtOper);
  AddWord('object', rtClass);
  AddWord('of', rtOf);
  AddWord('on', rtOn);
  AddWord('operator', rtProcedure);
  AddWord('or', rtOper);
  AddWord('out', rtReserved);
  AddWord('overload', rtFuncDirective);
  AddWord('override', rtFuncDirective);
  AddWord('packed', rtReserved);
  AddWord('pascal', rtFuncDirective);
  AddWord('platform', rtFuncDirective);
  AddWord('private', rtVisibility);
  AddWord('procedure', rtProcedure);
  AddWord('program', rtProgram);
  AddWord('property', rtProcedure);
  AddWord('protected', rtVisibility);
  AddWord('public', rtVisibility);
  AddWord('published', rtVisibility);
  AddWord('raise', rtReserved);
  AddWord('read', rtFuncDirective);
  AddWord('readonly', rtFuncDirective);
  AddWord('record', rtRecord);
  AddWord('reference', rtReserved);
  AddWord('register', rtFuncDirective);
  AddWord('reintroduce', rtFuncDirective);
  AddWord('repeat', rtRepeat);
  AddWord('requires', rtUses);
  AddWord('resident', rtFuncDirective);
  AddWord('resourcestring', rtVar);
  AddWord('safecall', rtFuncDirective);
  AddWord('set', rtReserved);
  AddWord('shl', rtOper);
  AddWord('shr', rtOper);
  AddWord('static', rtFuncDirective);
  AddWord('stdcall', rtFuncDirective);
  AddWord('stored', rtFuncDirective);
  AddWord('strict', rtVisibility);
  AddWord('string', rtReserved);
  AddWord('then', rtThen);
  AddWord('threadvar', rtVar);
  AddWord('to', rtOper);
  AddWord('try', rtTry);
  AddWord('type', rtType);
  AddWord('unit', rtProgram);
  AddWord('until', rtUntil);
  AddWord('uses', rtUses);
  AddWord('var', rtVar);
  AddWord('virtual', rtFuncDirective);
  AddWord('while', rtWhile);
  AddWord('with', rtWhile);
  AddWord('write', rtFuncDirective);
  AddWord('writeonly', rtFuncDirective);
  AddWord('xor', rtOper);
end;

destructor TReservedWordList.Destroy;
begin
  FreeAndNil(FWords);
  inherited;
end;

procedure TReservedWordList.AddWord(const _s: string; _ReservedType: TReservedType);
begin
  FWords.AddObject(LowerCase(_s), Pointer(Ord(_ReservedType)));
end;

function TReservedWordList.FindWord(const _s: string; out _ReservedType: TReservedType): Boolean;
var
  Idx: Integer;
begin
  Result := FWords.Find(LowerCase(_s), Idx);
  if Result then
    _ReservedType := TReservedType(Integer(FWords.Objects[Idx]));
end;

initialization
  ReservedWordList := TReservedWordList.Create;
finalization
  FreeAndNil(ReservedWordList);
end.

