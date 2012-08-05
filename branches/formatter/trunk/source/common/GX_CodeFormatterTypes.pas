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

//function TKeywordColl.Compare(Key1, Key2: Pointer): Integer;
//begin
//  Result := StrIComp(Key1, Key2);
//end;

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

procedure InitReservedWords;
begin
  ReservedWordList := TReservedWordList.Create;
  ReservedWordList.AddWord('absolute', rtAbsolute);
  ReservedWordList.AddWord('abstract', rtFuncDirective);
  ReservedWordList.AddWord('and', rtOper);
  ReservedWordList.AddWord('array', rtReserved);
  ReservedWordList.AddWord('as', rtOper);
  ReservedWordList.AddWord('asm', rtAsm);
  ReservedWordList.AddWord('assembler', rtFuncDirective);
  ReservedWordList.AddWord('automated', rtVisibility);
  ReservedWordList.AddWord('begin', rtBegin);
  ReservedWordList.AddWord('case', rtCase);
  ReservedWordList.AddWord('cdecl', rtFuncDirective);
  ReservedWordList.AddWord('class', rtClass);
  ReservedWordList.AddWord('const', rtVar);
  ReservedWordList.AddWord('constructor', rtProcedure);
  ReservedWordList.AddWord('contains', rtUses);
  ReservedWordList.AddWord('default', rtDefault);
  ReservedWordList.AddWord('deprecated', rtFuncDirective);
  ReservedWordList.AddWord('destructor', rtProcedure);
  ReservedWordList.AddWord('dispid', rtFuncDirective);
  ReservedWordList.AddWord('dispinterface', rtInterface);
  ReservedWordList.AddWord('div', rtOper);
  ReservedWordList.AddWord('do', rtDo);
  ReservedWordList.AddWord('downto', rtOper);
  ReservedWordList.AddWord('dynamic', rtFuncDirective);
  ReservedWordList.AddWord('else', rtElse);
  ReservedWordList.AddWord('end', rtEnd);
  ReservedWordList.AddWord('except', rtExcept);
  ReservedWordList.AddWord('export', rtFuncDirective);
  ReservedWordList.AddWord('exports', rtUses);
  ReservedWordList.AddWord('external', rtForward);
  ReservedWordList.AddWord('far', rtFuncDirective);
  ReservedWordList.AddWord('file', rtReserved);
  ReservedWordList.AddWord('finalization', rtInitialization);
  ReservedWordList.AddWord('finally', rtExcept);
  ReservedWordList.AddWord('for', rtWhile);
  ReservedWordList.AddWord('forward', rtForward);
  ReservedWordList.AddWord('function', rtProcedure);
  ReservedWordList.AddWord('goto', rtReserved);
  ReservedWordList.AddWord('helper', rtReserved);
  ReservedWordList.AddWord('if', rtIf);
  ReservedWordList.AddWord('implementation', rtImplementation);
  ReservedWordList.AddWord('implements', rtFuncDirective);
  ReservedWordList.AddWord('in', rtOper);
  ReservedWordList.AddWord('index', rtFuncDirective);
  ReservedWordList.AddWord('inherited', rtReserved);
  ReservedWordList.AddWord('initialization', rtInitialization);
  ReservedWordList.AddWord('inline', rtFuncDirective);
  ReservedWordList.AddWord('interface', rtInterface);
  ReservedWordList.AddWord('is', rtOper);
  ReservedWordList.AddWord('label', rtVar);
  ReservedWordList.AddWord('library', rtFuncDirective);
  ReservedWordList.AddWord('message', rtFuncDirective);
  ReservedWordList.AddWord('mod', rtOper);
  ReservedWordList.AddWord('name', rtFuncDirective);
  ReservedWordList.AddWord('near', rtFuncDirective);
  ReservedWordList.AddWord('nil', rtReserved);
  ReservedWordList.AddWord('nodefault', rtFuncDirective);
  ReservedWordList.AddWord('not', rtOper);
  ReservedWordList.AddWord('object', rtClass);
  ReservedWordList.AddWord('of', rtOf);
  ReservedWordList.AddWord('on', rtOn);
  ReservedWordList.AddWord('operator', rtProcedure);
  ReservedWordList.AddWord('or', rtOper);
  ReservedWordList.AddWord('out', rtReserved);
  ReservedWordList.AddWord('overload', rtFuncDirective);
  ReservedWordList.AddWord('override', rtFuncDirective);
  ReservedWordList.AddWord('packed', rtReserved);
  ReservedWordList.AddWord('pascal', rtFuncDirective);
  ReservedWordList.AddWord('platform', rtFuncDirective);
  ReservedWordList.AddWord('private', rtVisibility);
  ReservedWordList.AddWord('procedure', rtProcedure);
  ReservedWordList.AddWord('program', rtProgram);
  ReservedWordList.AddWord('property', rtProcedure);
  ReservedWordList.AddWord('protected', rtVisibility);
  ReservedWordList.AddWord('public', rtVisibility);
  ReservedWordList.AddWord('published', rtVisibility);
  ReservedWordList.AddWord('raise', rtReserved);
  ReservedWordList.AddWord('read', rtFuncDirective);
  ReservedWordList.AddWord('readonly', rtFuncDirective);
  ReservedWordList.AddWord('record', rtRecord);
  ReservedWordList.AddWord('reference', rtReserved);
  ReservedWordList.AddWord('register', rtFuncDirective);
  ReservedWordList.AddWord('reintroduce', rtFuncDirective);
  ReservedWordList.AddWord('repeat', rtRepeat);
  ReservedWordList.AddWord('requires', rtUses);
  ReservedWordList.AddWord('resident', rtFuncDirective);
  ReservedWordList.AddWord('resourcestring', rtVar);
  ReservedWordList.AddWord('safecall', rtFuncDirective);
  ReservedWordList.AddWord('set', rtReserved);
  ReservedWordList.AddWord('shl', rtOper);
  ReservedWordList.AddWord('shr', rtOper);
  ReservedWordList.AddWord('static', rtFuncDirective);
  ReservedWordList.AddWord('stdcall', rtFuncDirective);
  ReservedWordList.AddWord('stored', rtFuncDirective);
  ReservedWordList.AddWord('strict', rtVisibility);
  ReservedWordList.AddWord('string', rtReserved);
  ReservedWordList.AddWord('then', rtThen);
  ReservedWordList.AddWord('threadvar', rtVar);
  ReservedWordList.AddWord('to', rtOper);
  ReservedWordList.AddWord('try', rtTry);
  ReservedWordList.AddWord('type', rtType);
  ReservedWordList.AddWord('unit', rtProgram);
  ReservedWordList.AddWord('until', rtUntil);
  ReservedWordList.AddWord('uses', rtUses);
  ReservedWordList.AddWord('var', rtVar);
  ReservedWordList.AddWord('virtual', rtFuncDirective);
  ReservedWordList.AddWord('while', rtWhile);
  ReservedWordList.AddWord('with', rtWhile);
  ReservedWordList.AddWord('write', rtFuncDirective);
  ReservedWordList.AddWord('writeonly', rtFuncDirective);
  ReservedWordList.AddWord('xor', rtOper);
end;

initialization
  InitReservedWords;
finalization
  FreeAndNil(ReservedWordList);
end.

