unit GX_UnitExportList;

{$I 'GX_CondDefine.inc'}

interface

uses
  Windows,
  SysUtils,
  Classes,
  Contnrs,
  StrUtils,
{$IFDEF unicode}
  AnsiStrings,
{$ENDIF}
  u_dzTypes;

const
  CURRENT_UNIT_CACHE_VERSION = 'GExpertsUsesClauseManagerCacheVersion3';

type
  TUnitIdentifierTypes = (itUnknown, itConst, itType, itVar, itProcedure, itFunction);

type
  TUnitIdentifier = class
  private
    FIdentifier: AnsiString;
    FLCIdentifier: AnsiString;
    FLineNo: Integer;
  public
    class function FromLine(_s: PAnsiChar): TUnitIdentifier;
    constructor Create(const _Identifier: AnsiString; _LineNo: Integer);
    function AsLine: AnsiString;
    property Identifier: AnsiString read FIdentifier;
    property LCIdentifier: AnsiString read FLCIdentifier;
    property LineNo: Integer read FLineNo;
  end;

type
  TUnitIdentifierListAbstract = class
  private
    FItems: TObjectList;
    FIsSorted: Boolean;
  protected
    function CompareItems(_Idx1, _Idx2: Integer): Integer; virtual; abstract;
    function CompareToItem(const _Key; _Idx: Integer): Integer;
  public
    constructor Create(_Capacity: Integer);
    destructor Destroy; override;
    procedure ShrinkToCapacity;
    function Count: Integer;
    procedure Sort;
    function SearchStart(const _Words: TStrings; _Found: TList): Boolean;
    function SearchStartFirst(const _Words: TStrings; _Found: TList): Boolean;
    function SearchAnywhere(const _Words: TStrings; _Found: TList): Boolean;
    procedure AssignTo(_sl: TList);
  end;

type
  TUnitIdentifierList = class(TUnitIdentifierListAbstract)
  private
    function GetItems(_Idx: Integer): TUnitIdentifier;
  protected
    function CompareItems(_Idx1, _Idx2: Integer): Integer; override;
  public
    function Add(const _Identifier: AnsiString; _LineNo: Integer): Integer;
    ///<summary>
    /// @returns true, if the cache file has been read. This only happes for the latest
    ///                structure version (CURRENT_UNIT_CACHE_VERSION) </summary>
    function LoadFromFile(const _fn: string): Boolean;
    procedure SaveToFile(const _fn: string);
    property Items[_Idx: Integer]: TUnitIdentifier read GetItems; default;
  end;

type
  TUnitExport = class(TUnitIdentifier)
  private
    FFilename: string;
  public
    constructor Create(const _Identifier: AnsiString; const _Filename: string; _LineNo: Integer);
    property FileName: string read FFilename;
  end;

type
  TUnitExportlist = class(TUnitIdentifierListAbstract)
  private
    function GetItems(_Idx: Integer): TUnitExport;
  protected
    function CompareItems(_Idx1, _Idx2: Integer): Integer; override;
  public
    function Add(const _Identifier: AnsiString; const _Filename: string; _LineNo: Integer): Integer;
    property Items[_Idx: Integer]: TUnitExport read GetItems; default;
  end;

implementation

uses
  Math,
  u_dzConvertUtils,
  u_dzQuicksort,
  u_dzStringUtils,
  u_dzClassUtils;

{ TUnitIdentifier }

constructor TUnitIdentifier.Create(const _Identifier: AnsiString; _LineNo: Integer);
begin
  inherited Create;
  FIdentifier := _Identifier;
  FLCIdentifier := LowerCase(_Identifier);
  FLineNo := _LineNo;
end;

type
  TUnitIdentifierTypeChars = array[TUnitIdentifierTypes] of AnsiChar;
const
  UNIT_IDENTIFIER_TYPE_CHARS: TUnitIdentifierTypeChars = (
    'U', 'C', 'T', 'V', 'P', 'F');

function TUnitIdentifier.AsLine: AnsiString;
begin
  Result := UNIT_IDENTIFIER_TYPE_CHARS[itUnknown] + AnsiChar(#9) + Long2DecA(FLineNo) + AnsiChar(#9) + FIdentifier;
end;

class function TUnitIdentifier.FromLine(_s: PAnsiChar): TUnitIdentifier;
var
  Parts: array of PAnsiChar;
  Idx: Integer;
  l: ulong;
  LLineNo: Integer;
  p: PAnsiChar;
  Start: PAnsiChar;
begin
  Start := _s;
  Idx := 0;
  p := Start;
  while p^ <> #0 do begin
    if p^ <> #9 then begin
      Inc(p);
    end else begin
      SetLength(Parts, Idx + 1);
      Parts[Idx] := Start;
      Inc(Idx);
      // change the TAB character to #0 to mark the end of the string
      p^ := #0;
      Inc(p);
      Start := p;
    end;
  end;
  SetLength(Parts, Idx + 1);
  Parts[Idx] := Start;

  // todo: also use the type
  if TryDec2Long(Parts[1], l) then
    LLineNo := l
  else
    LLineNo := -1;
  Result := TUnitIdentifier.Create(Parts[2], LLineNo);
end;

{ TUnitExport }

constructor TUnitExport.Create(const _Identifier: AnsiString; const _Filename: string; _LineNo: Integer);
begin
  inherited Create(_Identifier, _LineNo);
  FFilename := _Filename;
end;

{ TUnitIdentifierListAbstract }

constructor TUnitIdentifierListAbstract.Create(_Capacity: Integer);
begin
  inherited Create;
  FItems := TObjectList.Create;
  FItems.Capacity := _Capacity;
end;

destructor TUnitIdentifierListAbstract.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

function TUnitIdentifierListAbstract.CompareToItem(const _Key; _Idx: Integer): Integer;
begin
  Result := CompareStr(AnsiString(_Key), (FItems[_Idx] as TUnitIdentifier).LCIdentifier);
end;

function TUnitIdentifierListAbstract.Count: Integer;
begin
  Result := FItems.Count;
end;

function StrMatchesAll(const _s: AnsiString; _Words: TStrings): Boolean;
var
  i: Integer;
begin
  for i := 0 to _Words.Count - 1 do begin
    if not ContainsStr(_s, AnsiString(_Words[i])) then begin
      Result := False;
      Exit; //==>
    end;
  end;
  Result := True;
end;

function TUnitIdentifierListAbstract.SearchAnywhere(const _Words: TStrings; _Found: TList): Boolean;
var
  LcWords: TStrings;
  i: Integer;
  Item: TUnitIdentifier;
begin
  _Found.Clear;
  LcWords := TStringList.Create;
  try
    for i := 0 to _Words.Count - 1 do
      LcWords.Add(LowerCase(_Words[i]));
    for i := 0 to Count - 1 do begin
      Item := FItems[i] as TUnitIdentifier;
      if StrMatchesAll(Item.LCIdentifier, LcWords) then
        _Found.Add(Item);
    end;
  finally
    FreeAndNil(LcWords);
  end;
  Result := (_Found.Count > 0);
end;

procedure RemoveNonMatchingAdditional(_Found: TList; _Words: TStrings);
var
  LcWords: TStrings;
  WordIdx: Integer;
  ItemIdx: Integer;
  Item: TUnitIdentifier;
begin
  if (_Words.Count = 0) or (_Found.Count = 0) then
    Exit; //==>

  LcWords := TStringList.Create;
  try
    // convert to lower case once instead of comparing case insentively
    // start with the second  item because we already know that the first one matches
    for WordIdx := 1 to _Words.Count - 1 do
      LcWords.Add(LowerCase(_Words[WordIdx]));

    // backwards, because we remove items that do not match
    for ItemIdx := _Found.Count - 1 downto 0 do begin
      for WordIdx := 0 to LcWords.Count - 1 do begin
        Item := _Found[ItemIdx];
        if not StrMatchesAll(Item.LCIdentifier, LcWords) then
          _Found.Delete(ItemIdx);
      end;
    end;
  finally
    FreeAndNil(LcWords);
  end;
end;

function TUnitIdentifierListAbstract.SearchStart(const _Words: TStrings; _Found: TList): Boolean;

  function StartsIdentifier(const _LcStart: AnsiString; _Idx: Integer; out _Item: TUnitIdentifier): Boolean;
  begin
    Result := (_Idx < Count);
    if Result then begin
      _Item := FItems[_Idx] as TUnitIdentifier;
      Result := StartsStr(_LcStart, _Item.LCIdentifier)
    end;
  end;

var
  ItemIdx: Integer;
  s: AnsiString;
  Item: TUnitIdentifier;
begin
  Assert(Assigned(_Words));
  Assert(Assigned(_Found));
  Assert(_Words.Count > 0);

  _Found.Clear;

  s := AnsiString(LowerCase(_Words[0]));
  BinarySearch(0, Count - 1, ItemIdx, s, CompareToItem, True);
  while StartsIdentifier(s, ItemIdx, Item) do begin
    _Found.Add(Item);
    Inc(ItemIdx);
  end;

  RemoveNonMatchingAdditional(_Found, _Words);

  Result := (_Found.Count > 0);
end;

function TUnitIdentifierListAbstract.SearchStartFirst(const _Words: TStrings; _Found: TList): Boolean;
var
  ItemIdx: Integer;
  s: AnsiString;
  NotAtStart: TList;
  Item: TUnitIdentifier;
  p: Integer;
begin
  Assert(Assigned(_Words));
  Assert(Assigned(_Found));
  Assert(_Words.Count > 0);

  _Found.Clear;
  s := AnsiString(LowerCase(_Words[0]));
  NotAtStart := TList.Create;
  try
    for ItemIdx := 0 to Count - 1 do begin
      Item := FItems[ItemIdx] as TUnitIdentifier;
      p := Pos(s, Item.LCIdentifier);
      if p = 1 then
        _Found.Add(Item)
      else if p > 1 then
        NotAtStart.Add(Item);
    end;
    _Found.Capacity := _Found.Count + NotAtStart.Count;
    for ItemIdx := 0 to NotAtStart.Count - 1 do
      _Found.Add(NotAtStart[ItemIdx]);
  finally
    FreeAndNil(NotAtStart);
  end;

  RemoveNonMatchingAdditional(_Found, _Words);

  Result := (_Found.Count > 0);
end;

procedure TUnitIdentifierListAbstract.AssignTo(_sl: TList);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    _sl.Add(FItems[i]);
end;

procedure TUnitIdentifierListAbstract.ShrinkToCapacity;
begin
  FItems.Capacity := FItems.Count;
end;

procedure TUnitIdentifierListAbstract.Sort;
begin
  QuickSort(0, Count - 1, CompareItems, FItems.Exchange);
end;

{ TUnitExportlist }

function TUnitExportlist.CompareItems(_Idx1, _Idx2: Integer): Integer;
var
  Item1: TUnitExport;
  Item2: TUnitExport;
begin
  Item1 := TUnitExport(FItems[_Idx1]);
  Item2 := TUnitExport(FItems[_Idx2]);
  Result := CompareStr(Item1.LCIdentifier, Item2.LCIdentifier);
  if Result = 0 then begin
    Result := CompareStr(Item1.FileName, Item2.FileName);
    if Result = 0 then
      Result := CompareValue(Item1.LineNo, Item2.LineNo);
  end;
end;

function TUnitExportlist.GetItems(_Idx: Integer): TUnitExport;
begin
  Result := TUnitExport(FItems[_Idx]);
end;

function TUnitExportlist.Add(const _Identifier: AnsiString; const _Filename: string; _LineNo: Integer): Integer;
begin
  Result := FItems.Add(TUnitExport.Create(_Identifier, _Filename, _LineNo));
  FIsSorted := False;
end;

{ TUnitIdentifierList }

function TUnitIdentifierList.Add(const _Identifier: AnsiString; _LineNo: Integer): Integer;
begin
  Result := FItems.Add(TUnitIdentifier.Create(_Identifier, _LineNo));
  FIsSorted := False;
end;

function TUnitIdentifierList.CompareItems(_Idx1, _Idx2: Integer): Integer;
var
  Item1: TUnitIdentifier;
  Item2: TUnitIdentifier;
begin
  Item1 := FItems[_Idx1] as TUnitIdentifier;
  Item2 := FItems[_Idx2] as TUnitIdentifier;
  Result := AnsiCompareStr(Item1.LCIdentifier, Item2.LCIdentifier);
  if Result = 0 then
    Result := CompareValue(Item1.LineNo, Item2.LineNo);
end;

function TUnitIdentifierList.GetItems(_Idx: Integer): TUnitIdentifier;
begin
  Result := FItems[_Idx] as TUnitIdentifier;
end;

function TUnitIdentifierList.LoadFromFile(const _fn: string): Boolean;

  function TryGetNextLine(const _Buffer: RawByteString; var _Start: PAnsiChar; out _Line: PAnsiChar): Boolean;
  var
    p: PAnsiChar;
  begin
    if _Start^ = #0 then begin
      Result := False;
      Exit; //==>
    end;

    _Line := _Start;
    p := _Start;
    while (p^ <> #0) and (p^ <> #13) do
      Inc(p);
    if p^ = #0 then begin
      _Start := p;
      Result := True;
      Exit; //==>
    end;

    _Start := p;
    // overwrite the #13 with #0 as an end  marker for the line
    p^ := #0;
    Inc(_Start);
    if _Start^ = #10 then
      Inc(_Start);
    Result := True;
  end;

var
  st: TFileStream;
  Buffer: RawByteString;
  Len: Integer;
  Start: PAnsiChar;
  Line: PAnsiChar;
begin
  FItems.Clear;
  st := TFileStream.Create(_fn, fmOpenRead or fmShareDenyWrite);
  try
    Len := st.Size;
    if Len = 0 then begin
      Result := False;
      Exit //==>
    end;

    SetLength(Buffer, Len);
    Start := @Buffer[1];
    st.Read(Start^, Len);
    if not TryGetNextLine(Buffer, Start, Line) then begin
      Result := False;
      Exit; //==>
    end;

    Result := (Line = CURRENT_UNIT_CACHE_VERSION);
    if not Result then begin
      // we don't support older cache files it's not worth the effort
      Exit; //==>
    end;

    while TryGetNextLine(Buffer, Start, Line) do begin
      FItems.Add(TUnitIdentifier.FromLine(Line));
    end;
  finally
    FreeAndNil(st);
  end;
end;

procedure TUnitIdentifierList.SaveToFile(const _fn: string);
var
  i: Integer;
  st: TFileStream;
begin
  st := TFileStream.Create(_fn, fmCreate or fmShareExclusive);
  try
    TStream_WriteStringLn(st, CURRENT_UNIT_CACHE_VERSION);
    for i := 0 to Count - 1 do begin
      TStream_WriteStringLn(st, Items[i].AsLine);
    end;
  finally
    FreeAndNil(st);
  end;
end;

end.
