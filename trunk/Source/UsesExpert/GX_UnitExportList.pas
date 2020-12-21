unit GX_UnitExportList;

{$I 'GX_CondDefine.inc'}

interface

uses
  Windows,
  SysUtils,
  Classes,
  Contnrs,
  u_dzTypes;

type
  TUnitIdentifierTypes = (itUnknown, itConst, itType, itVar, itProcedure, itFunction);

type
  TUnitIdentifier = class
  private
    FIdentifier: string;
    FLCIdentifier: string;
    FLineNo: Integer;
  public
    class function FromString(const _s: string): TUnitIdentifier;
    constructor Create(const _Identifier: string; _LineNo: Integer);
    function AsString: string;
    property Identifier: string read FIdentifier;
    property LCIdentifier: string read FLCIdentifier;
    property LineNo: Integer read FLineNo;
  end;

type
  TUnitIdentifierListAbstract = class
  private
    FItems: TObjectList;
    FIsSorted: Boolean;
  protected
    function CompareItems(_Idx1, _Idx2: Integer): Integer; virtual; abstract;
    procedure SwapItems(_Idx1, _Idx2: Integer);
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
    function Add(const _Identifier: string; _LineNo: Integer): Integer;
    procedure LoadFromFile(const _fn: string);
    procedure SaveToFile(const _fn: string);
    property Items[_Idx: Integer]: TUnitIdentifier read GetItems; default;
  end;

type
  TUnitExport = class(TUnitIdentifier)
  private
    FFilename: string;
  public
    constructor Create(const _Identifier, _Filename: string; _LineNo: Integer);
    property FileName: string read FFilename;
  end;

type
  TUnitExportlist = class(TUnitIdentifierListAbstract)
  private
    function GetItems(_Idx: Integer): TUnitExport;
  protected
    function CompareItems(_Idx1, _Idx2: Integer): Integer; override;
  public
    function Add(const _Identifier, _Filename: string; _LineNo: Integer): Integer;
    property Items[_Idx: Integer]: TUnitExport read GetItems; default;
  end;

implementation

uses
  StrUtils,
  u_dzQuicksort,
  u_dzStringUtils;

{ TUnitIdentifier }

constructor TUnitIdentifier.Create(const _Identifier: string; _LineNo: Integer);
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

function TUnitIdentifier.AsString: string;
begin
  Result := UNIT_IDENTIFIER_TYPE_CHARS[itUnknown] + #9 + IntToStr(FLineNo) + #9 + FIdentifier;
end;

class function TUnitIdentifier.FromString(const _s: string): TUnitIdentifier;
var
  Parts: TStringArray;
  LineNo: Integer;
begin
  Parts := u_dzStringUtils.SplitString(_s, [#9]);
  if Length(Parts) = 3 then begin
    // todo: also read the type
    if not TryStrToInt(Parts[1], LineNo) then
      LineNo := -1;
    Result := TUnitIdentifier.Create(Parts[2], LineNo);
  end else
    Result := TUnitIdentifier.Create(_s, -1);
end;

{ TUnitExport }

constructor TUnitExport.Create(const _Identifier, _Filename: string; _LineNo: Integer);
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
  Result := AnsiCompareStr(string(_Key), (FItems[_Idx] as TUnitIdentifier).LCIdentifier);
end;

function TUnitIdentifierListAbstract.Count: Integer;
begin
  Result := FItems.Count;
end;

function StrMatchesAll(const _s: string; _Words: TStrings): Boolean;
var
  i: Integer;
begin
  for i := 0 to _Words.Count - 1 do begin
    if not AnsiContainsStr(_s, _Words[i]) then begin
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
    // start with the second  item because we alreay know that the first on matches
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

  function StartsIdentifier(const _LcStart: string; _Idx: Integer; out _Item: TUnitIdentifier): Boolean;
  begin
    Result := (_Idx < Count);
    if Result then begin
      _Item := FItems[_Idx] as TUnitIdentifier;
      Result := StartsStr(_LcStart, _Item.LCIdentifier)
    end;
  end;

var
  ItemIdx: Integer;
  s: string;
  Item: TUnitIdentifier;
begin
  Assert(Assigned(_Words));
  Assert(Assigned(_Found));
  Assert(_Words.Count > 0);

  _Found.Clear;

  s := LowerCase(_Words[0]);
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
  s: string;
  NotAtStart: TList;
  Item: TUnitIdentifier;
  p: Integer;
begin
  Assert(Assigned(_Words));
  Assert(Assigned(_Found));
  Assert(_Words.Count > 0);

  _Found.Clear;
  s := LowerCase(_Words[0]);
  NotAtStart := TList.Create;
  try
    for ItemIdx := 0 to Count - 1 do begin
      Item := FItems[ItemIdx] as TUnitIdentifier;
      p := AnsiPos(s, Item.LCIdentifier);
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
  QuickSort(0, Count - 1, CompareItems, SwapItems);
end;

procedure TUnitIdentifierListAbstract.SwapItems(_Idx1, _Idx2: Integer);
begin
  FItems.Exchange(_Idx1, _Idx2);
end;

{ TUnitExportlist }

function TUnitExportlist.CompareItems(_Idx1, _Idx2: Integer): Integer;
var
  Item1: TUnitExport;
  Item2: TUnitExport;
begin
  Item1 := FItems[_Idx1] as TUnitExport;
  Item2 := FItems[_Idx2] as TUnitExport;
  Result := AnsiCompareStr(Item1.LCIdentifier, Item2.LCIdentifier);
  if Result = 0 then
    Result := AnsiCompareStr(Item1.FileName, Item2.FileName);
end;

function TUnitExportlist.GetItems(_Idx: Integer): TUnitExport;
begin
  Result := TUnitExport(FItems[_Idx]);
end;

function TUnitExportlist.Add(const _Identifier, _Filename: string; _LineNo: Integer): Integer;
begin
  Result := FItems.Add(TUnitExport.Create(_Identifier, _Filename, _LineNo));
  FIsSorted := False;
end;

{ TUnitIdentifierList }

function TUnitIdentifierList.Add(const _Identifier: string; _LineNo: Integer): Integer;
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
end;

function TUnitIdentifierList.GetItems(_Idx: Integer): TUnitIdentifier;
begin
  Result := FItems[_Idx] as TUnitIdentifier;
end;

procedure TUnitIdentifierList.LoadFromFile(const _fn: string);
var
  StartIdx: Integer;
  i: Integer;
  sl: TStringList;
begin
  FItems.Clear;
  sl := TStringList.Create;
  try
    sl.LoadFromFile(_fn);
    if sl.Count = 0 then
      Exit; //==>
    if sl[0] = 'Version2' then
      StartIdx := 1
    else
      StartIdx := 0;
    for i := StartIdx to sl.Count - 1 do begin
      FItems.Add(TUnitIdentifier.FromString(sl[i]));
    end;
    sl.SaveToFile(_fn);
  finally
    FreeAndNil(sl);
  end;

end;

procedure TUnitIdentifierList.SaveToFile(const _fn: string);
var
  i: Integer;
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.Add('Version2');
    for i := 0 to Count - 1 do begin
      sl.Add(Items[i].AsString);
    end;
    sl.SaveToFile(_fn);
  finally
    FreeAndNil(sl);
  end;
end;

end.
