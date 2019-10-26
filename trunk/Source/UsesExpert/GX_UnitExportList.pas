unit GX_UnitExportList;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Contnrs;

type
  TUnitExport = class
  private
    FFilename: string;
    FIdentifier: string;
    FLCIdentifier: string;
  public
    constructor Create(const _Identifier, _Filename: string);
    property Identifier: string read FIdentifier;
    property LCIdentifier: string read FLCIdentifier;
    property FileName: string read FFilename;
  end;

type
  TUnitExportlist = class
  private
    FItems: TObjectList;
    FIsSorted: Boolean;
    function CompareItems(_Idx1, _Idx2: Integer): Integer;
    procedure SwapItems(_Idx1, _Idx2: Integer);
    function CompareToItem(const _Key; _Idx: Integer): Integer;
    function GetItems(_Idx: Integer): TUnitExport;
  public
    constructor Create(_Capacity: Integer);
    destructor Destroy; override;
    function Add(const _Identifier, _Filename: string): Integer;
    procedure ShrinkToCapacity;
    function Count: Integer;
    procedure Sort;
    function SearchStart(const _Start: string; _Found: TList): Boolean;
    function SearchAnywhere(const _s: string; _Found: TList): Boolean;
    property Items[_Idx: Integer]: TUnitExport read GetItems;
  end;

implementation

uses
  StrUtils,
  GX_dzQuicksort;

{ TUnitExport }

constructor TUnitExport.Create(const _Identifier, _Filename: string);
begin
  inherited Create;
  FIdentifier := _Identifier;
  FLCIdentifier := LowerCase(_Identifier);
  FFilename := _Filename;
end;

{ TUnitExportlist }

constructor TUnitExportlist.Create(_Capacity: Integer);
begin
  inherited Create;
  FItems := TObjectList.Create;
  FItems.Capacity := _Capacity;
end;

destructor TUnitExportlist.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

function TUnitExportlist.GetItems(_Idx: Integer): TUnitExport;
begin
  Result := TUnitExport(FItems[_Idx]);
end;

function TUnitExportlist.Add(const _Identifier, _Filename: string): Integer;
begin
  Result := FItems.Add(TUnitExport.Create(_Identifier, _Filename));
  FIsSorted := False;
end;

function TUnitExportlist.Count: Integer;
begin
  Result := FItems.Count;
end;

function TUnitExportlist.SearchAnywhere(const _s: string; _Found: TList): Boolean;
var
  s: string;
  i: Integer;
begin
  s := LowerCase(_s);
  for i := 0 to Count - 1 do begin
    if AnsiContainsStr(Items[i].LCIdentifier, s) then
      _Found.Add(Items[i]);
  end;
  Result := (_Found.Count > 0);
end;

function TUnitExportlist.CompareToItem(const _Key; _Idx: Integer): Integer;
begin
  Result := AnsiCompareStr(string(_Key), Items[_Idx].LCIdentifier);
end;

function TUnitExportlist.SearchStart(const _Start: string; _Found: TList): Boolean;
var
  Idx: Integer;
  s: string;
begin
  s := LowerCase(_Start);
  Result := BinarySearch(0, Count - 1, Idx, s, CompareToItem, True);
  if Result then begin
    while (Idx < Count) and StartsStr(s, Items[Idx].LCIdentifier) do begin
      _Found.Add(FItems[Idx]);
      Inc(Idx);
    end;
  end;
end;

procedure TUnitExportlist.ShrinkToCapacity;
begin
  FItems.Capacity := FItems.Count;
end;

function TUnitExportlist.CompareItems(_Idx1, _Idx2: Integer): Integer;
begin
  Result := AnsiCompareStr(Items[_Idx1].LCIdentifier, Items[_Idx2].LCIdentifier);
  if Result = 0 then
    Result := AnsiCompareStr(Items[_Idx1].FileName, Items[_Idx2].FileName);
end;

procedure TUnitExportlist.SwapItems(_Idx1, _Idx2: Integer);
begin
  FItems.Exchange(_Idx1, _Idx2);
end;

procedure TUnitExportlist.Sort;
begin
  QuickSort(0, Count - 1, CompareItems, SwapItems);
end;

end.
