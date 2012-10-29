// Some changed TLists (more compatible with Borland Pascal 7)
// Original Author:     Egbert van Nes (http://www.dow.wau.nl/aew/People/Egbert_van_Nes.html)
// Contributors:        Thomas Mueller (http://www.dummzeuch.de)
unit GX_PascalTokenList;

{$I GX_CondDefine.inc}

interface

uses
  Classes;

const
  MaxCollectionSize = Maxint div (SizeOf(Integer) * 2);

type
  TPascalTokenList = class(TList)
  public
    constructor Create(ACapacity: Integer);
    procedure AtFree(Index: Integer);
    procedure FreeAll;
    procedure DoFree(Item: Pointer);
    procedure FreeItem(Item: Pointer); virtual;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;

constructor TPascalTokenList.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  {Delta is automatic in TList}
end;

destructor TPascalTokenList.Destroy;
begin
  FreeAll;
  inherited Destroy;
end;

procedure TPascalTokenList.AtFree(Index: Integer);
var
  Item: Pointer;
begin
  Item := Items[Index];
  Delete(Index);
  FreeItem(Item);
end;

procedure TPascalTokenList.FreeAll;
var
  I: Integer;
begin
  try
    for I := 0 to Count - 1 do
      FreeItem(Items[I]);
  finally
    Count := 0;
  end;
end;

procedure TPascalTokenList.DoFree(Item: Pointer);
begin
  AtFree(IndexOf(Item));
end;

procedure TPascalTokenList.FreeItem(Item: Pointer);
begin
  if (Item <> nil) then
    with TObject(Item) as TObject do
      Free;
end;

end.

