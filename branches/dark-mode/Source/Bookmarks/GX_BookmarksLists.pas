unit GX_BookmarksLists;

interface { --------------------------------------------------------------------------- }

uses SysUtils,Classes;

type TBookmark     = class
                     private

                      FNumber              : Integer;
                      FLine                : Integer;
                      FColumn              : Integer;
                      FModule              : String;
                      FText                : String;

                     public

                      property Number      : Integer read FNumber;
                      property Line        : Integer read FLine;
                      property Column      : Integer read FColumn;
                      property Module      : String  read FModule;
                      property Text        : String  read FText      write FText;

                      function IsSame      (_BM:TBookmark): Boolean;

                      constructor Create   (_Number,_Line,_Column:Integer;const _Module:String); overload;
                      constructor Create   (_BM:TBookmark);                                      overload;

                     end;

type TBookmarkList = class
                     private

                      function  GetCount   : Integer;
                      function  GetItems   (_Idx:Integer):TBookmark;

                     protected

                      FList                : TList;

                     public

                      property  Items      [_Idx:Integer]:TBookmark  read GetItems; default;
                      property  Count      : Integer                 read GetCount;

                      function  Add        (_Number,_Line,_Column:Integer;const _Module:String=''):Boolean;
                      procedure Clear;

                      constructor Create;
                      destructor  Destroy; override;

                     end;

implementation { ---------------------------------------------------------------------- }

// ............................................................................ TBookmark

function TBookmark.IsSame(_BM:TBookmark):Boolean;
begin
 Result:=(Number=_BM.Number) and (Line=_BM.Line) and (Column=_BM.Column) and (Module=_BM.Module);
end;

constructor TBookmark.Create(_Number,_Line,_Column:Integer;const _Module:String);
begin
 inherited Create;

 FNumber:=_Number;
 FLine  :=_Line;
 FColumn:=_Column;
 FModule:=_Module;
end;

constructor TBookmark.Create(_BM:TBookmark);
begin
 Create(_BM.Number,_BM.Line,_BM.Column,_BM.Module);
end;

// ........................................................................ TBookmarkList

function TBookmarkList.Add(_Number,_Line,_Column:Integer;const _Module:String):Boolean;
var BM    : TBookmark;
    i,Idx : Integer;
begin
 BM :=TBookmark.Create(_Number,_Line,_Column,_Module);
 Idx:=maxInt;

 // list contains items of same module only, so we have to compare number only
 for i:=0 to FList.Count-1 do
  if TBookmark(FList[i]).Number>BM.Number then begin Idx:=i; // don't use a sledgehammer to crack a nut
                                                     Break;  // we have a maximum of 20 items, so this
                                                     end     // code gives a better overview and is operable too!

                                          else if TBookmark(FList[i]).Number=BM.Number then begin Idx:=-1;
                                                                                                  Break;
                                                                                                  end;
 if Idx=-1 then begin BM.Free;
                      Result:=false;
                      end

           else begin if Idx<MaxInt then FList.Insert(Idx,BM)
                                    else FList.Add(BM);
                      Result:=true;
                      end;
end;

function TBookmarkList.GetCount:Integer;
begin
 Result:=FList.Count;
end;

function TBookmarkList.GetItems(_Idx:Integer):TBookmark;
begin
 Result:=FList[_Idx];
end;

procedure TBookmarkList.Clear;
var i : Integer;
begin
 for i:=0 to FList.Count-1 do TBookmark(FList[i]).Free;
 FList.Clear;
end;

// ......................................................................................

constructor TBookmarkList.Create;
begin
 inherited;
 FList:=TList.Create;
end;

destructor TBookmarkList.Destroy;
var i : Integer;
begin
 if Assigned(FList) then begin for i:=0 to FList.Count-1 do TBookmark(FList[i]).Free;
                               FList.Free;
                               FList:=nil;
                               end;
 inherited;
end;

// ......................................................................................
end.
