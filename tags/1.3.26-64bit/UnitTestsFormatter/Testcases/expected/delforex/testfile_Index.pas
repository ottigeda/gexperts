unit testfile_Index;

interface

type
  TFileResult = class
  private
    Module: string;
    Index: Integer;
    MatchStart: Integer;
    MatchLen: Integer;
    function GetIndexedProp(_Idx: integer): integer;
  public
    constructor Create(const _Module: string; _Index, _Start, _Len: Integer);
    property IndexedProp: integer index 1 read GetIndexedProp;
  end;

implementation

function blablub: integer; stdcall; external 'blablub.dll' index 5;

constructor TFileResult.Create(const _Module: string; _Index, _Start, _Len: Integer);
begin
end;

end.