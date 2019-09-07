unit testfile_Anonymous;

interface

implementation

procedure SomeProc;
var
  s: string;
begin
  RegisterConverter(TStringList,
    function: integer
    var b: boolean;
    begin
      bla;
      blub;
    end);
end;