unit testfile_Anonymous;

interface

implementation

function SomeProc: boolean;
var
  i: integer;
begin
  RegisterConverter(procedure
    var
      i: integer;
    begin
    end);
end;
