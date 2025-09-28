unit testfile_Anonymous;

interface

implementation

function SomeProc: boolean;
var
  i: integer;
begin
  RegisterConverter(procedure(bla: integer; var blub: string)
    var
      i: integer;
    begin
    end);
end;
