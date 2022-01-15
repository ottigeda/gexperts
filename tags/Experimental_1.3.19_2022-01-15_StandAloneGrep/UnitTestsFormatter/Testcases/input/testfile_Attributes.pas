unit testfile_Attributes;

interface

type
  TSomeClass = class
    [someAttribute]
    [anotherAttribute]
    procedure bla;
  end;
  
interface

procedure TSomeClass.bla;
begin
end;

end.