unit testfile_Attributes;

interface

type
  TSomeClass = class
    [someAttribute]
    [anotherAttribute]
    procedure bla;
  end;
  
implementation

procedure TSomeClass.bla;
begin
end;

end.