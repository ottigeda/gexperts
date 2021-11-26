unit testfile_Attributes2;

interface

const
  a = 5;
  b = 2;

type
  [AClassAtrribute]
  TSomeClass = class
  private
    [FieldAtrribute]
    FSomeField: Integer;
  public
    [someAttribute(a+b)]
    [anotherAttribute(1, 2)]
    procedure bla;
    [PropertyAttribute(SomeArgument)]
    property SomeProperty: Integer read FSomeField write FSomeField;
  end;

interface

procedure TSomeClass.bla;
begin
end;

end.