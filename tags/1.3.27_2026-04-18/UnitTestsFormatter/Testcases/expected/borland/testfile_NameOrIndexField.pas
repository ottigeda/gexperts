unit test;

interface

type
  TTest = class
  public
    Field: Integer;
    Name, Value: string;
    Index: Integer;
  public
    property bla: integer index 0 read GetBla;
  end;

implementation

end.