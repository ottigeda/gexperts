unit testfile_ClassFieldsWithComma;

interface

type
  TClass1 = class
    a,
    b,
    c: integer;
  end;

type
  TClass2 = class
  private
    a,
    b,
    c: integer;
  end;

implementation

end.