unit testfile_ClassFieldsWithComma;

interface

type
  TClass1 = class
    a,
    b, // bla
    c,
    d: integer;
  end;

type
  TClass2 = class
  private
    a,
    b, // bla
    c,
    d: integer;
  end;

implementation

end.