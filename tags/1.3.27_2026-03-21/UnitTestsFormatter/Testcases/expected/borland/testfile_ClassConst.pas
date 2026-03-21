unit testfile_ClassConst;

interface

type
  TMyClass = class
  strict private
    class const
      SomeTypedConst: Integer = clRed;
      SomeIntegerConst = 1;
      SomeText = 'bla';
  end;

implementation

end.
