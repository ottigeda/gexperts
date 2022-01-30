unit testfile_MultilineFunctionDirective1;

interface

type
  TBlub = class
    procedure bla;
      overload;
      forward;
  end;

implementation

end.