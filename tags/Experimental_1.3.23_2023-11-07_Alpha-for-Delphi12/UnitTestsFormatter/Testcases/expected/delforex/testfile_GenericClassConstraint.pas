unit testfile_GenericClass3;

interface

type
  TListOfList<T: TData; U: TDataList<T>> = class
    procedure foo; overload; virtual;
    procedure foo(i: Integer); overload virtual;
  end;

implementation

end.