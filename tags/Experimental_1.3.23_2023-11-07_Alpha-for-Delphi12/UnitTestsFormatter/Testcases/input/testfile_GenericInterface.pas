unit testfile_GenericInterface;

interface

type
  TGenericArray<T> = array of T;

type
  IMyData<T> = interface
    function Add(const aExistingSet: TGenericArray<T>;
      const aNewValue: T): TGenericArray<T>;
  end;

implementation

end.