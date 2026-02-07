unit testfile_GenericsProperty;

interface

type
  TBla = class
    property OneDict: TDictionary<string, Boolean> read GetOneDict;
  end;

implementation

end.
