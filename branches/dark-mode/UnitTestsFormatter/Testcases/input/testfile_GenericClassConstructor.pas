unit testfile_GenericClassConstructor;

interface

implementation

procedure Test;
var
  dict: TDictionary<string, Integer>;
begin
  dict := TDictionary<string, Integer>.Create;
end;

end.