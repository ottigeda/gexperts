unit testfile_GenericMethodCall;

interface

implementation

procedure Test;
begin
  TJSON.StringToObject<TJSONObject>('{"bla":"bla"}');
  TJSON.StringToObject<System.TJSONObject>('{"bla":"bla"}');
  if  1 < 2 then
    ;
end;

end.
