unit testfile_GenericNested;

interface

implementation

procedure Test;
begin
  // Nested generics in method calls (bug #219)
  TJSON.JsonToObject<TJSONReport<TPrintData>>(self.FPlaceholderData);
  TJSON.JsonToObject<TJSONReport<TPrintData>>(FPlaceholderData);

  // Nested generics with qualified type names
  TJSON.JsonToObject<TJSONReport<System.TPrintData>>(FData);

  // Double-nested generics
  Foo<TBar<TBaz<TQux>>>(x);

  // Simple generics should still work
  TJSON.StringToObject<TJSONObject>('{"bla":"bla"}');

  // Comparison operators should still get spaces
  if 1 < 2 then
    ;
  if a > b then
    ;
end;

end.
