unit testfile_InlineVarInTryFinally;

interface

implementation

procedure bla;
begin
  try
    var a: integer;
    a := 2;
    test(a);
  finally
  end;
end;

end.