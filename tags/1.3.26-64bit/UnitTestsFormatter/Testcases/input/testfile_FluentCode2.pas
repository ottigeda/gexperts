unit testfile_FluentCode2;

interface

implementation

procedure bla;
begin
  SomeFunc([1, 2, 3])
    .SomeMethod(blub)
    .SomeOtherMethod(a, b, 5);
end;

end.