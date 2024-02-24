unit testfile_InlineVar3;

interface

implementation

procedure bla;
begin
  SomeProc;
  var I := 22;
  ShowMessage(I.ToString);
end;

end.