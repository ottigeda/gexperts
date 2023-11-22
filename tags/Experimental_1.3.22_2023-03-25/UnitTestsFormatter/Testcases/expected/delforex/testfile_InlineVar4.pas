unit testfile_InlineVar4;

interface

implementation

procedure bla;
begin
  SomeProc;
  var I: Integer;
  I := 22;
  ShowMessage(I.ToString);
end;

end.