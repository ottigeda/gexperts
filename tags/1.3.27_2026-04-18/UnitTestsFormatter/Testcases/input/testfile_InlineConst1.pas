unit testfile_InlineConst1;

interface

implementation

procedure bla;
begin
  const M: Integer = (L + H) div 2;
  ShowMessage (M.ToString);
end;

end.