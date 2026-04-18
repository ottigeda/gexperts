unit testfile_InlineConst2;

interface

implementation

procedure bla;
begin
  const M = (L + H) div 2;
  ShowMessage (M.ToString);
end;

end.