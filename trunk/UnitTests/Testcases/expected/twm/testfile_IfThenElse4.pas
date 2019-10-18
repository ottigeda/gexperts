unit testfile_IfthenElse4;

interface

implementation

procedure bla;
begin
  if IsXmlWhiteSpace(S[i]) then begin // White space?
    text.addWideChar(SPACE)
  end else if S[i] = '&' then
    kindOfToken := IS_REFSTART
  else
    text.addWideChar(S[i]);
end;

end.
