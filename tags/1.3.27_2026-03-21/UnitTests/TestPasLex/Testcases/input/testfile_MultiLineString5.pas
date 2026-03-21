unit MultiLineString5;

interface

procedure Before;
procedure After;

implementation

procedure Before;
begin
end;

procedure After;
var
  S: string;
begin
  S := '''''
    content with '' embedded quotes
    and (* parens *) too
    ''''';
end;

end.
