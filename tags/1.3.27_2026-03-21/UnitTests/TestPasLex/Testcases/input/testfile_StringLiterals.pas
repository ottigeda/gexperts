unit StringLiterals;

interface

implementation

procedure Test;
var
  S: string;
begin
  S := 'hello';
  S := 'it''s';
  S := '';
  S := #65;
  S := #13#10'text';
  S := 'line1'#13#10'line2';
end;

end.
