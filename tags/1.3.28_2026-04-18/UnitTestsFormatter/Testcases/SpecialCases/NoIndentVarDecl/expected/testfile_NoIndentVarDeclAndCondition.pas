unit testfile_NoIndentVarDeclAndCondition;
// Bug #468: "Never indent var declaration" should not affect "and" in if conditions

interface

procedure Test;

implementation

procedure Test;
var
  FValue: Integer;
  I: Integer;
begin
  // This "and" line should be indented regardless of NoIndentVarDecl setting
  if (FValue > 100)
    and (FValue < 200) then
    I := 1;

  // Same for "or"
  if (FValue > 100)
    or (FValue < 200) then
    I := 2;
end;

end.
