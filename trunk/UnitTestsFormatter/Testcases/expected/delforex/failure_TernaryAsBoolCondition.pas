unit failure_TernaryAsBoolCondition;

interface

implementation

// Test case: inline if expression used as the boolean condition of a statement if
// The formatter incorrectly treats the inner inline if as a statement if,
// giving it statement-like indentation instead of expression indentation.

procedure TestInlineIfAsBooleanCondition;
var
  A, B, C, D: Integer;
begin
  // 4-level nested inline if used as boolean condition for statement if
  // The formatter incorrectly formats this as nested statement ifs
  if if A > 0 then
         if B > 0 then
             if C > 0 then
                 if D > 0 then
                     True
                   else
                     False
               else
                 False
           else
             False
        else
          False then
    WriteLn('All conditions met');
end;

end.
