unit failure_TernaryAsBoolCondition;

interface

implementation

// Test case: inline if expression used as the boolean condition of a statement if
// The formatter correctly recognizes the inner inline if as an expression,
// applying inline if indentation (relative to the if keyword) rather than
// statement indentation.

procedure TestInlineIfAsBooleanCondition;
var
  A, B, C, D: Integer;
begin
  // 4-level nested inline if used as boolean condition for statement if
  // The formatter correctly applies inline if indentation
  if if A > 0
then if B > 0
then if C > 0
then if D > 0
then True
else False
else False
else False
else False then
    WriteLn('All conditions met');
end;

end.
