unit failure_TernaryInCondition;

interface

implementation

// Test cases: inline if expression used within a statement if's condition
// The formatter incorrectly aligns 'else' with one extra space compared to 'then'.

procedure TestInlineIfInSetCondition;
var
  Y: Integer;
begin
  // Inline if in set inclusion check - else is misaligned
  if Y in if True
then [0..10]
else [11..20] then
    WriteLn('In range');
end;

procedure TestInlineIfInEqualityCondition;
type
  TStatus = (stNone, stActive, stInactive);
var
  Status: TStatus;
  Y: Integer;
begin
  // Inline if comparing enums - else is misaligned
  if Status = if Y > 0
then stActive
else stNone then
    WriteLn('Match');
end;

end.
