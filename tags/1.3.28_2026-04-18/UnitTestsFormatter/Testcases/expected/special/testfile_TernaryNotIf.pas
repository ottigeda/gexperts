unit failure_TernaryNotIf;

interface

implementation

// Test case: unary 'not' operator before inline if expression
// The formatter incorrectly handles this case - the inline if is not
// recognized as an expression and gets statement-like formatting.

procedure TestNotBeforeInlineIf;
var
  B: Boolean;
  Y: Integer;
begin
  // Inline if with not - formatter fails to format this correctly
  B := not if  Y > 0
             then True
             else False;
end;

end.
