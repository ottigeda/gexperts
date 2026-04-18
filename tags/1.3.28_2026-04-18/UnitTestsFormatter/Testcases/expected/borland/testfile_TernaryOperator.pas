unit TestTernaryOperator;

interface

const
  // Constant ternary expressions (compile-time evaluated)
  MAX_SIZE = if True then 100 else 50;
  MIN_SIZE = if False then 10 else 5;

  // Nested constant ternary
  BUFFER_SIZE = if True then (if True then 1024 else 512) else 256;

  // String constants
  DEFAULT_NAME = if True then 'Default' else 'Unknown';

implementation

procedure Test;
var
  X, Y, Z: Integer;
  S: string;
  B: Boolean;
begin
  // Simple ternary operator
  X := if Y > 0 then 10 else 20;

  // With proper spacing (what we want)
  X := if Y > 0 then 10 else 20;

  // In function call
  ShowMessage(if Y > 0 then 'Positive' else 'Not positive');

  // Multiple parameters with ternary
  WriteLn(if Y > 0 then 'Y is positive' else 'Y is not positive',
    if Z > 0 then 'Z is positive' else 'Z is not positive');

  // Nested ternary (without extra parentheses)
  X := if Y > 0 then if Y > 10 then 100 else 50 else 0;

  // Nested ternary (with parentheses for clarity)
  X := if Y > 0 then (if Y > 10 then 100 else 50) else 0;

  // Triple nested
  X := if Y > 0 then (if Y > 10 then (if Y > 100 then 1000 else 500) else
    50) else 0;

  // Nested in else branch
  X := if Y < 0 then -1 else (if Y > 0 then 1 else 0);

  // Longer expression
  S := if (Y > 0) and (Y < 100) then 'InRange' else 'OutOfRange';

  // Boolean result
  B := if Y > 0 then True else False;

  // Ternary in array index
  X := IntArray[if B then 1 else 2];

  // Ternary with method calls
  X := if Obj.IsValid then Obj.GetValue else Obj.GetDefaultValue;

  // Ternary in case statement value
  case if Y > 0 then Y else -Y of
    1: X := 1;
    2: X := 2;
  end;

  // Multiple ternaries in same expression
  X := (if Y > 0 then 10 else 5) + (if Z > 0 then 20 else 15);

  // Ternary with type cast
  X := Integer(if B then PtrValue1 else PtrValue2);

  // Ternary in comparison
  if (if Y > 0 then Y else -Y) > 10 then
    ShowMessage('Large');

  // Ternary in for loop limit
  for X := 1 to if Y > 0 then Y else 10 do
    WriteLn(X);

  // Ternary with set operations
  MySet := if B then [opt1, opt2] else [opt3];
end;

end.
