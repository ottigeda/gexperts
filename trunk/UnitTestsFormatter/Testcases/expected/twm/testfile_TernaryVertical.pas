unit TestTernaryVertical;

interface

const
  // Constant with vertical ternary (bad indentation - should be fixed)
  MAX_VALUE = if True
                then 100
                else 50;

  // Constant with value on separate line
  MIN_VALUE = if False
                then
                  20
                else
                  10;

implementation

procedure TestBasicVertical;
var
  X, Y: Integer;
  S: string;
begin
  // Basic vertical ternary with bad indentation
  X := if Y > 0
         then 10
         else 20;

  // Value on separate line (inconsistent indentation)
  X := if Y > 0
         then
           5
         else
           7;

  // Mixed: some values on same line, some separate
  X := if Y > 0
         then 15
         else
           25;

  // Another mixed case
  X := if Y > 0
         then
           100
         else 200;

  // One-line ternary (should remain unchanged)
  X := if Y > 0 then 10 else 20;

  // Vertical in function call (bad indentation)
  ShowMessage(if Y > 0
                then 'Positive'
                else 'Not positive');

  // String with value on separate line
  S := if Y > 0
         then
           'Yes'
         else
           'No';
end;

procedure TestNested;
var
  X, Y: Integer;
begin
  // Nested vertical ternary (bad indentation)
  X := if Y > 0
         then if Y > 10
                then 100
                else 50
         else 0;

  // Nested with some values on separate lines
  X := if Y > 0
         then if Y > 10
                then
                  1000
                else
                  500
         else 0;

  // Triple nested (very bad indentation)
  X := if Y > 0
         then if Y > 10
                then if Y > 100
                       then 10000
                       else 1000
                else 100
         else 0;

  // Nested with parentheses - inner stays on one line
  X := if Y > 0
         then (if Y > 10 then 100 else 50)
         else 0;

  // Nested in else branch
  X := if Y < 0
         then -1
         else if Y > 0
                then 1
                else 0;
end;

procedure TestDifferentContexts;
var
  X, Y, Z: Integer;
  IntArray: array[0..10] of Integer;
  B: Boolean;
begin
  // In array index
  X := IntArray[if B
                  then 1
                  else 2];

  // In for loop limit
  for X := 1 to if Y > 0
                  then Y
                  else 10 do
    WriteLn(X);

  // In case statement value
  case if Y > 0
         then Y
         else -Y of
    1: X := 1;
    2: X := 2;
  end;

  // Multiple parameters
  WriteLn(if Y > 0
            then 'Y positive'
            else 'Y not positive',
    if Z > 0
      then 'Z positive'
      else 'Z not positive');
end;

procedure TestIndentationLevels;
var
  X, Y: Integer;
begin
  // Inside begin/end block
  if Y > 5 then begin
    X := if Y > 10
           then 100
           else 50;
  end;

  // Inside while loop
  while Y > 0 do begin
    X := if Y > 10
           then Y
           else 5;
    Dec(Y);
  end;

  // Deeply nested in code
  if Y > 0 then begin
    if Y > 10 then begin
      X := if Y > 100
             then 1000
             else 500;
    end;
  end;
end;

procedure TestLongConditions;
var
  X, Y, Z: Integer;
  Condition1, Condition2: Boolean;
begin
  // Long condition (already broken across lines)
  X := if (Y > 0) and (Z > 0) and Condition1
         then 100
         else 0;

  // Very long condition with multiple parts
  X := if (Y > 0) and (Z > 0) and
    Condition1 and Condition2
         then 500
         else 0;

  // Long values
  X := if Y > 0
         then (Y * 10 + Z * 20)
         else (Y * 5 + Z * 10);
end;

procedure TestEdgeCases;
var
  X, Y: Integer;
begin
  // Then and else at start of line (column 1)
  X := if Y > 0
         then 10
         else 20;

  // Extra spaces before keywords (should be normalized)
  X := if Y > 0
         then 10
         else 20;

  // Comments between keywords
  X := if Y > 0
    // This is the true case
         then 10
  // This is the false case
         else 20;

  // Nested with mixed one-line and multi-line
  X := if Y > 0 then (if Y > 10
                        then 100
                        else 50) else 0;
end;

end.
