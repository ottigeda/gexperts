unit TestTernaryDeepNested;

interface

implementation

{ Test cases for deeply nested inline if expressions (4 and 5 levels) }

procedure TestFourLevelNesting;
var
  X, A, B, C, D: Integer;
begin
  // 4-level nesting in then branches
  X := if A > 0
         then if B > 0
                then if C > 0
                       then if D > 0
                              then 1000
                              else 100
                       else 10
                else 1
         else 0;

  // 4-level nesting with mixed branches
  X := if A > 0
         then if B > 0
                then 50
                else if C > 0
                       then if D > 0
                              then 40
                              else 30
                       else 20
         else 10;

  // 4-level nesting in else branches
  X := if A > 0
         then 1000
         else if B > 0
                then 100
                else if C > 0
                       then 10
                       else if D > 0
                              then 1
                              else 0;

  // 4-level nesting with values on separate lines
  X := if A > 0
         then if B > 0
                then if C > 0
                       then if D > 0
                              then
                                1000
                              else
                                100
                       else
                         10
                else
                  1
         else
           0;

  // 4-level nesting alternating then/else
  X := if A > 0
         then if B > 0
                then 100
                else if C > 0
                       then 50
                       else if D > 0
                              then 25
                              else 10
         else 0;
end;

procedure TestFiveLevelNesting;
var
  X, A, B, C, D, E: Integer;
begin
  // 5-level nesting in then branches
  X := if A > 0
         then if B > 0
                then if C > 0
                       then if D > 0
                              then if E > 0
                                     then 10000
                                     else 1000
                              else 100
                       else 10
                else 1
         else 0;

  // 5-level nesting with mixed branches
  X := if A > 0
         then if B > 0
                then if C > 0
                       then 500
                       else if D > 0
                              then if E > 0
                                     then 400
                                     else 300
                              else 200
                else 100
         else 50;

  // 5-level nesting in else branches (else-if chain)
  X := if A > 0
         then 10000
         else if B > 0
                then 1000
                else if C > 0
                       then 100
                       else if D > 0
                              then 10
                              else if E > 0
                                     then 1
                                     else 0;

  // 5-level nesting with values on separate lines
  X := if A > 0
         then if B > 0
                then if C > 0
                       then if D > 0
                              then if E > 0
                                     then
                                       10000
                                     else
                                       1000
                              else
                                100
                       else
                         10
                else
                  1
         else
           0;

  // 5-level nesting with complex alternating pattern
  X := if A > 0
         then if B > 0
                then 1000
                else if C > 0
                       then if D > 0
                              then 500
                              else if E > 0
                                     then 250
                                     else 125
                       else 100
         else 50;
end;

procedure TestDeepNestingWithParentheses;
var
  X, A, B, C, D, E: Integer;
begin
  // 4-level with parentheses around inner expressions
  X := if A > 0
         then (if B > 0
                 then (if C > 0
                         then (if D > 0 then 1000 else 100)
                         else 10)
                 else 1)
         else 0;

  // 5-level with mixed parentheses
  X := if A > 0
         then (if B > 0
                 then if C > 0
                        then (if D > 0
                                then (if E > 0 then 10000 else 1000)
                                else 100)
                        else 10
                 else 1)
         else 0;

  // Some branches on one line, others multi-line
  X := if A > 0
         then (if B > 0 then (if C > 0 then 100 else 50) else 25)
         else if D > 0
                then if E > 0
                       then 10
                       else 5
                else 0;
end;

procedure TestDeepNestingInExpressions;
var
  X, A, B, C, D, E: Integer;
begin
  // 4-level nesting as part of larger expression
  X := 10 + if A > 0
              then if B > 0
                     then if C > 0
                            then if D > 0
                                   then 1000
                                   else 100
                            else 10
                     else 1
              else 0 + 5;

  // 5-level nesting in function call
  WriteLn(if A > 0
            then if B > 0
                   then if C > 0
                          then if D > 0
                                 then if E > 0
                                        then 'ABCDE'
                                        else 'ABCD'
                                 else 'ABC'
                          else 'AB'
                   else 'A'
            else 'None');

  // Multiple 4-level nesting in one expression
  X := (if A > 0
          then if B > 0
                 then if C > 0
                        then if D > 0
                               then 1000
                               else 100
                        else 10
                 else 1
          else 0) * (if A > 0
                       then if B > 0
                              then if C > 0
                                     then if D > 0
                                            then 2
                                            else 3
                                     else 4
                              else 5
                       else 6);
end;

procedure TestDeepNestingWithComments;
var
  X, A, B, C, D, E: Integer;
begin
  // 4-level with comments
  X := if A > 0 // Check A
         then if B > 0 // Check B
                then if C > 0 // Check C
                       then if D > 0 // Check D
                              then 1000 // All positive
                              else 100 // D not positive
                       else 10 // C not positive
                else 1 // B not positive
         else 0; // A not positive

  // 5-level with inline comments
  X := if A > 0
         then if B > 0
                then if C > 0
                       then if D > 0
                              then if E > 0
                                     then 10000 // Deepest then
                                     else 1000 // E else
                              else 100 // D else
                       else 10 // C else
                else 1 // B else
         else 0; // A else
end;

procedure TestDeepNestingEdgeCases;
var
  X, A, B, C, D, E: Integer;
begin
  // 4-level nesting starting at column 0
  X := if A > 0
         then if B > 0
                then if C > 0
                       then if D > 0
                              then 1000
                              else 100
                       else 10
                else 1
         else 0;

  // 5-level nesting inside deeply nested code blocks
  if A > 0 then begin
    if B > 0 then begin
      X := if C > 0
             then if D > 0
                    then if E > 0
                           then if A + B > 0
                                  then if C + D > 0
                                         then 99999
                                         else 9999
                                  else 999
                           else 99
                    else 9
             else 0;
    end;
  end;

  // Note: "if if A > 0..." boolean condition case moved to failure_TernaryAsBoolCondition.pas
end;

procedure TestDeepNestingReturnValues;
var
  A, B, C, D, E: Integer;

  function Get4Level: Integer;
  begin
    Result := if A > 0
                then if B > 0
                       then if C > 0
                              then if D > 0
                                     then 1000
                                     else 100
                              else 10
                       else 1
                else 0;
  end;

  function Get5Level: Integer;
  begin
    Result := if A > 0
                then if B > 0
                       then if C > 0
                              then if D > 0
                                     then if E > 0
                                            then 10000
                                            else 1000
                                     else 100
                              else 10
                       else 1
                else 0;
  end;

begin
  A := Get4Level;
  B := Get5Level;
end;

procedure TestDeepNestingSymmetric;
var
  X, A, B, C, D, E: Integer;
begin
  // Symmetric 4-level: 2 in then, 2 in else
  X := if A > 0
         then if B > 0
                then 100
                else 50
         else if C > 0
                then 25
                else 10;

  // Symmetric 5-level variation
  X := if A > 0
         then if B > 0
                then if C > 0
                       then 1000
                       else 500
                else 250
         else if D > 0
                then if E > 0
                       then 125
                       else 60
                else 30;

  // Diamond pattern: nest in both then and else to same depth
  X := if A > 0
         then if B > 0
                then if C > 0
                       then 100
                       else 75
                else if C > 0
                       then 50
                       else 25
         else if B > 0
                then if C > 0
                       then 20
                       else 15
                else if C > 0
                       then 10
                       else 5;
end;

end.
