unit testfile_RepeatUntilMultiline;

interface

implementation

procedure TestRepeatUntilMultiline;
begin
  repeat
  until (AAA) or
    (BBB) or
    (CCC);

  repeat
    DoSomething;
  until (AAA) or
    (BBB) or
    (CCC);

  // Compare with if statement
  if (AAA) or
    (BBB) or
    (CCC) then
    DoSomething;

  // Compare with while statement
  while (AAA) or
    (BBB) or
    (CCC) do
    DoSomething;

  // Without parens around each sub-expression
  repeat
  until AAA or
    BBB or
    CCC;

  // Four line condition
  repeat
  until (AAA) or
    (BBB) or
    (CCC) or
    (DDD);
end;

end.
