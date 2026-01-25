unit testfile_DoubleIndentIfCondition;

interface

implementation

procedure TestSimpleIf;
begin
  if Condition1 then
    doSomething;
end;

procedure TestMultiLineCondition;
begin
  if Condition1
    and Condition2 then
  begin
    doSomething;
  end;
end;

procedure TestThreeConditions;
begin
  if Condition1
    and Condition2
    or Condition3 then
  begin
    doSomething;
  end;
end;

procedure TestNestedIf;
begin
  if Condition1
    and Condition2 then
  begin
    if InnerCondition1
      and InnerCondition2 then
    begin
      doSomethingInner;
    end;
  end;
end;

procedure TestIfElse;
begin
  if Condition1
    and Condition2 then
  begin
    doSomething;
  end
  else
  begin
    doSomethingElse;
  end;
end;

procedure TestWhileCondition;
begin
  while Condition1
    and Condition2 do
  begin
    doSomething;
  end;
end;

end.
