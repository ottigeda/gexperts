unit testfile_AlignAssign;

interface

implementation

procedure TestBasicAssignment;
var
  I: Integer;
  LongVariableName: string;
  X: Double;
begin
  I                           := 0;
  LongVariableName            := 'test';
  X                           := 1.5;
end;

procedure TestForLoop;
var
  I: Integer;
  Sum: Integer;
begin
  Sum                         := 0;
  for I := 0 to 10 do
    Sum                       := Sum + I;
end;

procedure TestForVarLoop;
var
  Sum: Integer;
begin
  Sum                         := 0;
  for var I := 0 to 10 do
    Sum                       := Sum + I;
end;

procedure TestForVarTypedLoop;
var
  Sum: Integer;
begin
  Sum                         := 0;
  for var I: Integer := 0 to 10 do
    Sum                       := Sum + I;
end;

procedure TestMixedStatements;
var
  A: Integer;
  B: Integer;
  C: Integer;
begin
  A                           := 1;
  B                           := A + 2;
  WriteLn(A);
  C                           := B * 3;
end;

procedure TestNestedBlocks;
var
  X: Integer;
  Y: Integer;
begin
  X                           := 10;
  if X > 5 then
  begin
    Y                         := X * 2;
    X                         := Y + 1;
  end;
end;

procedure TestLongLHS;
var
  VeryLongVariableNameThatExceedsAlignPos: Integer;
  S: string;
begin
  VeryLongVariableNameThatExceedsAlignPos := 42;
  S                           := 'short';
end;

procedure TestForWithComment;
var
  I: Integer;
begin
  // comment before for
  for I := 0 to 5 do
    WriteLn(I);
end;

end.
