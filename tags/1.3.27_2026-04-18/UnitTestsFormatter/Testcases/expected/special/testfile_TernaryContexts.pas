unit TestTernaryContexts;

interface

type
  TMyRecord = record
    Value: Integer;
    Name: string;
  end;

  TMyClass = class
  private
    FValue: Integer;
    function GetComputed: Integer;
  public
    property Computed: Integer read GetComputed;
    procedure DoSomething(AParam: Integer);
  end;

implementation

{ Additional contexts for inline if expressions }

procedure TestReturnValue;
var
  X: Integer;

  function GetValue(Condition: Boolean): Integer;
  begin
    // Inline if as return value
    Result := if  Condition
                then 100
                else 0;
  end;

  function GetValueNested(A, B: Boolean): Integer;
  begin
    // Nested inline if as return value
    Result := if  A
                then if  B
                       then 1
                       else 2
                else 3;
  end;

begin
  X := GetValue(True);
  X := GetValueNested(True, False);
end;

procedure TestArithmeticExpressions;
var
  X, Y, Z: Integer;
begin
  // Inline if in addition
  X := 10 + if  Y > 0
              then 5
              else 0;

  // Inline if in multiplication
  X := (if  Y > 0
          then 2
          else 1) * Z;

  // Multiple inline ifs in expression
  X := (if  Y > 0
          then 10
          else 5) + (if  Z > 0
                       then 20
                       else 15);

  // Inline if with unary minus
  X := -if  Y > 0
           then 10
           else 20;

  // Complex arithmetic with inline if
  X := ((if  Y > 0
           then Y
           else 0) * 10) div (if  Z > 0
                                then Z
                                else 1);
end;

procedure TestStringOperations;
var
  S: string;
  Y: Integer;
begin
  // Inline if in string concatenation
  S := 'Result: ' + if  Y > 0
                      then 'Positive'
                      else 'Non-positive';

  // Multiple concatenations with inline if
  S := 'X=' + if  Y > 0
                then 'Yes'
                else 'No' + ', Done';

  // Inline if with Format
  S := Format('Value: %s', [if  Y > 0
                               then 'Good'
                               else 'Bad']);
end;

procedure TestRepeatUntil;
var
  X, Y: Integer;
begin
  // Inline if in repeat-until condition
  X := 0;
  repeat
    Inc(X);
  until X > if  Y > 0
              then Y
              else 10;
end;

procedure TestWhileCondition;
var
  X, Y: Integer;
begin
  // Inline if in while condition
  X := 0;
  while X < if  Y > 0
              then Y
              else 10 do
    Inc(X);
end;

procedure TestWithStatement;
var
  Rec: TMyRecord;
  Y: Integer;
begin
  // Inline if inside with statement
  with Rec do begin
    Value := if  Y > 0
               then 100
               else 50;
    Name := if  Y > 0
              then 'High'
              else 'Low';
  end;
end;

procedure TestTryExcept;
var
  X, Y: Integer;
begin
  // Inline if in try block
  try
    X := if  Y > 0
           then 100 div Y
           else 0;
  except
    X := if  Y < 0
           then -1
           else 0;
  end;
end;

procedure TestTryFinally;
var
  X, Y: Integer;
begin
  // Inline if in try-finally
  try
    X := if  Y > 0
           then Y * 2
           else 0;
  finally
    X := if  X > 0
           then X
           else -1;
  end;
end;

procedure TestRaiseAndAssert;
var
  Y: Integer;
begin
  // Inline if in Assert
  Assert(Y > if  True
               then 0
               else -1, 'Value must be positive');

  // Inline if in raise (conditional message)
  if  Y < 0 then
    raise Exception.Create(if  Y = -1
                               then 'Invalid: minus one'
                               else 'Invalid: negative');
end;

procedure TestMethodChaining;
var
  List: TStringList;
  Y: Integer;
begin
  List := TStringList.Create;
  try
    // Inline if as parameter to method
    List.Add(if  Y > 0
                 then 'Positive'
                 else 'Negative');

    // Inline if for index
    List.Delete(if  Y > 0
                    then 0
                    else List.Count - 1);

    // Multiple inline ifs in chained calls
    List.Insert(if  Y > 0
                    then 0
                    else 1, if  Y > 0
                               then 'First'
                               else 'Second');
  finally
    List.Free;
  end;
end;

function TMyClass.GetComputed: Integer;
begin
  // Inline if in property getter
  Result := if  FValue > 0
              then FValue * 2
              else 0;
end;

procedure TMyClass.DoSomething(AParam: Integer);
begin
  // Inline if assigning to field
  FValue := if  AParam > 0
              then AParam
              else -AParam;
end;

procedure TestRecordInitialization;
var
  Rec: TMyRecord;
  Y: Integer;
begin
  // Inline if in record field assignment
  Rec.Value := if  Y > 0
                   then Y
                   else 0;
  Rec.Name := if  Y > 0
                  then 'Valid'
                  else 'Invalid';
end;

procedure TestArrayOperations;
var
  Arr: array[0..9] of Integer;
  DynArr: array of string;
  I, Y: Integer;
begin
  // Inline if for array element assignment
  for I := 0 to 9 do
    Arr[I] := if  I mod 2 = 0
                then I * 2
                else I;

  // Inline if for dynamic array length
  SetLength(DynArr, if  Y > 0
                       then Y
                       else 10);

  // Inline if in array element access and assignment
  Arr[if  Y > 0
        then 0
        else 9] := if  Y > 0
                     then 100
                     else 0;
end;

procedure TestBooleanExpressions;
var
  B1, B2: Boolean;
  Y: Integer;
begin
  // Inline if returning boolean
  B1 := if  Y > 0
          then True
          else False;

  // Inline if in boolean expression
  B2 := (Y > 0) and (if  Y > 10
                       then True
                       else False);

  // Note: "not if" case moved to failure_TernaryNotIf.pas
end;

procedure TestTypeConversions;
var
  I: Integer;
  S: string;
  D: Double;
  Y: Integer;
begin
  // Inline if with IntToStr
  S := IntToStr(if  Y > 0
                  then Y
                  else 0);

  // Inline if with StrToInt
  I := StrToInt(if  Y > 0
                  then '100'
                  else '0');

  // Inline if with floating point
  D := if  Y > 0
         then 3.14
         else 0.0;
end;

procedure TestPointerOperations;
var
  P: PInteger;
  X, Y: Integer;
begin
  // Inline if with pointer dereference
  P := @X;
  P^ := if  Y > 0
           then 100
           else 0;

  // Inline if with nil check (conceptual)
  X := if  Assigned(P)
         then P^
         else -1;
end;

procedure TestSetOperations;
type
  TMySet = set of 0..31;
var
  S: TMySet;
  Y: Integer;
begin
  // Inline if with set assignment
  S := if  Y > 0
         then [0, 1, 2]
         else [];

  // Note: "if Y in if..." case moved to failure_TernaryInCondition.pas
end;

procedure TestEnumOperations;
type
  TStatus = (stNone, stActive, stInactive);
var
  Status: TStatus;
  Y: Integer;
begin
  // Inline if with enum
  Status := if  Y > 0
              then stActive
              else stInactive;

  // Note: "if Status = if..." case moved to failure_TernaryInCondition.pas
end;

procedure TestInheritedCall;
begin
  // This would be in a derived class method
  // inherited DoSomething(if Condition then 1 else 2);
end;

procedure TestExitWithValue;
var
  Y: Integer;

  function TestFunc: Integer;
  begin
    // Inline if with Exit
    if  Y < 0 then
      Exit(if  Y = -1
             then -100
             else -200);
    Result := 0;
  end;

begin
  Y := TestFunc;
end;

end.
