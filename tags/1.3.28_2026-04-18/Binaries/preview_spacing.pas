unit PreviewSpacing;
{ Demonstrates spacing around operators }

interface

type
  TDemo = class
  private
    FValue: Integer;
  public
    procedure Calculate(A, B: Integer);
    function GetItem(Index: Integer): string;
  end;

implementation

procedure TDemo.Calculate(A, B: Integer);
var
  X, Y: Integer;
  S: string;
begin
  X := A + B;
  Y := A - B;
  X := X * Y;
  Y := X div Y;

  if (A > B) and (X < Y) then
    FValue := A
  else
    FValue := B;

  S := 'Test';
  S := S + IntToStr(X);

  FValue := Abs(X - Y);
end;

function TDemo.GetItem(Index: Integer): string;
var
  Arr: array[0..9] of Char;
begin
  Arr[0] := 'A';
  Arr[Index] := 'B';
  Result := Arr[0] + Arr[1];
end;

end.
