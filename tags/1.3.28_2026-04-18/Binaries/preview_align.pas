unit PreviewAlign;
{ Demonstrates alignment options }

interface

const
  MaxItems = 100;
  DefaultName = 'Unnamed';
  Version = '1.0';
  LongConstantName = 12345;

resourcestring
  SError = 'An error occurred';
  SWarning = 'Warning message';
  SInfo = 'Information';

type
  TDemo = class
  private
    FName: string;
    FValue: Integer;
    FDescription: string;
    FEnabled: Boolean;
  end;

implementation

procedure Test;
var
  I: Integer;
  LongVariableName: string;
  X: Double;
  Active: Boolean;
begin
  I := 0;           // initialize counter
  X := 1.5;         // set default
  Active := True;   // enable feature
  LongVariableName := 'test'; // set name
end;

procedure Another;
var
  A, B: Integer;
  Name: string;
begin
  A := 1;   // first value
  B := 2;   // second value
  Name := 'Demo';  // the name
  for A := 1 to 10 do
    B := B + A;
  for var I: Integer := 0 to 10 do
    B := B + I;
  var C: Integer := 42;
  var D := 'hello';
end;

end.
