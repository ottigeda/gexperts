unit testfile_NotInOperator;

interface

type
  TColor = (clRed, clGreen, clBlue, clYellow);
  TColors = set of TColor;

procedure TestNotIn;

implementation

procedure TestNotIn;
var
  Color: TColor;
  Colors: TColors;
begin
  Color := clRed;
  Colors := [clGreen, clBlue];

  if Color not in Colors then
    WriteLn('Color is not in set');

  if clYellow not in [clRed, clGreen] then
    WriteLn('Yellow not in red/green');

  // Combined with other conditions
  if (Color not in Colors) and (Color = clRed) then
    WriteLn('Red and not in colors');

  // Nested in complex expression
  while Color not in [clBlue, clYellow] do begin
    WriteLn('Still processing');
    Break;
  end;
end;

end.
