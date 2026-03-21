unit PreviewMisc;
{ Demonstrates miscellaneous options }

interface

type
  TDemo = class
    procedure LongMethodWithManyParametersDemo;
  end;

implementation

{ GXFormatter.config=<name> can override settings }

procedure TDemo.LongMethodWithManyParametersDemo;
var
  VeryLongVariableNameForDemonstration: string;
begin
  VeryLongVariableNameForDemonstration := 'This is a very long string value that might need to be wrapped depending on the wrap position setting';
end;

{(*}
// Code between {(*} and {*)} is not formatted
procedure   NotFormatted  ;
begin
  // This keeps original formatting
    if True then
  WriteLn('preserved');
end;
{*)}

procedure NormallyFormatted;
begin
  if True then
    WriteLn('formatted');
end;

{ Configuration precedence:
  1. GXFormatter.config directive in source
  2. GXFormatter.ini file in project folder
  3. Your settings configured here }

end.
