unit PreviewIndent;
{ Demonstrates indentation options }

interface

uses
  SysUtils
  , Classes
  , Forms;

type
  TExample = class
  private
    FValue: Integer;
  public
    procedure Process;
  end;

implementation

{$IFDEF DEBUG}
procedure DebugLog(const S: string);
begin
  WriteLn(S);
end;
{$ENDIF}

procedure TExample.Process;
  procedure NestedProc;
  begin
    FValue := 1;
  end;
var
  I: Integer;
label
  Done;
begin
  { Extra indent before begin }
  if FValue > 0 then begin
    NestedProc;
  end
  else begin
    FValue := 0;
  end;

  { Double indent if/while conditions }
  if (FValue > 100)
    and (FValue < 200) then
    I := 1;

  { Extra indent before try - try after then }
  if FValue > 0 then try
    I := StrToInt('123');
  except
    I := 0;
  end;

  { Extra indent before try else }
  try
    I := StrToInt('abc');
  except
    on E: EConvertError do
      I := -1;
  else
    I := 0;
  end;

  { Extra indent before case else }
  case FValue of
    0: I := 1;
    1: I := 2;
  else
    I := 0;
  end;

  { Never indent else if }
  if I = 0 then
    FValue := 0
  else if I = 1 then
    FValue := 1
  else if I = 2 then
    FValue := 2
  else
    FValue := -1;

  { Unindent labels }
  goto Done;
  I := 99;
Done:
  FValue := I;
end;

end.
