unit PreviewLineBreaks;
{ Demonstrates line break options }

interface
uses SysUtils, Classes, Forms, Graphics, Controls;

type
  TDemo = class
  public
    procedure Execute;
  end;

implementation
uses Windows, Messages;

procedure TDemo.Execute;
var I: Integer; S: string;
begin
  { Begin style demo }
  if I > 0 then begin S := 'positive'; end else begin S := 'negative'; end;

  { Try style demo - code before try on same line }
  S := 'test'; try I := StrToInt(S); except I := 0; finally S := ''; end;

  { Feed after then/else/do }
  if I = 0 then S := 'zero' else S := 'other';
  while I > 0 do Dec(I);
  case I of 0: S := 'zero'; 1: S := 'one'; end;


  { Remove double blank - see above }

  { Never before then/do - then/do on own line }
  if (I > 0) and (I < 100)
  then S := 'ok';

  while (I > 0) and (I < 100)
  do Dec(I);

  { Feed between else and if }
  if I = 0 then S := 'zero' else if I = 1 then S := 'one' else S := 'other';

  { Feed before end - end on same line as code }
  if I > 0 then begin
    S := 'yes'; end;

  begin
    I := 1; end;

  { Feed before else }
  if I > 0 then S := 'a' else S := 'b';

  { Feed after semicolon }
  I := 1; S := 'test'; I := I + 1;
end;

{ Blank line between main procedures }
procedure First;
begin
end;
procedure Second;
begin
end;

procedure WithLocals;
  { Blank line between local procedures }
  procedure LocalA;
  begin
  end;
  procedure LocalB;
  begin
  end;
begin
  LocalA; LocalB;
end;

end.
