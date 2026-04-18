unit testfile_label;

interface

implementation

procedure bla;
label
   SomeLabel;
begin
  hello;
  if someCondition then
    goto SomeLabel;
  world;
SomeLabel:
end;

procedure blub;
label
  99;
begin
  hello;
  if someCondition then
    goto 99;
  world; 99:
end;

procedure Nest;
label
   SomeLabel;

  procedure blub;
  label
    Error;
  begin
    hello;
    if someCondition then
      goto Error;
    world;
  Error: bla;
  end;

label
  99;
begin
  hello;
  if someCondition then
    goto SomeLabel;
  world;
SomeLabel:
    blub;
  hello;
  if someCondition then
    goto 99;
  world; 99: bla;
blub;
end;

procedure trallala(Sender: TObject);
label
  999;
label
  ALabel;
begin
  goto 999;
  raise exception.Create('test');
  999: asm nop end;
 ALabel:
     asm nop end;
end;

end.