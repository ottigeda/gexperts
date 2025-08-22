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
  Error:
  end;

label
  99;
begin
  hello;
  if someCondition then
    goto SomeLabel;
  world;
SomeLabel:

  hello;
  if someCondition then
    goto 99;
  world; 99:
end;

end.