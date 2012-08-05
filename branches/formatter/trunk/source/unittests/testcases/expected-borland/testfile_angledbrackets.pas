unit testfile_AngledBrackets;

interface

implementation

procedure test;
begin
  // this is supposed to be changed to Assert(2 <= (1 + 1))
  Assert(2 <= (1 + 1));
  
  // this is supposed to be changed to Assert(2 <= 1 + 1)
  Assert(2 <= 1 + 1);
end;

end.
