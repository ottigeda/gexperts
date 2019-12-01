unit testfile_Absolute;

interface

implementation

var
  VarA: Integer;
  VarB: Pointer absolute VarA;
  VarC: LongInt
    absolute VarA;
  VarD: Byte;

procedure Bla;
var
  Var1: array[0..4] of byte;
  Var2: integer absolute Var1;
  Var3: pointer 
    absolute Var1;
  Var4: string;
begin
end;

end.