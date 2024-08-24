unit testfile_Absolute;

interface

implementation

var
  VarA: Integer;
  VarB: Pointer absolute VarA;
  VarC: LongInt
    absolute VarA;
  VarD: Byte;

end.