unit testfile_RaiseAt;

interface

implementation

procedure Test;
var
  P: Pointer;
begin
  raise EAbstractError.CreateResFmt(PResStringRec(@SAbstractError), ['']) at P;
  raise Exception.Create('test') at ReturnAddress;
  raise E at Addr;
end;

end.
