unit testfile_CommentInContinuation;

interface

implementation

function TGrd2RdQPv3.TQprLaserData.IsValid: Boolean;
begin
  Result := HasBeenSet
    and InterpretStatus(Status)
    and InterpretValue(Value)
    // Some Comment
    // here
    // with
    // multiple
    // lines
  and (Value > 0);
end;

end.
