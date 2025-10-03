unit testfile_GenericFunction2;

interface

function Something(Callback: TProc < TSomething > = nil): Boolean;

implementation

function Something(Callback: TProc < TSomething > = nil): Boolean;
begin
  Result := true;
end;

end.