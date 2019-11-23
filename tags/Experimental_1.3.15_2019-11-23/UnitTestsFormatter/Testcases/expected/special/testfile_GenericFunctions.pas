unit testfile_GenericFunctions;

interface

function FuncA<T: TMyClass>(out AResult: T): Boolean;
function FuncB<T>: T;
function FuncC<T>(const Default: T): T;
procedure ProcX<T>(const Bla: T; Blub: T);

implementation

function FuncA<T: TMyClass>(out AResult: T): Boolean;
begin
  Result := True;
end;

function FuncB<T>: T;
begin
  Result := T;
end;

function FuncC<T>(const Default: T): T;
begin
  Result := Default;
end;

procedure ProcX<T>(const Bla: T; Blub: T);
begin
  Bla := Blub;
end;

end.