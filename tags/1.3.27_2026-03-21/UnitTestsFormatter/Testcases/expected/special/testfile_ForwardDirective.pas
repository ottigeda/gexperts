unit ForwardDirective;

interface

implementation

procedure ProcA; forward;

procedure ProcB;
begin
  ProcA;
end;

procedure ProcA;
begin
  ProcB;
end;

end.