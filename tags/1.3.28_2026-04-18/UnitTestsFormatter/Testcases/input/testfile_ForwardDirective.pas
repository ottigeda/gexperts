unit ForwardDirective;

interface

implementation

procedure ProcA;  FORWARD ;

procedure ProcB;
begin
  ProcA;
end;

procedure ProcA;
begin
  ProcB;
end;

end.