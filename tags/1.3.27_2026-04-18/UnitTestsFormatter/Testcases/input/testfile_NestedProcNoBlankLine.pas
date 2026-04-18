unit TestNestedProcNoBlankLine;

interface

type
  TClass = class
  private
    procedure M1(S, K: integer; S2, K2: Single);
  end;

implementation

procedure TClass.M1(S, K: integer;
  S2, K2: Single);
procedure SubProc;
  begin
  end;
begin
end;

procedure TClass.M2(A: Integer);

procedure AnotherSubProc;
  begin
  end;
begin
end;

end.
