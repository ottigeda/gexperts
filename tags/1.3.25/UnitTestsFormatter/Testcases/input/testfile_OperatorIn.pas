unit OperatorIn;

interface

type
  TBla = record
    class operator In(_a: TypeA; _b: TypeB): boolean;
  end;

interface

class operator TBla.In(_a: TypeA; _b: TypeB): boolean;
begin
end;

end.