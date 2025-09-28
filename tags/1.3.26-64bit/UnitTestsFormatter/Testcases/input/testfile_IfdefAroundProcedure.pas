unit testfile_IfdefAroundProcedure;

interface

{$ifdef bla}
procedure blub;
{$endif}

implementation

{$ifdef bla}
procedure blub;
begin
end;
{$endif}

end.