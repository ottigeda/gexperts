unit testfile_IfdefAroundProcedure;

interface

{$IFDEF bla}
procedure blub;
{$ENDIF}

implementation

{$IFDEF bla}
procedure blub;
begin
end;
{$ENDIF}

end.