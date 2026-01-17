unit AnonymousWithNestedProc;

interface

implementation

procedure Bla;
begin
  fThread_UDPServer := TThread.CreateAnonymousThread(procedure(bla: integer)

    procedure NestedProcedureInsideAnonymousProcedure;
    begin
      CodeOfNestedProcedure;
    end;

    begin
      CodeOfTheAnonymousProcedure;
    end);
  CodeOfProcedureBla;
end;

end.
