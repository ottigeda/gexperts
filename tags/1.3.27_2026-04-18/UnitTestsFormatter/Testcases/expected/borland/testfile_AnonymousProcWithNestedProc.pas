unit AnonymousWithNestedProc;

interface

implementation

function Bla: boolean;
begin
  fThread_UDPServer := TThread.CreateAnonymousThread(procedure
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
