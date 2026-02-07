unit AnonymousWithNestedProc;

interface

implementation

function Bla: boolean;
Begin
   fThread_UDPServer := TThread.CreateAnonymousThread(Procedure
     Procedure NestedProcedureInsideAnonymousProcedure;
     Begin
       CodeOfNestedProcedure;
     End;

   Begin
     CodeOfTheAnonymousProcedure;
   End);
  CodeOfProcedureBla;
End;

end.
