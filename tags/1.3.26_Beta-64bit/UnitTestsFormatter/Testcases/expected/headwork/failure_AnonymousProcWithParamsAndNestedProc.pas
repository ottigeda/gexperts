unit AnonymousWithNestedProc;

interface

implementation

Procedure Bla;
Begin
   fThread_UDPServer := TThread.CreateAnonymousThread(procedure(bla: integer)
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
