unit AnonymousWithNestedProc;

interface

implementation

Procedure Bla;
Begin
   fThread_UDPServer := TThread.CreateAnonymousThread(
     procedure
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
