unit testfile_IndentProcedureComment3;

interface

type
  TSomeType = Integer;

{: changes the string case as specified in aCase
   @param aStr is the input string
   @param aCase is a TCase specifying the desired case
   @returns the modified string }
function AdjustCase(a: string): string;

type
  TSomeType = Integer;

   {: changes the string case as specified in aCase
      @param aStr is the input string
      @param aCase is a TCase specifying the desired case
      @returns the modified string }
  function AdjustCase(a: string): string;

implementation
   {: changes the string case as specified in aCase
      @param aStr is the input string
      @param aCase is a TCase specifying the desired case
      @returns the modified string }
  function AdjustCase(a: string): string;
begin
end;

end.