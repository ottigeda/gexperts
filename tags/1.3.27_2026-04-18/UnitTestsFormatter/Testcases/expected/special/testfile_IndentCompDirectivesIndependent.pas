unit testfile_IndentCompDirectivesIndependent;

interface

implementation

procedure TestProc;
begin
  begin
    begin
      WriteLn('bla');
{$IFDEF CONDITIONALEXPRESSIONS}
  {$DEFINE HAS_COND_EXPR}
  {$IFDEF BCB}
      WriteLn('bcb');
  {$ELSE}
      WriteLn('no bcb');
  {$ENDIF}
  {$IF CompilerVersion >= 14}
      WriteLn('14');
  {$IFEND}
{$ENDIF}
    end;
  end;
end;

end.
