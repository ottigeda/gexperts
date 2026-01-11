unit testfile_InlineIfdef2;

interface

procedure bla;

implementation

procedure bla;
begin
{$IFDEF DEBUG}WriteLn('This is debug mode.'); {$ENDIF}
end;

end.