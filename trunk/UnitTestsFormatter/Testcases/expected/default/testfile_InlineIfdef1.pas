unit testfile_InlineIfdef1;

interface

procedure bla; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

implementation

procedure bla;
begin
  bla;
end;

end.