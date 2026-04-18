unit testfile_DisableFormattingInline;

interface

procedure Something; {(*}{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}{*)}

implementation

procedure Something;
begin
end;

end.
