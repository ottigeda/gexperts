unit testfile_FormatWithLineBreaks;

interface

implementation

procedure bla;
begin
  SQL := Format('Select %s from %s where (%s) and (%s)',
       [Field,
          Table,
            Condition1,
            Condition2]);
end;

end.
