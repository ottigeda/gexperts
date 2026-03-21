unit testfile_CommentIndent;

interface

implementation

begin
  case Value of
    1,
      { 1st line.
        2nd line - will be outdented. }
    2:
      begin
      end;
  end;
end.
