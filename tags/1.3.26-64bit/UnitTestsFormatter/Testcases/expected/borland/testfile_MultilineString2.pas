unit testfile_MultilineString1;

interface

const
  bla = '''
         // this is a multi line string containing broken and oddly indented code
    if bla then begin
        CallIndentedTwice();
      CallIndentedOnce() // <== no semicolon here
  else
    some arbitrary text with a comma, a semicolon ; and a dot . here
  and something that looks like a formula:
    a:=    (x+  2  )  * 5
  // the above should not be changed at all
  ''';

implementation

end.