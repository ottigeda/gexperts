unit testfile_CompilerMessage;

interface

{$message warn 'warning'}
procedure bla;

{$message error 'error'}

implementation

{$message hint 'hint'}

end.

{$message fatal 'fatal error'}

{$message invalid 'this is not a valid message type, but we uppercase it anyway'}
