unit testfile_CompilerMessage;

interface

{$MESSAGE WARN 'warning'}
procedure bla;

{$MESSAGE ERROR 'error'}

implementation

{$MESSAGE HINT 'hint'}

end.

{$MESSAGE FATAL 'fatal error'}

{$MESSAGE INVALID 'this is not a valid message type, but we uppercase it anyway'}
