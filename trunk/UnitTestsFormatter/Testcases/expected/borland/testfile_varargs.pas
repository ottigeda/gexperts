unit testfile_varargs;

interface

function wsprintf(Output: LPWSTR; Format: LPCWSTR): Integer; cdecl; varargs;

implementation

function wsprintf; external user32 name 'wsprintfW';

end.