unit testfile_MultilineFunctionDirective2;

interface

function RegisterClipboardFormatW(lpszFormat: PWideChar): UINT;
  stdcall;
  external user32
  Name 'RegisterClipboardFormatW';

implementation

end.