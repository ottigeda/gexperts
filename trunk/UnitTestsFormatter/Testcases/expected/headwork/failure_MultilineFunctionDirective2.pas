unit testfile_MultilineFunctionDirective1;

interface

function RegisterClipboardFormatW(lpszFormat: PWideChar): UINT;
  stdcall;
  external user32
  name 'RegisterClipboardFormatW';

implementation

end.