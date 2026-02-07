unit testfile_MultilineFunctionDirective2;

interface

function RegisterClipboardFormatW(lpszFormat: PWideChar): UINT;
  stdcall;
  external user32
  name 'RegisterClipboardFormatW';

implementation

end.