unit testfile_External

interface

implementation

procedure MoveWord(var Source, Dest; Count: Integer); external;
function SomeFunction(s: string): string; external 'strlib.dll';
function MessageBox(HWnd: Integer; Text, Caption: pchar; Flags: Integer): Integer;
  stdcall; external 'user32.dll' name 'MessageBoxA';
function MessageBox(HWnd: Integer; Text, Caption: pchar; Flags: Integer): Integer;
  stdcall; external 'user32.dll' index 5;
procedure FunctionName; cdecl; external object 'ObjectFile.o' Name '_FunctionName';
function foo: Integer; external framework 'framework name';
function ExternalMethod(const SomeString: pchar): Integer; stdcall;
  external 'cstyle.dll' delayed;
function DllGetDataSnapClassObject(const [REF]CLSID, [REF]IID: TGUID; var Obj): HResult;
  cdecl; external 'libmidas.a' dependency 'stdc++';

end.