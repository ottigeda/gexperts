unit testfile_External

interface

implementation

procedure MoveWord(var Source, Dest; Count: Integer); external;
function SomeFunction(s: string): string; External 'strlib.dll';
function MessageBox(HWnd: Integer; Text, Caption: pchar; Flags: Integer): Integer;
  stdcall; External 'user32.dll' Name 'MessageBoxA';
function MessageBox(HWnd: Integer; Text, Caption: pchar; Flags: Integer): Integer;
  stdcall; External 'user32.dll' Index 5;
procedure FunctionName; cdecl; External Object 'ObjectFile.o' Name '_FunctionName';
function foo: Integer; external Framework 'framework name';
function ExternalMethod(const SomeString: pchar): Integer; stdcall;
  external 'cstyle.dll' Delayed;
function DllGetDataSnapClassObject(const [REF]CLSID, [REF]IID: TGUID; var Obj): HResult;
  cdecl; external 'libmidas.a' Dependency 'stdc++';

end.