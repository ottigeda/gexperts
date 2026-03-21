unit magick_wand;

interface

function bla(wand: PMagickWand): MagickBooleanType; external 'wand.dll' index 5;

function ExternalMethod(const SomeString: PChar): Integer; stdcall; external 'cstyle.dll' delayed;

function MessageBox(HWnd: Integer; Text, Caption: PChar; Flags: Integer): Integer; stdcall;
  external 'user32.dll' name 'MessageBoxA';

function DllGetDataSnapClassObject(const [REF] CLSID, [REF] IID: TGUID; var Obj): HResult; cdecl;
  external 'libmidas.a' dependency 'stdc++';

function IsMagickWand(const wand: PMagickWand): MagickBooleanType; cdecl; external WandExport;
function MagickClearException(wand: PMagickWand): MagickBooleanType; cdecl; external 'wand.dll';
function bla(wand: PMagickWand): MagickBooleanType; external 'wand.dll';
function blub(wand: PMagickWand): MagickBooleanType; external;


function eins: integer; overload;

procedure blablub(a,b:char); external;

procedure Wandblablub(a,b:char); external WandExport;

implementation

end.
