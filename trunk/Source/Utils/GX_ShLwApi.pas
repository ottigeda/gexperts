unit GX_ShLwApi;
// used in Delphi 6 and 7 by defining a unit alias "ShLwApi=GX_ShLwAp"
// because the do not have the shlwapi unit
interface

{$IFNDEF HAS_SHLWAPI}

uses
  Windows;

const
  shlwapi32 = 'shlwapi.dll';

// Manually import ShLwApi routines in Delphi 7 and earlier (supported in W2K or later)
function PathCombine(szDest: PChar; lpszDir, lpszFile: PChar): PChar; stdcall;
  external shlwapi32 name 'PathCombineA';

function PathRelativePathTo(pszPath: PChar; pszFrom: PChar; dwAttrFrom: DWORD;
  pszTo: PChar; dwAttrTo: DWORD): BOOL; stdcall;
  external shlwapi32 name 'PathRelativePathToA';
{$ENDIF}

implementation

end.
