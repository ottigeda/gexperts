///<summary> This unit contains operating system dependent functions, at least some of them. </summary>
unit GX_dzOsUtils;

interface

uses
  Windows,
  SysUtils,
  Classes;

type
  EOsFunc = class(Exception);
  EOFNoFileinfo = class(EOsFunc);

///<summary> Reads a file's version information and returns the four parts of the version
///          number.
///          @param Filename is a string with the name of the file to check, if empty, the
///                 current program is checked.
///          @param Major is a word returning the major version number
///          @param Minor is a word returning the minor version number
///          @param Revision is a word returning the revision number
///          @param Build is a word returning the build number
///          @returns True, if version information was found,
///                   False if the file does not contain any version information </summary>
///          @note: There is also an overloaded version that returns words.
function GetFileBuildInfo(_Filename: string;
  out _Major, _Minor, _Revision, _Build: Integer): Boolean; overload;

///<summary> Reads a file's version information and returns the four parts of the version
///          number.
///          @param Filename is a string with the name of the file to check, if empty, the
///                 current program is checked.
///          @param Major is an integer returning the major version number
///          @param Minor is an integer returning the minor version number
///          @param Revision is an integer returning the revision number
///          @param Build is an integer returning the build number
///          @returns True, if version information was found,
///                   False if the file does not contain any version information </summary>
///          @note: There is also an overloaded version that returns integers.
function GetFileBuildInfo(const _Filename: string;
  out _Major, _Minor, _Revision, _Build: Word): Boolean; overload;

///<summary> @returns the filename of the current module </summary>
function GetModuleFilename: string; overload;
function GetModuleFilename(const _Module: Cardinal): string; overload;

///<summary> tries to open a file with the associated application
///          @param Filename is the name of the file to open
///          @returns true on success, false otherwise </summary>
function OpenFileWithAssociatedApp(const _Filename: string; _ShowAssociateDialog: Boolean = False): Boolean;

///<summary> Calls ShellExecuteEx with the given parameters </summary>
function ShellExecEx(const _Filename: string; const _Parameters: string;
  const _Verb: string; _CmdShow: Integer; _ShowAssociateDialog: Boolean = False): Boolean;

///<summary>
/// Opens Windows Explorer and selects the given file, if it exists. If it doesn't exist, it
/// opens the first directory in that filename that exists.
/// @returns True if the file exists, False if not </summary>
function OpenExplorerAndSelectFile(const _Filename: string): Boolean;

///<summary>
/// @returns true, if the Ctrl key is currently pressed </summary>
function IsCtrlDown: Boolean;
///<summary>
/// @returns true, if the Shift key is currently pressed </summary>
function IsShiftDown: Boolean;

///<summary>
/// Returns a human readable string with the OS version, e.g.
/// 'Windows XP', 'Windows 8.1' or 'Windows 10 (1809)'
/// (For Windows 10 this is probably more a guess since there seems to be no official method
///  for getting that version. (Or maybe I haven't looked hard enough.))
/// @Note: Versions older than Windows 2000 (Windows 95/98/ME and Windows NT3.1, 3.5 and 4)
/// did not support the GetVersionEx API call used here. But we don't really care any more. </summary>
function GetOsVersionString: string;

implementation

uses
  ShellAPI;

function IsShiftDown: Boolean;
var
  State: TKeyboardState;
begin
  GetKeyboardState(State);
  Result := ((State[vk_Shift] and 128) <> 0);
end;

function IsCtrlDown: Boolean;
var
  State: TKeyboardState;
begin
  GetKeyboardState(State);
  Result := ((State[VK_CONTROL] and 128) <> 0);
end;

function GetModuleFilename(const _Module: Cardinal): string;
var
  Buffer: array[0..260] of Char;
begin
  SetString(Result, Buffer, Windows.GetModuleFilename(_Module, Buffer, SizeOf(Buffer)))
end;

function GetModuleFilename: string;
begin
  Result := GetModuleFilename(HInstance);
end;

function GetFileBuildInfo(_Filename: string;
  out _Major, _Minor, _Revision, _Build: Integer): Boolean;
var
  VerInfoSize: DWORD;
  VerInfo: Pointer;
  VerValueSize: DWORD;
  VerValue: PVSFixedFileInfo;
  Dummy: DWORD;
begin
  if _Filename = '' then
    _Filename := GetModuleFilename;
  VerInfoSize := GetFileVersionInfoSize(PChar(_Filename), Dummy);
  Result := (VerInfoSize <> 0);
  if Result then begin
    GetMem(VerInfo, VerInfoSize);
    try
      GetFileVersionInfo(PChar(_Filename), 0, VerInfoSize, VerInfo);
      VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
      with VerValue^ do begin
        _Major := dwFileVersionMS shr 16;
        _Minor := dwFileVersionMS and $FFFF;
        _Revision := dwFileVersionLS shr 16;
        _Build := dwFileVersionLS and $FFFF;
      end;
    finally
      FreeMem(VerInfo, VerInfoSize);
    end;
  end;
end;

function GetFileProductInfo(_Filename: string;
  out _Major, _Minor, _Revision, _Build: Integer): Boolean;
var
  VerInfoSize: DWORD;
  VerInfo: Pointer;
  VerValueSize: DWORD;
  VerValue: PVSFixedFileInfo;
  Dummy: DWORD;
begin
  if _Filename = '' then
    _Filename := GetModuleFilename;
  VerInfoSize := GetFileVersionInfoSize(PChar(_Filename), Dummy);
  Result := (VerInfoSize <> 0);
  if Result then begin
    GetMem(VerInfo, VerInfoSize);
    try
      GetFileVersionInfo(PChar(_Filename), 0, VerInfoSize, VerInfo);
      VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
      with VerValue^ do begin
        _Major := dwProductVersionMS shr 16;
        _Minor := dwProductVersionMS and $FFFF;
        _Revision := dwProductVersionLS shr 16;
        _Build := dwProductVersionLS and $FFFF;
      end;
    finally
      FreeMem(VerInfo, VerInfoSize);
    end;
  end;
end;

function GetFileBuildInfo(const _Filename: string;
  out _Major, _Minor, _Revision, _Build: Word): Boolean;
var
  Major, Minor, Revision, Build: Integer;
begin
  Result := GetFileBuildInfo(_Filename, Major, Minor, Revision, Build);
  if Result then begin
    _Major := Major;
    _Minor := Minor;
    _Revision := Revision;
    _Build := Build;
  end;
end;

function ShellExecEx(const _Filename: string; const _Parameters: string;
  const _Verb: string; _CmdShow: Integer; _ShowAssociateDialog: Boolean = False): Boolean;
var
  Sei: TShellExecuteInfo;
begin
  FillChar(Sei, SizeOf(Sei), #0);
  Sei.cbSize := SizeOf(Sei);
  Sei.FMask := SEE_MASK_DOENVSUBST;
  if not _ShowAssociateDialog then
    Sei.FMask := Sei.FMask or SEE_MASK_FLAG_NO_UI;
  Sei.lpFile := PChar(_Filename);
  if _Parameters <> '' then
    Sei.lpParameters := PChar(_Parameters)
  else
    Sei.lpParameters := nil;
  if _Verb <> '' then
    Sei.lpVerb := PChar(_Verb)
  else
    Sei.lpVerb := nil;
  Sei.nShow := _CmdShow;
  Result := ShellExecuteEx(@Sei);
end;

function OpenExplorerAndSelectFile(const _Filename: string): Boolean;
var
  fn: string;
begin
  fn := _Filename;
  Result := FileExists(fn);
  if Result then begin
    ShellExecEx('explorer.exe', '/select,"' + fn + '"', '', SW_SHOWNORMAL);
    Exit; //==>
  end;

  while ExtractFileName(fn) <> '' do begin
    fn := ExtractFileDir(fn);
    if (fn <> '') and DirectoryExists(fn) then begin
      ShellExecEx(fn, '', '', SW_SHOWNORMAL);
      Exit; //==>
    end;
  end;
end;

function OpenFileWithAssociatedApp(const _Filename: string; _ShowAssociateDialog: Boolean = False): Boolean;
begin
  Result := ShellExecEx(_Filename, '', 'open', SW_SHOWNORMAL, _ShowAssociateDialog);
end;

// Delphi 2007 declares only the GetVersionEx function with a TOsVersionInfo record,
// so we have to add an overloaded version of it for a TOsVersionInfoEx record.
type
  TOsVersionInfoExW = record
    dwOSVersionInfoSize: DWORD;
    dwMajorVersion: DWORD;
    dwMinorVersion: DWORD;
    dwBuildNumber: DWORD;
    dwPlatformId: DWORD;
    szCSDVersion: array[0..127] of WideChar; { Maintenance UnicodeString for PSS usage }
    wServicePackMajor: Word;
    wServicePackMinor: Word;
    wSuiteMask: Word;
    wProductType: BYTE;
    wReserved: BYTE;
  end;

  TOsVersionInfoExA = record
    dwOSVersionInfoSize: DWORD;
    dwMajorVersion: DWORD;
    dwMinorVersion: DWORD;
    dwBuildNumber: DWORD;
    dwPlatformId: DWORD;
    szCSDVersion: array[0..127] of AnsiChar; { Maintenance AnsiString for PSS usage }
    wServicePackMajor: Word;
    wServicePackMinor: Word;
    wSuiteMask: Word;
    wProductType: BYTE;
    wReserved: BYTE;
  end;

function GetVersionExA(var lpVersionInformation: TOsVersionInfoExA): BOOL; stdcall; overload; external kernel32 Name 'GetVersionExA';

function GetVersionExW(var lpVersionInformation: TOsVersionInfoExW): BOOL; stdcall; overload; external kernel32 Name 'GetVersionExW';

{$IFDEF unicode}
type
  TOsVersionInfoEx = TOsVersionInfoExW;

function GetVersionEx(var lpVersionInformation: TOsVersionInfoEx): BOOL; stdcall; overload; external kernel32 Name 'GetVersionExW';

{$ELSE}
type
  TOsVersionInfoEx = TOsVersionInfoExA;

function GetVersionEx(var lpVersionInformation: TOsVersionInfoEx): BOOL; stdcall; overload; external kernel32 Name 'GetVersionExA';

{$ENDIF}

function GetKernel32Version(out _Major, _Minor, _Revision, _Build: Integer): Boolean;
var
  Kernel32Handle: DWORD;
  Kernel32Filename: string;
begin
  Kernel32Handle := GetModuleHandle('kernel32.dll');
  Kernel32Filename := GX_dzOsUtils.GetModuleFilename(Kernel32Handle);
  Result := GetFileProductInfo(Kernel32Filename, _Major, _Minor, _Revision, _Build);
end;

function OsVersionFromKernel32Version(_IsServer: Boolean): string;
var
  Major: Integer;
  Minor: Integer;
  Revision: Integer;
  Build: Integer;
begin
  // Starting with Windows 8 the GerVersionEx function is lying:
  // > With the release of Windows 8.1, the behavior of the GetVersionEx API has changed
  // in the value it will return for the operating system version. The value returned
  // by the GetVersionEx function now depends on how the application is manifested.
  // Applications not manifested for Windows 8.1 or Windows 10 will return the
  // Windows 8 OS version value (6.2). Once an application is manifested for a given
  // operating system version, GetVersionEx will always return the version that the
  // application is manifested for in future releases. To manifest your applications
  // for Windows 8.1 or Windows 10 <
  // https://docs.microsoft.com/en-us/windows/desktop/api/sysinfoapi/nf-sysinfoapi-getversionexa
  // So, we can only get the correct version, if the Delphi IDE has a manifest telling
  // Windows that it supports the version installed. This of course will not work if
  // the Delphi version is older than the Windows version (e.g. Delphi 2007 won't know
  // about anything newer than Windows XP).
  // Instead we now use GetFileVersionInfo on kernel32.dll.
  // https://docs.microsoft.com/en-us/windows/desktop/sysinfo/getting-the-system-version

  if not GetKernel32Version(Major, Minor, Revision, Build) then begin
    Result := 'Unknown OS version (GetFileProductInfo(kernel32.dll) failed).';
    Exit; //==>
  end;

  // we start with the most likely ones (not that it matters much here, nobody is going
  // to call this function so often that it makes a difference performance wise)
  if (Major = 10) and (Minor = 0) then begin
    // We have reached the "last" Windows version 10, which of course still has
    // new version numbers internally for each major update:
    // For whatever reason, the Windows 10 revision number is often called build number.
    // (e.g. https://en.wikipedia.org/wiki/Windows_10_version_history )
    // But it is really the revision number.
    //
    // As of 2019-04-21 there are the following revisions (newest first):
    // Windows 10 (1903) 10.0.18362
    // Windows 10 (1809) 10.0.17763
    // Windows 10 (1803) 10.0.17134
    // Windows 10 (1709) 10.0.16299
    // Windows 10 (1703) 10.0.15063
    // Windows 10 (1607) 10.0.14393
    // Windows 10 (1511) 10.0.10586
    // Windows 10 (1507) 10.0.10240 (the first release from 2015)

    if _IsServer then
      Result := 'Windows Server 2016'
    else
      Result := 'Windows 10';

    // todo: There should be a way to detect that there has been a new major Windows 10 update ...
    if Revision >= 18362 then begin
      Result := Format('%s (%d) (kernel %d.%d.%d build %d)', [Result, 1903, Major, Minor, Revision, Build]);
    end else if Revision >= 17763 then begin
      Result := Format('%s (%d) (kernel %d.%d.%d build %d)', [Result, 1809, Major, Minor, Revision, Build]);
    end else if Revision >= 17134 then begin
      Result := Format('%s (%d) (kernel %d.%d.%d build %d)', [Result, 1803, Major, Minor, Revision, Build]);
    end else if Revision >= 16299 then begin
      Result := Format('%s (%d) (kernel %d.%d.%d build %d)', [Result, 1709, Major, Minor, Revision, Build]);
    end else if Revision >= 15063 then begin
      Result := Format('%s (%d) (kernel %d.%d.%d build %d)', [Result, 1703, Major, Minor, Revision, Build]);
    end else if Revision >= 14393 then begin
      Result := Format('%s (%d) (kernel %d.%d.%d build %d)', [Result, 1607, Major, Minor, Revision, Build]);
    end else if Revision >= 10586 then begin
      Result := Format('%s (%d) (kernel %d.%d.%d build %d)', [Result, 1511, Major, Minor, Revision, Build]);
    end else if Revision >= 10240 then begin
      Result := Format('%s (%d) (kernel %d.%d.%d build %d)', [Result, 1507, Major, Minor, Revision, Build]);
    end else
      Result := Format('Windows %d.%d rev %d build %d (from kernel32)', [Major, Minor, Revision, Build]);
  end else if Major = 6 then begin
    if Minor = 3 then begin
      if _IsServer then
        Result := 'Windows Server 2012 R2'
      else
        Result := 'Windows 8.1';
    end else
      Result := Format('Unknown Windows %d.%d rev %d build %d (from kernel32)', [Major, Minor, Revision, Build]);
  end else
    Result := Format('Unknown Windows %d.%d rev %d build %d (from kernel32)', [Major, Minor, Revision, Build]);
end;

function GetOsVersionString: string;
const
  VER_NT_WORKSTATION = $0000001;
  VER_NT_DOMAIN_CONTROLLER = $0000002;
  VER_NT_SERVER = $0000003;
var
  VerInfo: TOsVersionInfoEx;
  IsServer: Boolean;
begin
  ZeroMemory(@VerInfo, SizeOf(VerInfo));
  VerInfo.dwOSVersionInfoSize := SizeOf(VerInfo);
  if not GX_dzOsUtils.GetVersionEx(VerInfo) then begin
    Result := 'Unknown OS version (GetVersionInfoEx failed).';
    Exit; //==>
  end;

  // todo: This might not be correct. MSDN tells us to use GetSystemMetrics for some
  //       server OSs, but since I don't have any way to test it, I might as well
  //       just not go through the trouble of implementing it.
  IsServer := (VerInfo.wProductType <> VER_NT_WORKSTATION);

  // we start with the most likely ones
  if (VerInfo.dwMajorVersion = 10) and (VerInfo.dwMinorVersion = 0) then begin
    // Windows 10 or Windows Server 2016
    // But this only works if the manifest tells Windows not to lie.
    // Since we don't know the version number ("Windows 10" is the product, it's not Windows
    // version 10), we need to check further anyway.
    Result := OsVersionFromKernel32Version(IsServer);
  end else if VerInfo.dwMajorVersion = 6 then begin
    if VerInfo.dwMinorVersion = 2 then begin
      // Windows 8
      // In theory at least, but Windows might be lying so we need to check further
      Result := OsVersionFromKernel32Version(IsServer);
    end else if VerInfo.dwMinorVersion = 3 then begin
      // Windows 8.1
      // But this only works if the manifest tells Windows not to lie
      // otherwise all versions newer than Windows 8 will be reported as Windows 8
      if IsServer then
        Result := 'Windows Server 2012 R2'
      else
        Result := 'Windows 8.1';
    end else if VerInfo.dwMinorVersion = 1 then begin
      // Windows 7
      if IsServer then
        Result := 'Windows Server 2008 R2'
      else
        Result := 'Windows 7';
    end else if VerInfo.dwMinorVersion = 0 then begin
      // Windows Vista
      if IsServer then
        Result := 'Windows Server 2008'
      else
        Result := 'Windows Vista';
    end else
      Result := Format('Unknown Windows version %d.%d build %d',
        [VerInfo.dwMajorVersion, VerInfo.dwMinorVersion, VerInfo.dwBuildNumber]);
  end else if VerInfo.dwMajorVersion = 5 then begin
    if VerInfo.dwMinorVersion = 2 then begin
      Result := 'Windows XP Professional x64 Edition'
      // or Windows Server 2003, Windows Home Server or Windows Server 2003 R2
      // maybe we should return a server version rather than the 64 bit XP edition,
      // it was probably more common.
    end else if VerInfo.dwMinorVersion = 1 then begin
      Result := 'Windows XP'
    end else if VerInfo.dwMinorVersion = 0 then begin
      Result := 'Windows 2000'
    end else
      Result := Format('Unknown Windows version %d.%d build %d',
        [VerInfo.dwMajorVersion, VerInfo.dwMinorVersion, VerInfo.dwBuildNumber]);
  end else
    Result := Format('Unknown Windows version %d.%d build %d',
      [VerInfo.dwMajorVersion, VerInfo.dwMinorVersion, VerInfo.dwBuildNumber]);
end;

end.

