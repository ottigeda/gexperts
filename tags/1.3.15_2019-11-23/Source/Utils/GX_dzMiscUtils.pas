{.GXFormatter.config=twm}
/// This is an extract from u_dzFileUtils in dzlib http://blog.dummzeuch.de/dzlib/ </summary>
///<summary>
/// Implements commonly used functions.
/// This unit implements some commonly used functions.
///<br/>
/// There is also a NotImplemend procedure which should be called
/// whereever some features are left out to be implemented "later"
/// This procedure will not be available when we compile the
/// shipping code (no DEBUG symbol), so the compiler should
/// complain if it is still used by then.
/// <br>
/// Note: String functions have been moved to dzStringUtils
/// Note: Variant functions have been moved to dzVariantUtils
/// @author twm
///</summary>

unit GX_dzMiscUtils;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils,
  Windows,
  Registry;

///<summary> Uses GetLastError to get the last WinAPI error code, then
///          calls SysErrorMessage to get the corresponding error string,
///          optionally takes a format string.
///          @param Error is the error string, only valid if error code <> 0
///          @param Format The Format string to use. It must have %d and %s in it, to
///                        change the order, use %0:d and %1:s, e.g. 'Error %1:s (%0:d)'
///                        %d is replaced by the error code and %s is replaced by the
///                        error message string.
///                        If no format string is given Error will just contain the
///                        Windows error message.
///                        NOTE: Do not pass a resource string or a string translated
///                              using DxGetText to this function since this
///                              would clear the GetLastError result. Use the
///                              overloaded version that takes the ErrCode
///                              parameter instead.
///          @returns the error code /</summary>
function GetLastOsError(_ErrCode: Integer; out _Error: string; const _Format: string = ''): DWORD;

///<summary> Same as VCL RaiseLastWin32Error but can specify a format.
///          This procedure does the same as the VCL RaiseLastWin32Error but you can
///          specify a format string to use. With this string you can provide some
///          additional information about where the error occured.
///          If ErrorCode <> 0 the function uses SysErrorMessage to retrieve an error
///          message for the error code and raises raises an EWin32Error exception
///          (to be compatible with the VCL function) with the Error message.
///          NOTE: If you pass a resource string as format parameter make sure you
///          call GetLastError before referencing the resource string, otherwise
///          loading the string will reset the error code returned by GetLastError, so
///          you always get 0.
///          @param ErrorCode is an error code returned from GetLastWin32Error
///          @param Format The Format string to use. It must have %d and %s in it, to
///                        change the order, use %0:d and %1:s, e.g. 'Error %1:s (%0:d)'
///                        %d is replaced by the error code and %s is replaced by the
///                        error message string. </summary>
procedure RaiseLastOsErrorEx(_ErrorCode: Integer; const _Format: string); overload;

implementation

{$IF not Declared(_)}

function _(const _s: string): string;
begin
  Result := _s;
end;
{$IFEND}

//procedure RaiseLastOsErrorEx(const _Format: string);
//begin
//  RaiseLastOsErrorEx(GetLastError, _Format);
//end;

procedure RaiseLastOsErrorEx(_ErrorCode: Integer; const _Format: string); overload;
var
  Error: EOSError;
begin
  if _ErrorCode <> ERROR_SUCCESS then
    Error := EOSError.CreateFmt(_Format, [_ErrorCode, SysErrorMessage(_ErrorCode)])
  else
    Error := EOSError.CreateFmt(_Format, [_ErrorCode, _('unknown OS error')]);
  Error.ErrorCode := _ErrorCode;
  raise Error;
end;

//function GetLastOsError(out _Error: string; const _Format: string = ''): DWORD;
//begin
//  Result := GetLastOsError(GetLastError, _Error, _Format);
//end;

function GetLastOsError(_ErrCode: Integer; out _Error: string; const _Format: string = ''): DWORD;
var
  s: string;
begin
  Result := _ErrCode;
  if Result <> ERROR_SUCCESS then
    s := SysErrorMessage(Result)
  else
    s := _('unknown OS error');
  if _Format <> '' then
    try
      _Error := Format(_Format, [Result, s])
    except
      _Error := s;
    end else
    _Error := s;
end;

end.
