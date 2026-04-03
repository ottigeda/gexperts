unit GX_IdeUtils;

interface

uses
  Windows,
  SysUtils,
  Graphics,
  Controls,
  Forms;

const
  clWarningBack = $01FFFF; // BGR: almost yellow
  clWarningText = $010000; // BGR: almost black

function GetIdeRootDirectory: string;

///<summary>
/// Always returns true </summary>
function RunningDelphi8OrGreater: Boolean;

///<summary>
/// Always returns False </summary>
function FileIsWelcomePage(const FileName: string): Boolean;

///<summary>
/// @returns an empty string </summary>
function GetIdeEdition: string;

///<summary>
/// @returns False </summary>
function InitThemingForForm(const AForm: TCustomForm): Boolean;

///<summary>
/// @returns False </summary>
function IsThemingEnabled: Boolean;

///<summary>
/// does nothing </summary>
procedure RemoveStyleElements(AControl: TWinControl; IncludeSubComponents: Boolean);

///<summary>
/// @returns AColor </summary>
function GetThemedColor(AColor: TColor): TColor;

///<summary>
/// @returns an empty string </summary>
function GetIDEVersionID: string;

implementation

uses
  StrUtils,
  u_dzClassUtils,
  GX_OtaUtils,
  GX_GenericUtils;

function GetIdeRootDirectory: string;
const
  BinDirPostfix = PathDelim + 'Bin';
begin
  if TRegistry_TryReadString(GxOtaGetIdeBaseRegistryKey, 'RootDir', Result, HKEY_LOCAL_MACHINE) then begin
    if DirectoryExists(Result) then begin
      Result := AddSlash(Result);
      Exit; //==>
    end;
  end;

  // There is no entry in the registry or it is invalid -> use the application's exename
  Result := RemoveSlash(ExtractFilePath(Application.ExeName));
  if SameText(RightStr(Result, Length(BinDirPostfix)), BinDirPostfix) then begin
    Result := DeleteRight(Result, Length(BinDirPostfix));
    Result := AddSlash(Result);
  end else
    Result := '';
end;

function RunningDelphi8OrGreater: Boolean;
begin
  Result := True;
end;

function FileIsWelcomePage(const FileName: string): Boolean;
begin
  Result := False;
end;

function GetIdeEdition: string;
begin
  Result := '';
end;

function InitThemingForForm(const AForm: TCustomForm): Boolean;
begin
  Result := False;
end;

function IsThemingEnabled: Boolean;
begin
  Result := False;
end;

procedure RemoveStyleElements(AControl: TWinControl; IncludeSubComponents: Boolean);
begin
  // no nothing
end;

function GetThemedColor(AColor: TColor): TColor;
begin
  Result := AColor;
end;

function GetIDEVersionID: string;
begin
  Result := '';
end;

end.
