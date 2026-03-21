// reduced to what's needed for the stand alone formatter
// Original Author:     Thomas Mueller (http://www.dummzeuch.de)
unit GX_CodeFormatterGXConfigWrapper;

{$I GX_CondDefine.inc}

interface

uses
  Classes,
  GX_CodeFormatterConfigHandler;

function GetDefaultCapitalizationFilename: string;
function GetConfigFilename: string;
function GetCapitalizationFilename: string;

implementation

uses
  GX_GenericUtils,
  u_dzFileUtils;

function GetConfigPathBS: string;
begin
  // We do not store the configuration in the registry, in particular not under some IDE version
  // specific GExperts registry key. Instead we put a Formatter.ini file and a Capitialization.txt
  // file in a subdirectory of
  // %USERPROFILE%\AppData\roaming
  Result := itpd(GetUserApplicationDataFolder) + itpd('GExperts') + itpd('CodeFormatter');
end;

function GetConfigFilename: string;
begin
  Result := GetConfigPathBS + 'Formatter.ini';
end;

function GetCapitalizationFilename: string;
begin
  Result := GetConfigPathBS + 'Capitalization.txt';
end;

function GetDefaultCapitalizationFilename: string;
begin
  Result := GetCapitalizationFilename;
end;

end.
