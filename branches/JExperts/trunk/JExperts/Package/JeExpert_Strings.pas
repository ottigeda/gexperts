{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JeExpert_Strings.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the JEDI Experts home page,
located at http://jexperts.sourceforge.net/

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}


unit JeExpert_Strings;

interface

ResourceString
  //Variables
  RC_PackTitle          = 'JExperts 1.00';
  RC_PackVersion        = '1.00';
  RC_RegistryPath       = 'Software\JEDI\JExperts\';

  //Messages
  CrLf                  = #10#13;
  RC_ExpertDllName      = 'JExperts.dll';
  RC_ExpertDllFailed    = 'Unable to find expert library... '+#10#13+
                          'You have probably deleted the JExperts.dll, reinstall the package !';
  RC_ExpertDllCorrupted = 'The JExperts.dll file is corrupted, please reinstall the package';
  RC_ExpertDllBadVersion= 'JExperts.dll has a bad version. You have probably not installed correctly the pack';

  RC_BadMenuName        = 'Insert before menu name is invalid !';

  //Compiler specific
  {$IFDEF Delphi4}
  RC_ExpertOptions = 'Options_D4';
  RC_PalettePath   = 'Software\Borland\Delphi\4.0\Palette';
  RC_SpecificPath  = 'Delphi4';
  {$ELSE}
   {$IFDEF Delphi5}
     RC_ExpertOptions = 'Options_D5';
     RC_PalettePath   = 'Software\Borland\Delphi\5.0\Palette';
     RC_SpecificPath  = 'Delphi5\';
   {$ELSE}
    {$IFDEF Delphi6}
      RC_ExpertOptions = 'Options_D6';
      RC_PalettePath   = 'Software\Borland\Delphi\6.0\Palette';
      RC_SpecificPath  = 'Delphi6\';
    {$ENDIF}
   {$ENDIF}
  {$ENDIF}

  {$IFDEF DELPHI}
  RC_FileExtensions   = '.DFM;.DPR;.PAS';
  RC_SourceExtension  = '.PAS;.DPR';
  RC_SourceDefExt     = '.pas';
  RC_PrjExtension     = '.DPR';
  RC_BatchCompiler    = 'dcc32.exe';
  RC_PathPseudo       = '$(DELPHI)';
  {$ENDIF}

  {$IFDEF BUILDER}
  //XXX CPP
  {$ENDIF}

implementation

end.
