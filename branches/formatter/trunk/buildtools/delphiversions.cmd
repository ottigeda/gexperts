@rem set environment variables for various Delphi versions
@echo off

rem Support fuer Windows 7/8, 64 Bit
set ProgFiles=%ProgramFiles(x86)%
if not "%ProgFiles%"=="" goto Win64Bit
set ProgFiles=%ProgramFiles%
:Win64Bit

if not "%DelphiProgDir%"=="" goto custom

set Delphi2007Dir=%ProgFiles%\CodeGear\RAD Studio\5.0
set Delphi2009Dir=%ProgFiles%\CodeGear\RAD Studio\6.0
set Delphi2010Dir=%ProgFiles%\Embarcadero\RAD Studio\7.0
set DelphiXEDir=%ProgFiles%\Embarcadero\RAD Studio\8.0
set DelphiXE2Dir=%ProgFiles%\Embarcadero\RAD Studio\9.0
set DelphiXE3Dir=%ProgFiles%\Embarcadero\RAD Studio\10.0
set DelphiXE4Dir=%ProgFiles%\Embarcadero\RAD Studio\11.0
goto :eof

:custom
set Delphi2007Dir=%DelphiProgDir%\Delphi2007
set Delphi2009Dir=%DelphiProgDir%\Delphi2009
set Delphi2010Dir=%DelphiProgDir%\Delphi2010
set DelphiXEDir=%DelphiProgDir%\DelphiXE1
set DelphiXE2Dir=%DelphiProgDir%\DelphiXE2
set DelphiXE3Dir=%DelphiProgDir%\DelphiXE3
rem Stupid me forgot to install XE4 to c:\delphi - sorry if anybody relied on this being consistent
set DelphiXE4Dir=%ProgFiles%\Embarcadero\RAD Studio\11.0
goto :eof
