@rem Build GExperts Grep standalone tool
@setlocal enableextensions
@if not defined gx_cmd_debug (echo off)
@endlocal
setlocal

rem Select Delphi version
call "%~dp0doSelectDelphi.cmd"
if not defined delphiver goto :eof

rem Go to source directory
pushd src

rem Select the correct .dproj file based on format group
rem 2007 = Delphi 2007, 10 = Delphi 10.2-10.4, 11 = Delphi 11+
if "%delphigroup%"=="2007" (
  set dprname=GExpertsGrep.2007.dproj
) else if "%delphigroup%"=="10" (
  set dprname=GExpertsGrep.10.dproj
) else (
  set dprname=GExpertsGrep.dproj
)

rem Check if selected .dproj exists
if not exist "%dprname%" (
  echo Error: %dprname% not found
  popd
  pause
  goto :eof
)

echo Building %dprname% with Delphi %delphiver%
echo.

rem Find buildtools and get Delphi path
call "%~dp0..\..\doFindBuildTools.cmd"
call "%buildtools%\doGetDelphiPath.cmd" %delphiver%

if not exist "%DelphiPath%" (
  echo Error: Delphi not found at %DelphiPath%
  popd
  pause
  goto :eof
)

rem Build based on Delphi version
if "%delphigroup%"=="2007" goto build2007

rem Delphi 10.2+ build
call "%DelphiPath%\bin\rsvars.bat"
msbuild /target:rebuild /p:config=Release "%dprname%"
goto :done

:build2007
rem Delphi 2007 needs special handling for 64-bit Windows
set OldPath=%PATH%
call "%DelphiPath%\bin\rsvars.bat"
SET FrameworkDir=%SystemRoot%\Microsoft.NET\Framework\
SET PATH=%FrameworkDir%%FrameworkVersion%;%FrameworkSDKDir%;%OldPath%

rem Check for Borland.Delphi.Targets
if not exist "%FrameworkDir%%FrameworkVersion%\Borland.Delphi.Targets" (
  echo.
  echo ERROR: Borland.Delphi.Targets not found in
  echo %FrameworkDir%%FrameworkVersion%
  echo.
  echo This can happen after Windows updates.
  echo Copy the files from %buildtools%\dotNET2007 to the directory above.
  popd
  pause
  goto :eof
)

rem Set BDS for msbuild
set BDS=%DelphiPath:~0,-1%

rem Note: Delphi 2007 uses /p:Configuration= instead of /p:config=
msbuild /target:rebuild /p:Configuration=Release "%dprname%"

:done
popd
echo.
echo Done building %dprname%
pause
