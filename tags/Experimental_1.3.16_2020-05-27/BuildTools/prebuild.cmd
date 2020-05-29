@setlocal enableextensions
@if not defined gx_cmd_debug (echo off)
@endlocal
@rem prebuild.cmd should be called as pre-build event like this:
@rem call ..\buildtools\prebuild.cmd $(PROJECTPATH)
@echo %0
@echo running in %CD%

set PROJECTPATH=%1
rem remove quotes
set PROJECTPATH=%PROJECTPATH:"=%
rem echo PROJECTPATH=%PROJECTPATH%

if "%PROJECTPATH%"=="" goto NeedPara
rem echo PROJECTPATH=%PROJECTPATH%
call :DelExt %1
echo RESULT=%RESULT%
call :DelExt %RESULT%
set PROJECTNAMEONLY=%RESULT%
echo PROJECTNAMEONLY=%PROJECTNAMEONLY%

set OUTPUTDIR=%~dp1
rem echo OUTPUTDIR=%OUTPUTDIR%

pushd %OUTPUTDIR%
rem echo calling prepbuild.exe
set MANIFESTOPTIONS=
set INPUTMANIFEST=%PROJECTNAMEONLY%.manifest.in
if not exist "%INPUTMANIFEST%" goto nomaniin
set MANIFESTOPTIONS=--InputManifest="%INPUTMANIFEST%" --manifest="%PROJECTNAMEONLY%" --updatemanifest --WriteManifestRc="%PROJECTNAMEONLY%" --ignoremanifesterrors
:nomaniin 
rem echo MANIFESTOPTIONS=%MANIFESTOPTIONS%
"%~dp0\prepbuild.exe" --incbuild --BuildDateTime={today} --readini="%PROJECTNAMEONLY%" --WriteRc="%PROJECTNAMEONLY%" %MANIFESTOPTIONS%
brcc32 "%PROJECTNAMEONLY%_Version.rc"

echo checking for mainfest.rc
if not exist %PROJECTNAMEONLY%_Manifest.rc goto nomanirc
echo manifest.rc file found

echo checking for .manifest
if not exist %PROJECTNAMEONLY%.manifest goto nomani
echo .manifest file found
brcc32 %PROJECTNAMEONLY%_Manifest.rc
goto donemani
:nomanirc
echo Hint: %PROJECTNAMEONLY%_Manifest.rc not found, skipping
goto donemani
:nomani
echo Hint: %PROJECTNAMEONLY%.manifest not found, skipping
goto donemani
:donemani

echo checking for icon.rc
if not exist %PROJECTNAMEONLY%_Icon.rc goto noicon
echo icon.rc file found
%~dp0\rc %PROJECTNAMEONLY%_Icon.rc
goto doneicon
:noicon
echo Hint: %PROJECTNAMEONLY%_Icon.rc not found, skipping
:doneicon

popd

echo %0 exiting
goto :EOF

:NeedPara
echo Error: Needs the base filename of the executable as parameter
goto :EOF

:DelExt
setlocal
set RESULT=%~dpn1
endlocal & set RESULT=%RESULT%

