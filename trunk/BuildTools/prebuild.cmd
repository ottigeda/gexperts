@setlocal enableextensions
@if not defined gx_cmd_debug (echo off)
@endlocal
@rem prebuild.cmd should be called as pre-build event like this:
@rem call ..\buildtools\prebuild.cmd $(PROJECTPATH)
@echo %0
@echo running in %CD%

set PROJECTPATH="%1"
rem remove quotes
set PROJECTPATH=%PROJECTPATH:"=%
rem echo PROJECTPATH=%PROJECTPATH%

subwcrev %~dp0\.. %~dp0\templates\SVN_Version_template.ini %~dp0\..\SVN_Version.ini

if "%PROJECTPATH%"=="" goto NeedPara
rem echo PROJECTPATH=%PROJECTPATH%
call :DelExt %1
echo RESULT=%RESULT%
call :DelExt "%RESULT%"
set PROJECTNAMEONLY=%RESULT%
echo PROJECTNAMEONLY=%PROJECTNAMEONLY%

set OUTPUTDIR="%~dp1"
rem echo OUTPUTDIR=%OUTPUTDIR%

pushd %OUTPUTDIR%
rem echo calling prepbuild.exe
set MANIFESTOPTIONS=
set INPUTMANIFEST="%PROJECTNAMEONLY%.manifest.in"
if not exist %INPUTMANIFEST% goto nomaniin
set MANIFESTOPTIONS=--InputManifest=%INPUTMANIFEST% --manifest="%PROJECTNAMEONLY%" --updatemanifest --WriteManifestRc="%PROJECTNAMEONLY%" --ignoremanifesterrors
:nomaniin
rem echo MANIFESTOPTIONS=%MANIFESTOPTIONS%
"%~dp0\prepbuild.exe" --BuildDateTime={today} --readini="%PROJECTNAMEONLY%" --WriteRc="%PROJECTNAMEONLY%" %MANIFESTOPTIONS%
echo compiling %PROJECTNAMEONLY%_Version.rc
brcc32 "%PROJECTNAMEONLY%_Version.rc"

echo checking for manifest.rc
set MANIFESTRC=%PROJECTNAMEONLY%_Manifest.rc
if not exist "%MANIFESTRC%" goto nomanirc
echo found: %MANIFESTRC%

echo checking for .manifest
set DOTMANIFEST=%PROJECTNAMEONLY%.manifest
if not exist "%DOTMANIFEST%" goto nomani
echo found: %DOTMANIFEST%

echo compiling %MANIFESTRC%
brcc32 "%MANIFESTRC%"
goto donemani
:nomanirc
echo Hint: %MANIFESTRC% not found, skipping
goto donemani
:nomani
echo Hint: %DOTMANIFEST% not found, skipping
goto donemani
:donemani

echo checking for icon.rc
set ICONRC=%PROJECTNAMEONLY%_Icon.rc
if not exist "%ICONRC%" goto noicon
echo found: %ICONRC%
echo compiling %ICONRC%
"%~dp0\rc" "%ICONRC%"
goto doneicon
:noicon
echo Hint: %ICONRC% not found, skipping
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

