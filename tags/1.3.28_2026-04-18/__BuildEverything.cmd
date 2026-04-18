@rem Full GExperts release build: all DLLs, standalone tools, and installers
@setlocal enableextensions
@if not defined gx_cmd_debug (echo off)
@endlocal
setlocal

cd /d "%~dp0"

rem Read version from Projects\GExperts_version.ini
for /f "tokens=1,2 delims==" %%a in ('findstr /b /c:"MajorVer=" /c:"MinorVer=" /c:"Release=" "Projects\GExperts_version.ini"') do (
  if /i "%%a"=="MajorVer" set _major=%%b
  if /i "%%a"=="MinorVer" set _minor=%%b
  if /i "%%a"=="Release" set _release=%%b
)
set _version=%_major%.%_minor%.%_release%

echo.
echo ============================================================
echo  GExperts Full Release Build - Version %_version%
echo ============================================================
echo.
echo This script will:
echo  1. Build all GExperts DLLs (all Delphi versions)
echo  2. Build standalone tools included in the installer:
echo       CodeFormatter, DebugWindow, ExpertManager, Grep, PeInformation
echo  3. Build all installers
echo.
echo Select Delphi version for building the standalone tools:
echo.
call Projects\StandAlone\doSelectDelphi.cmd
if not defined delphiver (
  echo.
  echo Cancelled - no Delphi version selected.
  exit /b 1
)
echo.
echo Standalone tools will be built with Delphi %delphiver%.
echo.
set /p _confirm=Press Enter to start the build, or Ctrl+C to cancel...
echo.

rem === Step 1: Build all GExperts DLLs ===
title Step 1/3: Building GExperts DLLs
echo Step 1 of 3: Building all GExperts DLLs...
echo.
pushd Projects
call _BuildNoPause.cmd
set _err=%ERRORLEVEL%
popd
if %_err% neq 0 (
  echo.
  echo ERROR: GExperts DLL build failed.
  set /p _dummy=Press Enter to exit...
  exit /b 1
)
echo.

rem === Step 2: Build standalone tools ===
echo Step 2 of 3: Building standalone tools...
echo.
call :standalone CodeFormatter GExpertsFormatter
if errorlevel 1 exit /b 1
call :standalone DebugWindow GExpertsDebugWindow
if errorlevel 1 exit /b 1
call :standalone ExpertManager ExpertManager
if errorlevel 1 exit /b 1
call :standalone Grep GExpertsGrep
if errorlevel 1 exit /b 1
call :standalone PeInformation GExpertsPeInformation
if errorlevel 1 exit /b 1
echo.

rem === Step 3: Build installers ===
title Step 3/3: Building installers
echo Step 3 of 3: Building installers...
echo.
pushd BuildInstall
call MakeInstallers.cmd
set _err=%ERRORLEVEL%
popd
if %_err% neq 0 (
  echo.
  echo ERROR: Installer build failed.
  set /p _dummy=Press Enter to exit...
  exit /b 1
)

title Build complete
echo.
echo ============================================================
echo  Build complete! Installers are in BuildInstall\Output\
echo ============================================================
echo.
if not defined gx_no_success_pause pause
exit /b 0

:standalone
rem %1 = directory name under Projects\StandAlone\
rem %2 = project name prefix passed to doBuildStandAlone.cmd
setlocal
set _tool=%~1
set _prefix=%~2
title Step 2/3: Building %_prefix%
echo   Building %_prefix%...
pushd "Projects\StandAlone\%_tool%\src"
call "..\..\doBuildStandAlone.cmd" %_prefix%
set _err=%ERRORLEVEL%
popd
if %_err%==0 (endlocal & exit /b 0)
endlocal
echo.
echo ERROR: Failed to build %_prefix%.
set /p _dummy=Press Enter to exit...
exit /b 1
