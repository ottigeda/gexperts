@echo off

rem we no longer increment the build number, it's now set to the highest svn revision

set buildver=
set buildflags=
set BUILDNOWAIT=0
:parseargs
if "%~1"=="" goto :endparse
if "%~1"=="--nowait" (set BUILDNOWAIT=1 & set buildflags=%buildflags% --nowait) else if "%~1"=="--enable-gxinspect" (set buildflags=%buildflags% --enable-gxinspect) else (set buildver=%~1)
shift
goto :parseargs
:endparse

if not "%buildver%"=="" goto :buildone
goto :list

:buildone
call :dobuild %buildver%
goto :eof

:list
call :dobuild 6
if errorlevel 2 goto :eof
call :dobuild 7
if errorlevel 2 goto :eof
call :dobuild 2005
if errorlevel 2 goto :eof
call :dobuild 2006
if errorlevel 2 goto :eof
call :dobuild 2007
if errorlevel 2 goto :eof
call :dobuild 2009
if errorlevel 2 goto :eof
call :dobuild 2010
if errorlevel 2 goto :eof
call :dobuild XE1
if errorlevel 2 goto :eof
call :dobuild XE2
if errorlevel 2 goto :eof
call :dobuild XE3
if errorlevel 2 goto :eof
call :dobuild XE4
if errorlevel 2 goto :eof
call :dobuild XE5
if errorlevel 2 goto :eof
call :dobuild XE6
if errorlevel 2 goto :eof
call :dobuild XE7
if errorlevel 2 goto :eof
call :dobuild XE8
if errorlevel 2 goto :eof
call :dobuild Xx10Seattle
if errorlevel 2 goto :eof
call :dobuild Xx101Berlin
if errorlevel 2 goto :eof
call :dobuild Xx102Tokyo
if errorlevel 2 goto :eof
call :dobuild Xx103Rio
if errorlevel 2 goto :eof
call :dobuild Xx104Sydney
if errorlevel 2 goto :eof
call :dobuild Xx110Alexandria
if errorlevel 2 goto :eof
call :dobuild Xx120
if errorlevel 2 goto :eof
call :dobuild Xx130
if errorlevel 2 goto :eof
call :dobuild Xx130_64
if errorlevel 2 goto :eof

goto :eof

:dobuild
setlocal
set _ver=%1
:dobuild_retry
pushd Delphi%_ver%
call __Build_Project.cmd %buildflags%
set BUILDERROR=%ERRORLEVEL%
popd
if %BUILDERROR%==0 (endlocal & exit /b 0)
if "%BUILDNOWAIT%"=="1" (endlocal & exit /b 0)
echo.
echo Build failed for Delphi %_ver%.
set "_choice=R"
set /p "_choice=[A]bort  [R]etry (default)  [C]ontinue with next version? "
if /i "%_choice%"=="A" (endlocal & exit /b 2)
if /i "%_choice%"=="C" (endlocal & exit /b 0)
goto :dobuild_retry
