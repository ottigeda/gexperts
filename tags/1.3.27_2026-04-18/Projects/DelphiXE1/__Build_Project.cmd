@rem Build project by calling shared buildtools
@rem Usage: __Build_Project.cmd [--nowait] [--enable-gxinspect]
@setlocal enableextensions
@if not defined gx_cmd_debug (echo off)
@endlocal
setlocal

set NOWAIT=0
set GX_INSPECT=0
:parseargs
if "%~1"=="" goto endparse
if "%~1"=="--nowait" set NOWAIT=1
if "%~1"=="--enable-gxinspect" set GX_INSPECT=1
shift
goto parseargs
:endparse

call "%~dp0..\doFindBuildTools.cmd"
call "%buildtools%\doBuildProject.cmd"
set BUILDERROR=%ERRORLEVEL%

if "%NOWAIT%"=="0" pause
exit /b %BUILDERROR%
