@rem Build project by calling shared buildtools
@rem Usage: __Build_Project.cmd [--nowait]
@setlocal enableextensions
@if not defined gx_cmd_debug (echo off)
@endlocal
setlocal

set NOWAIT=0
if "%~1"=="--nowait" set NOWAIT=1

call "%~dp0..\doFindBuildTools.cmd"
call "%buildtools%\doBuildProject.cmd"

if "%NOWAIT%"=="0" pause
