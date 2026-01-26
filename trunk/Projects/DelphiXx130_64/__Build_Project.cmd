@rem Build project by calling shared buildtools
@setlocal enableextensions
@if not defined gx_cmd_debug (echo off)
@endlocal
setlocal
call "%~dp0..\doFindBuildTools.cmd"
call "%buildtools%\doBuildProject.cmd"
pause
