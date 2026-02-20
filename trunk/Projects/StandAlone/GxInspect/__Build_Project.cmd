@rem Build GxInspect standalone tool
@rem Usage: __Build_Project.cmd [--nowait]
@setlocal enableextensions
@if not defined gx_cmd_debug (echo off)
@endlocal
setlocal

if "%~1"=="--nowait" set NOWAIT=1

call "%~dp0..\doSelectDelphi.cmd"
if not defined delphiver goto :eof

pushd src
call "%~dp0..\doBuildStandAlone.cmd" GxInspect
popd

echo.
echo Done.
if not defined NOWAIT pause
