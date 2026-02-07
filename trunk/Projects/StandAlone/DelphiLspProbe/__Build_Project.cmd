@rem Build DelphiLspProbe standalone tool
@rem Requires Delphi 10.2+ (uses System.JSON)
@setlocal enableextensions
@if not defined gx_cmd_debug (echo off)
@endlocal
setlocal

call "%~dp0..\doSelectDelphi.cmd"
if not defined delphiver goto :eof

pushd src
call "%~dp0..\doBuildStandAlone.cmd" DelphiLspProbe
popd

echo.
echo Done.
pause
