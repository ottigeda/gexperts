@rem Build GExperts CodeLibrarian standalone tool
@setlocal enableextensions
@if not defined gx_cmd_debug (echo off)
@endlocal
setlocal

call "%~dp0..\doSelectDelphi.cmd"
if not defined delphiver goto :eof

pushd src
call "%~dp0..\doBuildStandAlone.cmd" CodeLibrarian
popd

echo.
echo Done.
pause
