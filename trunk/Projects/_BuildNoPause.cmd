@rem Builds all GExperts DLL versions without pausing after each build
@rem Usage: _BuildNoPause.cmd [version]
@setlocal enableextensions
@if not defined gx_cmd_debug (echo off)
@endlocal
cd /d "%~dp0"
call __build.cmd --nowait %*
