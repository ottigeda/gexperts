@setlocal enableextensions
@if not defined gx_cmd_debug (echo off)
@endlocal
@echo %0
@echo running in %CD%
@echo checking for %1
if exist %1 (
  echo moving %1 to %1~ so the DLL can be compiled even though it is currently loaded in the IDE
  move %1 %1~
) else (
  echo DLL %1 not found, does not need to be moved
)
set MAPFILE=%~dpn1.map
if exist %MAPFILE% (
  echo moving %MAPFILE% to %MAPFILE%~
  move %MAPFILE% %MAPFILE%~
)
set MAPFILE=
set MAPFILE=
echo %0 exiting
exit /b 0