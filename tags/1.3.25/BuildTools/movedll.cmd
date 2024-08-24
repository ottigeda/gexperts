@setlocal enableextensions
@if not defined gx_cmd_debug (echo off)
@endlocal
@echo %0
@echo running in %CD%
@echo checking for %1
if not exist %1 goto skipit
echo moving %1 to %1~ so the DLL can be compiled even though it is currently loaded in the IDE
move %1 %1~
goto :done
:skipit
echo DLL not found, does not need to be moved
:done
echo %0 exiting
exit /b 0