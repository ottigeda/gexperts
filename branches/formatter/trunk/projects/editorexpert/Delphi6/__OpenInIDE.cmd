@rem Searches the parent dirctories for the buildtools and calls the doOpenInIde.cmd there
@echo off
setlocal
call :GetBuildTools %0%
call %result%\doOpenInIde.cmd
goto :eof

:GetBuildTools
@rem search all parent directories for a subdir "buildtools" and return
@rem the full path to the buildtools directory in %result%
setlocal
set parentdir=%1%
:loop
call :GetDir %parentdir%
set parentdir=%result%
if exist %parentdir%\buildtools goto found
goto loop
:found
set result=%parentdir%\buildtools
endlocal & set result=%result%
goto :eof

:GetDir
rem extract path
setlocal
set result=%~dp1%
rem remove any quotes
set result=%result:"=%
rem add quotes
set result="%result%"
rem remove \ before the closing quote
set result=%result:\"="%
rem remove any quotes
set result=%result:"=%
endlocal & set result=%result%
goto :eof
