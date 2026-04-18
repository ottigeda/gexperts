@rem Searches the parent directories for the buildtools directory
@rem Sets the %buildtools% variable to the full path
call :FindInParents "%~dp0" buildtools
goto :eof

:FindInParents
setlocal
set parentdir=%~1
set subdir=%2
:loop
call :GetDir "%parentdir%"
set parentdir=%result%
if exist "%parentdir%\%subdir%" goto found
goto loop
:found
endlocal & set buildtools=%parentdir%\%subdir%
goto :eof

:GetDir
setlocal
set result=%~dp1
set result=%result:~0,-1%
endlocal & set result=%result%
goto :eof
