@rem Open project in Delphi IDE
setlocal
@echo off
call :GetLastDir %CD%
set thisdir=%result%
set delphiver=%thisdir:~6%
call _prebuild.cmd
call ..\..\..\buildtools\InitForDelphi%delphiver%.cmd
start "Delphi %delphiver% IDE" "%DelphiPath%\bin\delphi32.exe" GExpertsD%delphiver%.dpr
goto :eof

:GetLastDir
rem extract path
setlocal
set directory=%1%
rem extract "filename" (= last directory of path)
call :LastItem %directory%
endlocal & set result=%result%
goto :eof

:LastItem
set result=%~n1%
goto :eof
