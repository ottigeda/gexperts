@rem Open project in Delphi IDE
@echo off
setlocal
set buildtools=%~dp0%
call %buildtools%doGetDelphiVer.cmd
call _prebuild.cmd
call %buildtools%\delphipath.cmd %delphiver%
start "Delphi %delphiver% IDE" "%DelphiPath%\bin\delphi32.exe" GExpertsD%delphiver%.dpr
endlocal
goto :eof
