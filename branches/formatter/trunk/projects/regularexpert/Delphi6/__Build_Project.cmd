@rem builds a project using dcc32
call _prebuild.cmd
call ..\..\..\buildtools\delphiversions.cmd
"%Delphi6Dir%\bin\dcc32.exe" GExpertsD6.dpr
pause
