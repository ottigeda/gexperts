@rem builds a project using dcc32
call _prebuild.cmd
call ..\..\..\buildtools\delphiversions.cmd
"%Delphi7Dir%\bin\dcc32.exe" GExpertsD7.dpr
pause
