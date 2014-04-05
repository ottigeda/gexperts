@rem builds a project using dcc32
call _prebuild.cmd
call ..\..\..\buildtools\delphiversions.cmd
"%Delphi2005Dir%\bin\dcc32.exe" GExpertsDelphi2005.dpr
pause
