@rem builds a project using dcc32
call _prebuild.cmd
call ..\..\..\buildtools\delphiversions.cmd
"%Delphi2006Dir%\bin\dcc32.exe" GExpertsBDS2006.dpr
pause
