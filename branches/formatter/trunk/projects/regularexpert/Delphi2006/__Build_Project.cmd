@rem builds a project using dcc32
setlocal
call _prebuild.cmd
call ..\..\..\buildtools\delphiversions.cmd
"%Delphi2006Dir%\bin\dcc32.exe" GExpertsBDS2006.dpr
echo done building GExpertsBDS2006
endlocal
pause
