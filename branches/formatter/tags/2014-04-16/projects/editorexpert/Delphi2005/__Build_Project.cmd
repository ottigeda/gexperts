@rem builds a project using dcc32
setlocal
call _prebuild.cmd
call ..\..\..\buildtools\InitForDelphi2005.cmd
"%DelphiPath%\bin\dcc32.exe" GExpertsDelphi2005.dpr
echo done building GExpertsDelphi2005
endlocal
pause
