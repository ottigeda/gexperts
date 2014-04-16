@rem builds a project using dcc32
setlocal
call _prebuild.cmd
call ..\..\..\buildtools\InitForDelphi7.cmd
"%DelphiPath%\bin\dcc32.exe" GExpertsD7.dpr
echo done building GExpertsD7
endlocal
pause
