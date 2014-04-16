@rem builds a project using dcc32
setlocal
call _prebuild.cmd
call ..\..\..\buildtools\InitForDelphi6.cmd
"%DelphiPath%\bin\dcc32.exe" GExpertsD6.dpr
echo done building GExpertsD6
endlocal
pause
