@rem builds a project using dcc32
setlocal
call _prebuild.cmd
call ..\..\..\buildtools\delphiversions.cmd
"%Delphi6Dir%\bin\dcc32.exe" GExpertsD6.dpr
echo done building GExpertsD6
endlocal
pause
