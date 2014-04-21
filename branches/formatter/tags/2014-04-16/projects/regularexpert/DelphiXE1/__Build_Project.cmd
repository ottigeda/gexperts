@rem builds a project using msbuild
@rem Copy this to the project's root directory and
@rem change the projectname below
setlocal
call ..\..\..\buildtools\InitForDelphiXE.cmd"
msbuild GExpertsRSXE1.dproj
echo done building GExpertsRSXE1
endlocal
pause
