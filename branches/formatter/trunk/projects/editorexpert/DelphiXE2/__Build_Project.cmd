@rem builds a project using msbuild
@rem Copy this to the project's root directory and
@rem change the projectname below
setlocal
call ..\..\..\buildtools\InitForDelphiXE2.cmd"
msbuild GExpertsRSXE2.dproj
echo done building GExpertsRS2XE2
endlocal
pause
