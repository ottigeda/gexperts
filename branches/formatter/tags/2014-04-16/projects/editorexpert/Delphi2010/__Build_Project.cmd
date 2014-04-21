@rem builds a project using msbuild
@rem Copy this to the project's root directory and
@rem change the projectname below
setlocal
call ..\..\..\buildtools\InitForDelphi2010.cmd
msbuild GExpertsRS2010.dproj
echo done building GExpertsRS2010
endlocal
pause
