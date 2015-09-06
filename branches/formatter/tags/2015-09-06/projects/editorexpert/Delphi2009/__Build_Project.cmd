@rem builds a project using msbuild
@rem Copy this to the project's root directory and
@rem change the projectname below
setlocal
call ..\..\..\buildtools\InitForDelphi2009.cmd"
msbuild /target:Build /p:config=Release GExpertsRS2009.dproj
echo done building GExpertsRS2009
endlocal
pause
