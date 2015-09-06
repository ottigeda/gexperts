@rem builds a project using msbuild
@rem Copy this to the project's root directory and
@rem change the projectname below
setlocal
call ..\..\..\buildtools\InitForDelphi10Seattle.cmd"
msbuild /target:Build /p:config=Release GExpertsRS10Seattle.dproj
echo done building GExpertsRS10Seattle
endlocal
pause
