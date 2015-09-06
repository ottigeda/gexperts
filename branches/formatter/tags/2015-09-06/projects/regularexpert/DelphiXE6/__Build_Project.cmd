@rem builds a project using msbuild
@rem Copy this to the project's root directory and
@rem change the projectname below
setlocal
call ..\..\..\buildtools\InitForDelphiXE6.cmd"
msbuild /target:Build /p:config=Release GExpertsRSXE6.dproj
echo done building GExpertsRSXE6
endlocal
pause
