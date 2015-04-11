@rem builds a project using msbuild
@rem Copy this to the project's root directory and
@rem change the projectname below
setlocal
call ..\..\..\buildtools\InitForDelphiXE8.cmd"
msbuild /target:Build /p:config=Release GExpertsRSXE8.dproj
echo done building GExpertsRSXE8
endlocal
pause
