@rem builds a project using msbuild
@rem Copy this to the project's root directory and
@rem change the projectname below
setlocal
call ..\..\..\buildtools\InitForDelphiXE7.cmd"
msbuild /target:Build /p:config=Release GExpertsRSXE7.dproj
echo done building GExpertsRSXE7
endlocal
pause
