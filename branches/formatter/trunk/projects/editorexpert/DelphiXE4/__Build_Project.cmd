@rem builds a project using msbuild
@rem Copy this to the project's root directory and
@rem change the projectname below
setlocal
call ..\..\..\buildtools\InitForDelphiXE4.cmd"
msbuild /target:Build /p:config=Release GExpertsRSXE4.dproj
echo done building GExpertsRSXE4
endlocal
pause
