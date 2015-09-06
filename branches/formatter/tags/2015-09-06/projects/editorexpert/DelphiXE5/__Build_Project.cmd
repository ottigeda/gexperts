@rem builds a project using msbuild
@rem Copy this to the project's root directory and
@rem change the projectname below
setlocal
call ..\..\..\buildtools\InitForDelphiXE5.cmd"
msbuild /target:Build /p:config=Release GExpertsRSXE5.dproj
echo done building GExpertsRSXE5
endlocal
pause
