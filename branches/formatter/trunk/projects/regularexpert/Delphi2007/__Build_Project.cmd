@rem builds a project using msbuild
@rem Copy this to the project's root directory and
@rem change the projectname below
setlocal
call ..\..\..\buildtools\InitForDelphi2007.cmd"
msbuild /target:Build /p:config=Release GExpertsDelphi2007.dproj
echo done building GExpertsDelphi2007
endlocal
pause
