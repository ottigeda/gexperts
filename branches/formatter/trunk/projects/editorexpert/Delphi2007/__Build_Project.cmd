@rem builds a project using msbuild
@rem Copy this to the project's root directory and
@rem change the projectname below
call ..\..\..\buildtools\InitForDelphi2007.cmd"
msbuild GExpertsDelphi2007.dproj
pause
