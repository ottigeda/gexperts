@rem builds a project using msbuild
@rem Copy this to the project's root directory and
@rem change the projectname below
call ..\..\..\buildtools\InitForDelphiXE.cmd"
msbuild GExpertsRSXE1.dproj
pause
