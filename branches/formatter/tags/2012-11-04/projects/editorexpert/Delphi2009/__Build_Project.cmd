@rem builds a project using msbuild
@rem Copy this to the project's root directory and
@rem change the projectname below
call ..\..\..\buildtools\InitForDelphi2009.cmd"
msbuild GExpertsRS2009.dproj
pause
