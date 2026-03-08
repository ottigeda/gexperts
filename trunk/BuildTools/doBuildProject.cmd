@rem builds a project using dcc32
@setlocal enableextensions
@if not defined gx_cmd_debug (echo off)
@endlocal
setlocal
set buildtools=%~dp0%

@rem Pass GX_INSPECT_ENABLED define to compiler when requested.
@rem For MSBuild: DCC_Define env var is picked up via $(DCC_Define) chaining in .dproj files.
@rem For DCC32: -D flag is passed directly on the command line.
set GX_DCC_EXTRA=
if "%GX_INSPECT%"=="1" (
  set DCC_Define=GX_INSPECT_ENABLED
  set GX_DCC_EXTRA=-DGX_INSPECT_ENABLED
)

if exist _prebuild.cmd call _prebuild.cmd
if "%1" == "" goto detectver
echo %1
set delphiver=%1
goto continue
:detectver
call "%buildtools%doGetDelphiVer.cmd"
:continue
call "%buildtools%\doGetDelphiPath.cmd" %delphiver%

if "%delphiver%" == "6" goto dcc32
if "%delphiver%" == "7" goto dcc32
if "%delphiver%" == "2005" goto dcc32
if "%delphiver%" == "2006" goto dcc32

@rem build is done with msbuild
if "%delphiver%" == "2007" goto is2007
if "%delphiver%" == "Xx103" goto isRio

@rem Delphi 2009+ works fine in 64 bit Windows
call "%DelphiPath%\bin\rsvars.bat"
@rem determine the .dproj file
for %%a in (GExperts*.dproj) do set dprname=%%a
@rem build it
:dobuild
msbuild /target:rebuild /p:config=Release %dprname%
if errorlevel 1 goto :buildfailed
goto :done

:isRio
@rem On my machine the Installer created a rsvars.bat which points to the wrong
@rem directory for the dotNet framework: C:\Windows\Microsoft.NET\Framework\v4.5
@rem The actual installation is in: C:\WINDOWS\Microsoft.NET\Framework\v4.0.30319
@rem (This is because MS installed all dotNET updates after 4.0 to the existing directory.)
set OldPath=%PATH%
call "%DelphiPath%\bin\rsvars.bat"

@rem if msbuild.exe exists in that directory, everthing is fine
if exist %FrameworkDir%\msbuild.exe goto :dobuild

@rem The following could probably check the registriy as described in the link below,
@rem but I am too lazy right now to implement it. Maybe later. For now I simply check for it,
@rem where it is on my computer, otherwise we simply fail and let the user deal with it.
if not exist C:\WINDOWS\Microsoft.NET\Framework\v4.0.30319\msbuild.exe goto :wrongdn
@rem if it is there, change FrameworkDir and search path and call it.
SET FrameworkDir=C:\WINDOWS\Microsoft.NET\Framework\v4.0.30319
set PATH=%FrameworkDir%;%OldPath%
goto :dobuild

:wrongdn
echo dotNET %FrameworkVersion% not found on your computer. Check
echo https://docs.microsoft.com/en-us/dotnet/framework/migration-guide/how-to-determine-which-versions-are-installed#to-find-net-framework-versions-by-viewing-the-registry-net-framework-45-and-later
echo on how to find or install it.
pause
goto :done

:is2007
@rem Delphi 2007 sets the FrameworkDir incorrectly on 64 bit Windows, so we fix this here
set OldPath=%PATH%
call "%DelphiPath%\bin\rsvars.bat"
SET FrameworkDir=%SystemRoot%\Microsoft.NET\Framework\
SET PATH=%FrameworkDir%%FrameworkVersion%;%FrameworkSDKDir%;%OldPath%
if not exist %FrameworkDir%%FrameworkVersion%\Borland.Delphi.Targets goto 2007hint
set BDS=%DelphiPath:~0,-1%
rem EnvOptions.proj is only present if the IDE has been run at least once, which is not the case for non-interactive users.
rem Default EnvOptions.proj settings (note $(BDSUSERDIR) is not needed, as the build does not depend on user paths)
rem set Win32LibraryPath=$(BDS)\lib;$(BDSUSERDIR)\Imports;$(BDS)\Imports;$(BDS)\Lib\Indy10;$(BDSCOMMONDIR)\Dcp;$(BDS)\RaveReports\Lib
rem Adapted Win32LibraryPath because the Delphi 2007 msbuild DCC task does something odd: the first "$(BDS)\lib" is search/replaced wrongly into "$C:\Program Files....\lib"; quick fix is to add it twice
set Win32LibraryPath=$(BDS)\lib;$(BDS)\lib;$(BDS)\Imports;$(BDSCOMMONDIR)\Dcp
@rem determine the .dproj file
for %%a in (GExperts*.dproj) do set dprname=%%a
@rem build it
@rem note that for Delphi 2007 the target is rebuild and the configuration must be specified with
@rem /p:Configuration= instead of /p:Config= as in later versions.
:: if msbuild stuff fails, then add this parameter: -verbosity:diagnostic
msbuild /target:rebuild /p:Configuration=Release -p:DCC_Quiet=true %dprname%
if errorlevel 1 goto :buildfailed
goto :done

:2007hint
echo.
echo ERROR:
echo The Delphi 2007 installation is broken.
echo The file Borland.Delphi.Targets is missing from
echo %FrameworkDir%%FrameworkVersion%
echo.
echo This happens after every major Windows update.
echo.
echo You can fix this by copying the files from
echo %buildtools%dotNET2007
echo the the directory stated above.
echo.
echo See
echo https://blog.dummzeuch.de/2013/11/10/delphi-2007-on-windows-8-1/
echo for some more details.
pause
goto :eof

:dcc32
@rem determine the .dpr file
for %%a in (*.dpr) do set dprname=%%a
@rem build is done with dcc32, -B forces full rebuild (consistent with msbuild /target:rebuild)
"%DelphiPath%\bin\dcc32.exe" -B -$D- -$L- -$Y- -DRELEASE %GX_DCC_EXTRA% %dprname%
if errorlevel 1 goto :buildfailed
goto :done

:buildfailed
echo BUILD FAILED for %dprname%
endlocal
exit /b 1

:done
echo done building %dprname%
endlocal
