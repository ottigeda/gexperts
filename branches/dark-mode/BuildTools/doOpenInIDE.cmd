@rem Open project in Delphi IDE
@setlocal enableextensions
@if not defined gx_cmd_debug (echo off)
@endlocal
setlocal
set buildtools=%~dp0%
if "%1" == "" goto detectver
echo %1
set delphiver=%1
goto continue
:detectver
call "%buildtools%doGetDelphiVer.cmd"
:continue
call "%buildtools%\doGetDelphiPath.cmd" %delphiver%
if %delphiver% LSS 8 goto delphi32
set delphiexe=bds.exe
if "%delphiver%" == "2007" goto is2007
goto :bds
:is2007
SET FrameworkDir=%SystemRoot%\Microsoft.NET\Framework\
set FrameworkVersion=v2.0.50727
if exist %FrameworkDir%%FrameworkVersion%\Borland.Delphi.Targets goto bds
echo.
echo ERROR:
echo Your Delphi 2007 installation is broken.
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

goto :bds
:delphi32
set delphiexe=delphi32.exe
:bds
for %%a in (*.dpr) do set dprname=%%a
start "Delphi %delphiver% IDE" "%DelphiPath%\bin\%delphiexe%" %dprname%
endlocal
goto :eof
