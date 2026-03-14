@rem Builds the PasParser Unit Tests with Delphi 13
@rem Usage: _BuildProject.cmd [--nowait]
@setlocal enableextensions
@if not defined gx_cmd_debug (echo off)
@endlocal
setlocal

set NOWAIT=0
if "%~1"=="--nowait" set NOWAIT=1

call c:\Delphi\13\bin\rsvars.bat
if errorlevel 1 goto :nodelphi

cd /d "%~dp0src"
msbuild /target:rebuild /p:config=Release TestPasParser.dproj
if errorlevel 1 goto :buildfailed

echo.
echo Build successful.
goto :done

:nodelphi
echo ERROR: Delphi 13 not found. Check that c:\Delphi\13\bin\rsvars.bat exists.
exit /b 1

:buildfailed
echo.
echo ERROR: Build failed.
exit /b 1

:done
if "%NOWAIT%"=="0" pause
endlocal
