@rem Builds the Reverse Statement Unit Tests with Delphi 13
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
msbuild /target:rebuild /p:config=Release TestReverseStatement.dproj
if errorlevel 1 goto :buildfailed

echo.
echo Build successful.
goto :done

:nodelphi
echo ERROR: Delphi 13 not found. Check that c:\Delphi\13\bin\rsvars.bat exists.
goto :done

:buildfailed
echo.
echo ERROR: Build failed.
goto :done

:done
if "%NOWAIT%"=="0" pause
endlocal
