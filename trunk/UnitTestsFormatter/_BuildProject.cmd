@rem Builds the Formatter Unit Tests with Delphi 13
@setlocal enableextensions
@if not defined gx_cmd_debug (echo off)
@endlocal
setlocal

call c:\Delphi\13\bin\rsvars.bat
if errorlevel 1 goto :nodelphi

cd /d "%~dp0source"
msbuild /target:rebuild /p:config=Release GX_FormatterUnitTests.13.dproj
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
endlocal
pause
