@rem Detect installed Delphi versions and let user select one
@rem Sets %delphiver% to the selected version (e.g., 2007, Xx102, Xx120, Xx130)
@rem Sets %delphigroup% to the format group (2007 or 10)
@setlocal enableextensions enabledelayedexpansion
@if not defined gx_cmd_debug (echo off)
@endlocal
setlocal enabledelayedexpansion

set count=0
set latest=

rem Check for Delphi 2007
call :CheckVersion "Borland\BDS\5.0" 2007 2007
rem Check for Delphi 10.2 Tokyo
call :CheckVersion "Embarcadero\BDS\19.0" Xx102 10
rem Check for Delphi 10.3 Rio
call :CheckVersion "Embarcadero\BDS\20.0" Xx103 10
rem Check for Delphi 10.4 Sydney
call :CheckVersion "Embarcadero\BDS\21.0" Xx104 10
rem Check for Delphi 11 Alexandria
call :CheckVersion "Embarcadero\BDS\22.0" Xx110 11
rem Check for Delphi 12
call :CheckVersion "Embarcadero\BDS\23.0" Xx120 11
rem Check for Delphi 13
call :CheckVersion "Embarcadero\BDS\37.0" Xx130 11

if %count%==0 (
  echo No supported Delphi version found.
  echo Supported versions: 2007, 10.2, 10.3, 10.4, 11, 12, 13
  pause
  goto :eof
)

if %count%==1 (
  echo Found one Delphi version: %latest%
  set delphiver=%latest%
  set delphigroup=%latestgroup%
  goto :done
)

echo.
echo Found %count% Delphi versions:
echo.
set choice=0
if defined ver1 (
  set /a choice+=1
  echo   !choice!. Delphi %name1%
)
if defined ver2 (
  set /a choice+=1
  echo   !choice!. Delphi %name2%
)
if defined ver3 (
  set /a choice+=1
  echo   !choice!. Delphi %name3%
)
if defined ver4 (
  set /a choice+=1
  echo   !choice!. Delphi %name4%
)
if defined ver5 (
  set /a choice+=1
  echo   !choice!. Delphi %name5%
)
if defined ver6 (
  set /a choice+=1
  echo   !choice!. Delphi %name6%
)
if defined ver7 (
  set /a choice+=1
  echo   !choice!. Delphi %name7%
)
echo.
echo   0. Cancel
echo.
set /p selection="Select Delphi version [%count%]: "
if "%selection%"=="" set selection=%count%
if "%selection%"=="0" goto :eof

if "%selection%"=="1" if defined ver1 (
  set delphiver=%ver1%
  set delphigroup=%group1%
  goto :done
)
if "%selection%"=="2" if defined ver2 (
  set delphiver=%ver2%
  set delphigroup=%group2%
  goto :done
)
if "%selection%"=="3" if defined ver3 (
  set delphiver=%ver3%
  set delphigroup=%group3%
  goto :done
)
if "%selection%"=="4" if defined ver4 (
  set delphiver=%ver4%
  set delphigroup=%group4%
  goto :done
)
if "%selection%"=="5" if defined ver5 (
  set delphiver=%ver5%
  set delphigroup=%group5%
  goto :done
)
if "%selection%"=="6" if defined ver6 (
  set delphiver=%ver6%
  set delphigroup=%group6%
  goto :done
)
if "%selection%"=="7" if defined ver7 (
  set delphiver=%ver7%
  set delphigroup=%group7%
  goto :done
)

echo Invalid selection.
goto :eof

:done
echo.
echo Using Delphi %delphiver% (format group: %delphigroup%)
endlocal & set delphiver=%delphiver%& set delphigroup=%delphigroup%
goto :eof

:CheckVersion
rem %1 = registry key, %2 = version identifier, %3 = format group
set regkey=%~1
set ver=%~2
set grp=%~3
call :CheckRegistry "%regkey%"
if not defined regresult goto :eof
set /a count+=1
set ver%count%=%ver%
set group%count%=%grp%
rem Set display name
if "%ver%"=="2007" set name%count%=2007
if "%ver%"=="Xx102" set name%count%=10.2 Tokyo
if "%ver%"=="Xx103" set name%count%=10.3 Rio
if "%ver%"=="Xx104" set name%count%=10.4 Sydney
if "%ver%"=="Xx110" set name%count%=11 Alexandria
if "%ver%"=="Xx120" set name%count%=12
if "%ver%"=="Xx130" set name%count%=13
set latest=%ver%
set latestgroup=%grp%
goto :eof

:CheckRegistry
rem Check if registry key exists
set regresult=
FOR /F "usebackq skip=2 tokens=3,*" %%A IN (`REG QUERY "HKLM\SOFTWARE\%~1" /v RootDir 2^>nul`) DO (
  set regresult=%%A
  goto :eof
)
rem Try Wow6432Node for 64-bit Windows
FOR /F "usebackq skip=2 tokens=3,*" %%A IN (`REG QUERY "HKLM\SOFTWARE\Wow6432Node\%~1" /v RootDir 2^>nul`) DO (
  set regresult=%%A
  goto :eof
)
goto :eof
