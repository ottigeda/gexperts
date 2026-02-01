@setlocal enableextensions
@if not defined gx_cmd_debug (echo off)
@endlocal
set iscc="C:\Program Files (x86)\Inno Setup 5\ISCC.exe"

if not exist %iscc% goto ISerror

call :readver
rem echo %result%
set version=%result%

rem set instver=13D
set instver=%version%
call :getdate
set special=%dateYYYYMMDD%

call :makeinst RS130_64 RS13_64
pause
goto :eof

:makeinst
set Delphi=%1
set Prefix=%2
if "%prefix%" == "" set Prefix=%Delphi%
%iscc% GExperts.iss /d%Delphi% /dVersion=%version% /fGExperts_%Prefix%_%instver%_%special%
goto :eof

:readver
rem taken from https://stackoverflow.com/a/2866328/49925 but heavily modified
setlocal enableextensions enabledelayedexpansion
set IniFile=..\projects\GExperts_version.ini
set MajorVer=unknown
set MinorVer=unknown
set Release=unknown
set CurrSection=
for /f "usebackq delims=" %%a in ("!IniFile!") do (
    set ln=%%a

    if "x!ln:~0,1!"=="x[" (
        set CurrSection=!ln!
    ) else (
        for /f "tokens=1,2 delims==" %%b in ("!ln!") do (
            set currkey=%%b
            set currval=%%c
            if "x!CurrSection!"=="x[Version Info]" (
				if "x!currkey!"=="x!MajorVer" (
					rem echo MajorVer: !currval!
					set MajorVer=!currval!
				) else if "x!currkey!"=="x!MinorVer" (
					rem echo MinorVer: !currval!
					set MinorVer=!currval!
				) else if "x!currkey!"=="x!Release" (
					rem echo Release: !currval!
					set Release=!currval!
				) else (
					rem echo other: !currkey!=!currval!
				)
            )
        )
    )
)

endlocal & set result=%MajorVer%.%MinorVer%.%Release%
goto :eof

:getdate
rem taken from http://stackoverflow.com/a/16349176/49925
:: Check WMIC is available
WMIC.EXE Alias /? >NUL 2>&1 || GOTO s_error

:: Use WMIC to retrieve date and time
FOR /F "skip=1 tokens=1-6" %%G IN ('WMIC Path Win32_LocalTime Get Day^,Hour^,Minute^,Month^,Second^,Year /Format:table') DO (
   IF "%%~L"=="" goto s_done
      Set _yyyy=%%L
      Set _mm=00%%J
      Set _dd=00%%G
      Set _hour=00%%H
      SET _minute=00%%I
      SET _second=00%%K
)
:s_done

:: Pad digits with leading zeros
      Set _mm=%_mm:~-2%
      Set _dd=%_dd:~-2%
      Set _hour=%_hour:~-2%
      Set _minute=%_minute:~-2%
      Set _second=%_second:~-2%

Set dateYYYYMMDD=%_yyyy%-%_mm%-%_dd%
set timeHHMMSS=hour%_%_minute%_%_second%
Set logtimestamp=%dateYYYYMMDD%_%timeHHMMSS%
goto :eof

:s_error
echo WMIC is not available, cannot deterimine date and time
goto :eof
pause


:ISerror
echo InnoSetup compiler not found:
echo %iscc%
echo Install InnnoSetup or correct the setting for iscc at the start of the script!
pause
goto :eof
