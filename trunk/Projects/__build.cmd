@echo off

rem we no longer increment the build number, it's now set to the highest svn revision

set buildver=
set buildflags=
:parseargs
if "%~1"=="" goto :endparse
if "%~1"=="--nowait" (set buildflags=--nowait) else (set buildver=%~1)
shift
goto :parseargs
:endparse

if not "%buildver%"=="" goto :buildone
goto :list

:buildone
call :dobuild %buildver%
goto :eof

:list
call :dobuild 6
call :dobuild 7
call :dobuild 2005
call :dobuild 2006
call :dobuild 2007
call :dobuild 2009
call :dobuild 2010
call :dobuild XE1
call :dobuild XE2
call :dobuild XE3
call :dobuild XE4
call :dobuild XE5
call :dobuild XE6
call :dobuild XE7
call :dobuild XE8
call :dobuild Xx10Seattle
call :dobuild Xx101Berlin
call :dobuild Xx102Tokyo
call :dobuild Xx103Rio
call :dobuild Xx104Sydney
call :dobuild Xx110Alexandria
call :dobuild Xx120
call :dobuild Xx130
call :dobuild Xx130_64

goto :eof

:dobuild
pushd Delphi%1
setlocal
call __Build_Project.cmd %buildflags%
endlocal
popd
goto :eof
