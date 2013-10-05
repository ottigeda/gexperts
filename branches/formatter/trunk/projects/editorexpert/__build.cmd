if "%1"=="" goto :list
call :dobuild %1
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

goto :eof

:dobuild
pushd Delphi%1
call __Build_Project.cmd
popd
goto :eof
