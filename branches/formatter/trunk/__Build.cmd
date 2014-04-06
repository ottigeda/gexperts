@echo off
if "%1"=="" goto :list
call :doItem %1
goto :eof
:list

call :doItem 6
call :doItem 7
call :doItem 2005
call :doItem 2006
call :doItem 2007
call :doItem 2009
call :doItem 2010
call :doItem XE1
call :doItem XE2
call :doItem XE3
call :doItem XE4
call :doItem XE5

goto :eof

:doItem
pushd projects\editorexpert
setlocal
call __build.cmd %1
endlocal
popd

pushd projects\regularexpert
setlocal
call __build.cmd %1
endlocal
popd

goto :eof
