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
call :doItem XE6
call :doItem XE7
call :doItem XE8
call :doEdtr Xx10Seattle

goto :eof

:doItem
call :doEdtr %1
call :doRglr %1
got :eof

:doEdtr
pushd projects\editorexpert
setlocal
call __build.cmd %1
endlocal
popd

:doRglr
pushd projects\regularexpert
setlocal
call __build.cmd %1
endlocal
popd

goto :eof
