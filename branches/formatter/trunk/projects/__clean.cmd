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
call :doDir editorexpert %1
call :doDir regularexpert %1
call :doDir standalone %1

goto :eof

:doDir
if not exist %1 goto :eof
pushd %1
call __clean.cmd %2
popd

goto :eof
