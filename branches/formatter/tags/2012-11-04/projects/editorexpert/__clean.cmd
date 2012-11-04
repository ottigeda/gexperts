call :cleandir 6
call :cleandir 7
call :cleandir 2005
call :cleandir 2006
call :cleandir 2007
call :cleandir 2009
call :cleandir 2010
call :cleandir XE1
call :cleandir XE2
call :cleandir XE3
goto :eof

:cleandir
pushd delphi%1
call :doclean
popd
if not exist ..\..\dcu\Delphi%1\editorexpert\*.dcu goto :eof
del ..\..\dcu\Delphi%1\editorexpert\*.dcu
goto :eof

:doclean
call :delfile GXIcons.res
call :delfile *.~*
call :delfile *.local
call :delfile *.cfg
call :delfile *.identcache
call :delfile *.dsk
call :deltree __history
call :deltree ModelSupport
goto :eof

:delfile
if exist %1 del %1
goto :eof

:deltree
if not exist %1\* goto :eof
del /s /q %1\*
rd %1
goto :eof
