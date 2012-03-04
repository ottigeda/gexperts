call :cleandir 2009
call :cleandir 2010
call :cleandir 2011
call :deltree __history
call :deltree ModelSupport
goto :eof

:cleandir
pushd delphi%1
call :doclean
popd
if not exist ..\..\dcu\Delphi%1\standalone\*.dcu goto :eof
del ..\..\dcu\Delphi%1\standalone\*.dcu
goto :eof

:doclean
call :delfile *.~*
call :delfile *.local
call :delfile *.identcache
call :delfile *.cfg
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
