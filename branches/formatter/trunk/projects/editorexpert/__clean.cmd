call :cleandir 6
call :cleandir 7
call :cleandir 2005
call :cleandir 2006
call :cleandir 2007
call :cleandir 2009
call :cleandir 2010
call :cleandir 2011
goto :eof

:cleandir
cd delphi%1
call :doclean
cd ..
del ..\..\dcu\Delphi%1\editorexpert\*.dcu
goto :eof

:doclean
del GXIcons.res
del GExperts*.~*
del GExperts*.bdsproj.local
del GExperts*.dproj.local
del GExperts*.cfg
del GExperts*.identcache
del GExperts*.dsk
del /s /q __history\*
rd __history
del /s /q ModelSupport\*
rd ModelSupport
goto :eof