call :cleandir 2007
call :doclean
del GXIcons.res
pause
goto :eof

:cleandir
cd delphi%1
call :doclean
cd ..
goto :eof

:doclean
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