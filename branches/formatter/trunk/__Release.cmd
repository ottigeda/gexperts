@echo off
set ZIPFile=GExperts-experimental-twm.zip
set ZIPEXE=buildtools\7z a -tzip

if exist %ZIPFile% del %ZIPFile%
pushd release
echo adding common files
..\%ZIPEXE% ..\%ZIPFile% *
popd

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
call :dozip editorexpert %1
call :dozip regularexpert %1

pause
goto :eof

:dozip
echo adding %1 %2

if %2==6     %ZIPEXE% %ZIPFile% %1\GExpertsD6.dll
if %2==7     %ZIPEXE% %ZIPFile% %1\GExpertsD7.dll
if %2==2005  %ZIPEXE% %ZIPFile% %1\GExpertsDelphi2005.dll
if %2==2006  %ZIPEXE% %ZIPFile% %1\GExpertsBDS2006.dll
if %2==2007  %ZIPEXE% %ZIPFile% %1\GExpertsDelphi2007.dll
if %2==2009  %ZIPEXE% %ZIPFile% %1\GExpertsRS2009.dll
if %2==2010  %ZIPEXE% %ZIPFile% %1\GExpertsRS2010.dll
if %2==XE1   %ZIPEXE% %ZIPFile% %1\GExpertsRSXE1.dll
if %2==XE2   %ZIPEXE% %ZIPFile% %1\GExpertsRSXE2.dll
if %2==XE3   %ZIPEXE% %ZIPFile% %1\GExpertsRSXE3.dll
if %2==XE4   %ZIPEXE% %ZIPFile% %1\GExpertsRSXE4.dll
if %2==XE5   %ZIPEXE% %ZIPFile% %1\GExpertsRSXE5.dll
pushd install
..\%ZIPEXE% ..\%ZIPFile% *-%2*.*
popd

goto :eof
