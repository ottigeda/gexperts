set ZIPFile=GExperts-experimental-twm.zip

del %ZIPFile%
cd release
echo adding common files
zip -9 ..\%ZIPFile% *
cd ..

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

if %2==6     zip -9 %ZIPFile% %1\GExpertsD6.dll
if %2==7     zip -9 %ZIPFile% %1\GExpertsD7.dll
if %2==2005  zip -9 %ZIPFile% %1\GExpertsDelphi2005.dll
if %2==2006  zip -9 %ZIPFile% %1\GExpertsBDS2006.dll
if %2==2007  zip -9 %ZIPFile% %1\GExpertsDelphi2007.dll
if %2==2009  zip -9 %ZIPFile% %1\GExpertsRS2009.dll
if %2==2010  zip -9 %ZIPFile% %1\GExpertsRS2010.dll
if %2==XE1   zip -9 %ZIPFile% %1\GExpertsRSXE1.dll
if %2==XE2   zip -9 %ZIPFile% %1\GExpertsRSXE2.dll
if %2==XE3   zip -9 %ZIPFile% %1\GExpertsRSXE3.dll
if %2==XE4   zip -9 %ZIPFile% %1\GExpertsRSXE4.dll
if %2==XE5   zip -9 %ZIPFile% %1\GExpertsRSXE5.dll
pushd install
zip -9 ..\%ZIPFile% *-%2*.*
popd

goto :eof
