set ZIPFile=GExperts-experimental-twm.zip

del %ZIPFile%
cd release
echo adding common files
zip -9 ..\%ZIPFile% *
cd ..

call :dozip editorexpert
call :dozip regularexpert

pause
goto :eof

:dozip
echo adding %1
zip -9 %ZIPFile% %1\GExpertsD6.dll
zip -9 %ZIPFile% %1\GExpertsD7.dll
zip -9 %ZIPFile% %1\GExpertsDelphi2005.dll
zip -9 %ZIPFile% %1\GExpertsBDS2006.dll
zip -9 %ZIPFile% %1\GExpertsDelphi2007.dll
zip -9 %ZIPFile% %1\GExpertsRS2009.dll
zip -9 %ZIPFile% %1\GExpertsRS2010.dll
zip -9 %ZIPFile% %1\GExpertsRSXE1.dll
zip -9 %ZIPFile% %1\GExpertsRSXE2.dll
zip -9 %ZIPFile% %1\GExpertsRSXE3.dll
zip -9 %ZIPFile% %1\GExpertsRSXE4.dll
goto :eof
