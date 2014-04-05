rem @echo off
echo Initializing for Delphi 2007
call %~p0%delphiversions.cmd
set OldPath=%PATH%
set DelphiExe=%Delphi2007Dir%\bin\bds.exe
call "%Delphi2007Dir%\bin\rsvars.bat"
rem Delphi 2007 sets the FrameworkDir incorrectly on 64 bit Windows, so we fix this here
SET FrameworkDir=%SystemRoot%\Microsoft.NET\Framework\
SET PATH=%FrameworkDir%%FrameworkVersion%;%FrameworkSDKDir%;%OldPath%
