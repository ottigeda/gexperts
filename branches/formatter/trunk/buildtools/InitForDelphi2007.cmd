rem @echo off
echo Initializing for Delphi 2007
call %~p0%delphiversions.cmd
set DelphiExe=%Delphi2007Dir%\bin\bds.exe
call "%Delphi2007Dir%\bin\rsvars.bat"
