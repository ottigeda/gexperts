@rem init for Delphi XE2 batch compile
echo Initializing for Delphi XE2
call %~dp0\delphiversions.cmd
set DelphiExe=%DelphiXE2Dir%\bin\bds.exe
call "%DelphiXE2Dir%\bin\rsvars.bat"
