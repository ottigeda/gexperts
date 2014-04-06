@rem init for Delphi XE4 batch compile
echo Initializing for Delphi XE4
call %~dp0\delphiversions.cmd
set DelphiExe=%DelphiXE4Dir%\bin\bds.exe
call "%DelphiXE4Dir%\bin\rsvars.bat"
