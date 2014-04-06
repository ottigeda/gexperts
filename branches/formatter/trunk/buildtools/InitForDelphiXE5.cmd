@rem init for Delphi XE5 batch compile
echo Initializing for Delphi XE5
call %~dp0\delphiversions.cmd
set DelphiExe=%DelphiXE5Dir%\bin\bds.exe
call "%DelphiXE5Dir%\bin\rsvars.bat"
