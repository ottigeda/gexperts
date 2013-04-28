@rem init for Delphi XE3 batch compile
echo Initializing for Delphi XE3
call %~p0%delphiversions.cmd
set DelphiExe=%DelphiXE3Dir%\bin\bds.exe
call "%DelphiXE3Dir%\bin\rsvars.bat"
