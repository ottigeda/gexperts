@rem init for Delphi XE5 batch compile
echo Initializing for Delphi XE5
call %~p0%delphiversions.cmd
set DelphiExe=%DelphiXE5Dir%\bin\bds.exe
call "%DelphiXE5Dir%\bin\rsvars.bat"
echo rsvars turns echo off, turn them back on:
@echo on
