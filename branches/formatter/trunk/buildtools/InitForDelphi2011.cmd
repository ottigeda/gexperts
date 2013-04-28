@rem init for Delphi 2011 (XE) batch compile
echo Initializing for Delphi XE
call %~p0%delphiversions.cmd
set DelphiExe=%DelphiXEDir%\bin\bds.exe
call "%DelphiXEDir%\bin\rsvars.bat"
