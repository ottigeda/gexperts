@rem init for Delphi 2010 batch compile
echo Initializing for Delphi 2010
call %~dp0\delphiversions.cmd
set DelphiExe=%Delphi2010Dir%\bin\bds.exe
call "%Delphi2010Dir%\bin\rsvars.bat"
