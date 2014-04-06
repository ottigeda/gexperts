@rem Init for Delphi 2009 batch compile
echo Initializing for Delphi 2009
call %~dp0\delphiversions.cmd
set DelphiExe=%Delphi2009Dir%\bin\bds.exe
call "%Delphi2009Dir%\bin\rsvars.bat"
