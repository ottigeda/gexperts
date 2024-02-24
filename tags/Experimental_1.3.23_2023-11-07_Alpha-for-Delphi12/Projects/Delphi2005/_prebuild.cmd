del dcu\*.dcu
call ..\..\images\_CreateGXIconsRc.cmd
..\..\buildtools\bdsproj2cfg GExpertsDelphi2005.dpr --ver=3.0
call ..\..\buildtools\prebuild.cmd GExpertsDelphi2005.dpr
