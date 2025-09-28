del dcu\*.dcu
call ..\..\images\_CreateGXIconsRc.cmd
..\..\buildtools\bdsproj2cfg GExpertsBDS2006.dpr --ver=4.0
call ..\..\buildtools\prebuild.cmd GExpertsBDS2006.dproj
