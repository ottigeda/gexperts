del dcu\*.dcu
call ..\..\images\_CreateGXIconsRc.cmd
..\..\buildtools\dof2cfg GExpertsD6.dpr
call ..\..\buildtools\prebuild.cmd GExpertsD6.dpr
