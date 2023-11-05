del dcu\*.dcu
call ..\..\images\_CreateGXIconsRc.cmd
..\..\buildtools\dof2cfg GExpertsD7.dpr
call ..\..\buildtools\prebuild.cmd GExpertsD7.dpr
