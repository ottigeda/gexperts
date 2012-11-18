@rem builds a project using dcc32
call _prebuild.cmd
set units=..\..\..\gx\source\framework;..\..\..\gx\externalsource;..\..\..\gx\externalsource\abbrevia;..\..\..\gx\externalsource\UniSynEdit
set incs=..\..\..\gx\source\framework
set pkgs=vcl;rtl;vclx;designide
set defs=synedit
set out=..\..\..\editorexpert
set uout=..\..\..\dcu\delphi2005\editorexpert
"%ProgramFiles%\Borland\BDS\3.0\bin\dcc32.exe" -E%out% -N%uout% -D%defs% -I%incs% -U%units% -LU%pkgs% GExpertsDelphi2005.dpr
pause
