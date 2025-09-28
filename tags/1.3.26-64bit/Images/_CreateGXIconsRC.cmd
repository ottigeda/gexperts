@setlocal
@if not defined gx_cmd_debug (echo off)
cd %~dp0
set GXIcons=GXIcons
@echo generating %GXIcons%.rc in %CD%
if exist %GXIcons%.rc del %GXIcons%.rc
@for %%I in (*.bmp) do echo %%~nI BITMAP "%%I">> %GXIcons%.rc
BRCC32 -r -32 %GXIcons%.rc
ren %GXIcons%.RES %GXIcons%.res
@endlocal
