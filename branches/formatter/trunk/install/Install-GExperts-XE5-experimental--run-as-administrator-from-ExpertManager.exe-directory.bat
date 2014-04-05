:: check if user is administrator

  "%SystemRoot%\system32\cacls.exe" "%SystemRoot%\system32\config\system" 1>nul 2>&1  && (goto :isAdmin)
:isNoAdmin
  echo you need to be Administrator, and (when under Vista or higher) run this using using UAC
  goto :exit

:isAdmin

:: Gets the right link for x86 (32-bit) program files
IF /I %PROCESSOR_ARCHITECTURE% == amd64 goto :x64
IF /I %PROCESSOR_ARCHITEW6432% == amd64 goto :x64
goto :x86
:x64
   :: OS is 64bit
   set ProgramFilesX86=%ProgramFiles(x86)%
  goto :continue
:x86
   :: OS is 32bit
  set ProgramFilesX86=%ProgramFiles%
  goto :continue
 
:continue

:: create the right directory and copy the files

  setlocal

  set TargetDirectory=%ProgramFilesX86%\GExperts for RAD Studio XE5

  mkdir "%TargetDirectory%"
::  set FilesToCopy=DbugIntf.pas ExpertManager.exe GExperts.chm GExpertsDebugWindow.exe GExpertsGrep.exe regularexpert\GExpertsRSXE5.dll Readme.txt preview.pas *.ini
  set FilesToCopy=GExperts.chm regularexpert\GExpertsRSXE5.dll preview.pas *.ini

  for %%f in (%FilesToCopy%) do copy /y %%f "%TargetDirectory%\"
  set RegFile="%TargetDirectory%\ExpertsXE5.reg"
::  explorer /select,"%TargetDirectory%\GExpertsRSXE5.dll"

:: expand backslash into double backslash for .REG file
  set ExpertTarget="%TargetDirectory%\GExpertsRSXE5.dll"
  set ExpertTarget=%ExpertTarget:\=\\%

::Windows Registry Editor Version 5.00
::
::[HKEY_CURRENT_USER\Software\Embarcadero\BDS\12.0\Experts]
::"GExperts"="C:\\Program Files (x86)\\GExperts for RAD Studio XE5\\GExpertsRSXE5.dll"
::
  echo Windows Registry Editor Version 5.00 >%RegFile%
  echo. >>%RegFile%
  echo [HKEY_CURRENT_USER\Software\Embarcadero\BDS\12.0\Experts] >>%RegFile%
  echo "GExperts"=%ExpertTarget% >>%RegFile%
  echo. >>%RegFile%
::  explorer /select,%RegFile%
  regedit /S %RegFile%

  endlocal

  goto :exit


:exit
