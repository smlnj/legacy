@echo off
REM
REM build-msi.bat
REM
REM (C) 2024 The Fellowship of SML/NJ.
REM
REM Script to build the MSI installer for SML/NJ on Windows.
REM

if "%SMLNJ_HOME%"=="" (echo Please set the SMLNJ_HOME environment variable && goto :EOF)
if NOT EXIST %SMLNJ_HOME%\sml.boot.x86-win32 (echo Please expand the boot.x86-win32.tgz file to the root of your SMLNJ source tree && goto :EOF)

set WIN_DIST_DIR=%CD%

chdir %SMLNJ_HOME%

REM start by building the runtime, compiler, and other tools and libraries
REM

call config\install.bat

REM now we can build the installer using the WiX Toolset
REM

echo build MSI package
setlocal
SET SCRIPTDIR=%WIN_DIST_DIR%/WinSetup

%SCRIPTDIR%\GenerateWixFile.exe %SMLNJ_HOME%\lib
%SCRIPTDIR%\candle.exe lib.wxs %SCRIPTDIR%\smlnj.wxs
@if ERRORLEVEL 1 (echo Candle failed & goto :EOF)
%SCRIPTDIR%\light.exe -out smlnj.msi lib.wixobj smlnj.wixobj %SCRIPTDIR%\wixui.wixlib -loc %SCRIPTDIR%\WixUI_en-us.wxl %SCRIPTDIR%\sca.wixlib

chdir %WIN_DIST_DIR%

