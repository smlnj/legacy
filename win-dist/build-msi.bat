REM build-msi.bat
REM
REM (C) 2020 The Fellowship of SML/NJ.
REM
REM Script to build the MSI installer for SML/NJ on Windows.
REM

if "%SMLNJ_HOME%"=="" (echo Please set the SMLNJ_HOME environment variable && goto :EOF)
if NOT EXIST %SMLNJ_HOME%\sml.boot.x86-win32 (echo Please expand the boot.x86-win32.tgz file to the root of your SMLNJ source tree && goto :EOF)

REM start by building the runtime, compiler, and other tools and libraries
REM

config\install.bat

REM now we can build the installer using the WiX Toolset
REM

setlocal
SET SCRIPTDIR=config\WinSetup

%SCRIPTDIR%\GenerateWixFile.exe %SMLNJ_HOME%\lib
%SCRIPTDIR%\candle.exe lib.wxs %SCRIPTDIR%\smlnj.wxs
@if ERRORLEVEL 1 (echo Candle failed & goto :EOF)
%SCRIPTDIR%\light.exe -out smlnj.msi lib.wixobj smlnj.wixobj %SCRIPTDIR%\wixui.wixlib -loc %SCRIPTDIR%\WixUI_en-us.wxl %SCRIPTDIR%\sca.wixlib
