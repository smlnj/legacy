setlocal
SET SCRIPTDIR=%~dp0

REM Note that if you're mapping your source from a shared drive, you may need to
REM do the following to allow .NET code to run 'like on a local drive'
REM caspol -q -machine -addgroup 1 -url file://z:/* FullTrust -name "Z Drive"

%SCRIPTDIR%\GenerateWixFile.exe %SMLNJ_HOME%\lib
%SCRIPTDIR%\candle.exe lib.wxs %SCRIPTDIR%\smlnj.wxs
@if ERRORLEVEL 1 (echo Candle failed & goto :EOF)
%SCRIPTDIR%\light.exe -out smlnj.msi lib.wixobj smlnj.wixobj %SCRIPTDIR%\wixui.wixlib -loc %SCRIPTDIR%\WixUI_en-us.wxl %SCRIPTDIR%\sca.wixlib
