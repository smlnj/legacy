@echo off

set flags=
set setup=

:DOFLAGS

if %1 == -D goto FOUNDFLAG
if %1 == -U goto FOUNDFLAG
if %1 == -C goto FOUNDFLAG
if %1 == -S goto FOUNDSETUP
goto DONEFLAGS

:FOUNDFLAG

set flags=%flags% %1%2
shift
shift
goto DOFLAGS

:FOUNDSETUP

set setup=%2
shift
shift
goto DOFLAGS

:DONEFLAGS

set root=%1
set main=%2
set heap=%3

set smlfile=XYZ_XXX_smlfile.sml
set cmfile=XYZ_XXX_cmfile.cm
set listfile=XYZ_XXX_BOOTLIST
set linkargsfile=XYZ_XXX_LINKARGS

set rare=XYZ_XXX_0123

echo structure %rare% = struct val _ = SMLofNJ.exportFn ("%heap%", %main%) end >"%smlfile%"

echo Group structure %rare% is $/basis.cm "%root%" %smlfile% >%cmfile%

%COMSPEC% /C "%SMLNJ_HOME%\bin\sml.bat %flags% %setup% @CMbuild %root% %cmfile% %heap% %listfile% %linkargsfile%"
if ERRORLEVEL 1 goto ERR
if NOT EXIST %linkargsfile% goto END
"%SMLNJ_HOME%\bin\.run\run.x86-win32.exe" @SMLboot=%listfile%
del %linkargsfile%
goto END

:ERR
echo Compilation failed with error.

:END
rem more cleaning up
del %smlfile%
del %cmfile%
del %listfile%
del .cm\GUID\%smlfile%
del .cm\SKEL\%smlfile%
del .cm\x86-win32\%smlfile%
