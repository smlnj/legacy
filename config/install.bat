REM Win32 installer for SML/NJ.
REM
REM (C) 2003 The Fellowship of SML/NJ.
REM
REM Author: Matthias Blume (blume@tti-c.org)

if "%SMLNJ_HOME%"=="" (echo Please set the SMLNJ_HOME environment variable && goto :EOF)
if NOT EXIST %SMLNJ_HOME%\sml.boot.x86-win32 (echo Please expand the boot.x86-win32.tgz file to the root of your SMLNJ source tree && goto :EOF)

REM begin by creating rudimentary directory hierarchy
if EXIST bin (rmdir /s /q bin)
mkdir bin
mkdir bin\.run
mkdir bin\.heap
mkdir lib

REM compile runtime system and move executable to bin\.run
cd base\runtime\objs
nmake -f mk.x86-win32
copy /y run.x86-win32.exe ..\..\..\bin\.run\run.x86-win32.exe
cd ..\..\..

REM put helper .bat scripts into bin
copy config\link-sml.bat bin
copy config\ml-build.bat bin
copy config\sml.bat bin

REM copy config\ml-lex.bat bin
REM copy config\lexgen.bat bin
REM copy config\ml-yacc.bat bin

REM create heap image and lib hierarchy ("boot")
copy config\preloads preloads.standard
cd sml.boot.x86-win32
..\bin\.run\run.x86-win32.exe @SMLboot=BOOTLIST @SMLheap=sml @SMLalloc=1M @SMLverbose
cd ..
move sml.x86-win32 bin\.heap
del preloads.standard
cd sml.boot.x86-win32
for /D %%a in (*.*) do echo %%a %%a >>..\lib\pathconfig
for /D %%a in (*.*) do xcopy /e /y /h /i %%a ..\lib\%%a
cd ..

REM Do all the rest using the precompiled installer.
%COMSPEC% /C "bin\sml.bat -m $smlnj/installer.cm"
