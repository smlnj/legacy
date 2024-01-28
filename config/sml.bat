@echo OFF

REM sml.bat
REM
REM Copyright 2022 The Fellowship of SML/NJ (http://www.smlnj.org)
REM All rights reserved.
REM
REM The standard driver for SML/NJ under the new runtime system
REM

title Standard ML of New Jersey
setlocal

if "%SMLNJ_HOME%"=="" set SMLNJ_HOME=%~dp0\..
if NOT EXIST "%SMLNJ_HOME%\bin\.run\run.x86-win32.exe" set SMLNJ_HOME=%~dp0\..

set CM_PATHCONFIG=%SMLNJ_HOME%\lib\pathconfig
"%SMLNJ_HOME%\bin\.run\run.x86-win32.exe" "@SMLload=%SMLNJ_HOME%\bin\.heap\sml" %*
