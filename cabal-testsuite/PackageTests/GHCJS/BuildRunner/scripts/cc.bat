@ECHO off

FOR /f "delims=" %%A in ('call ghc.exe --print-libdir') do set "libdir=%%A"
FOR /f "delims=" %%A in ('call ghc.exe --numeric-version') do set "numVersion=%%A"
setlocal EnableDelayedExpansion

call :compareVersions 9.4.1 %numVersion%
if %errorlevel% == 1 (set "cc=gcc.exe") else (set "cc=clang.exe")
CALL !libdir:lib=mingw\bin\!%cc% %*
EXIT /B %ERRORLEVEL%

REM taken from https://stackoverflow.com/questions/15807762/compare-version-numbers-in-batch-file

:compareVersions  version1  version2
::
:: Compares two version numbers and returns the result in the ERRORLEVEL
::
:: Returns 1 if version1 > version2
::         0 if version1 = version2
::        -1 if version1 < version2
::
:: The nodes must be delimited by . or , or -
::
:: Nodes are normally strictly numeric, without a 0 prefix. A letter suffix
:: is treated as a separate node
::
setlocal enableDelayedExpansion
set "v1=%~1"
set "v2=%~2"
:loop
call :parseNode "%v1%" n1 v1
call :parseNode "%v2%" n2 v2
if %n1% gtr %n2% exit /b 1
if %n1% lss %n2% exit /b -1
if not defined v1 if not defined v2 exit /b 0
if not defined v1 exit /b -1
if not defined v2 exit /b 1
goto :loop


:parseNode  version  nodeVar  remainderVar
for /f "tokens=1* delims=." %%A in ("%~1") do (
  set "%~2=%%A"
  set "%~3=%%B"
)
