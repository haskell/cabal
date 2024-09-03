@echo OFF

REM replace the libdir with the path to the compiler
FOR /f "delims=" %%A in ('call ghc.exe --print-libdir') do set "var=%%A"
setlocal EnableDelayedExpansion
CALL !var:lib=mingw\bin\clang.exe! -DNOERROR6 %*
EXIT /B %ERRORLEVEL%
