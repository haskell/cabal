@echo OFF

where /q clang.exe

IF %ERRORLEVEL% EQU 0 (
   call clang.exe -DNOERROR6 %*
   EXIT /B %ERRORLEVEL%
)

ECHO "Cannot find C compiler"
EXIT /B 1
