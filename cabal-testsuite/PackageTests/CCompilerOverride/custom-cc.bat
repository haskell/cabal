@echo OFF

where /q gcc.exe

IF %ERRORLEVEL% EQU 0 (
   call gcc.exe -DNOERROR6 %*
   EXIT /B %ERRORLEVEL%
)

ECHO "Cannot find C compiler"
EXIT /B 1