@cls
@echo off

rem rf Windows Toolchain: rf_shell.bat
rem This script starts a shell providing the same environment as for the R-Forge build/check system
rem In previous versions this file was called 'rforge_start_Windows_shell.bat'rem 
rem Licence GPL-3
rem Author: Stefan Theussl
rem Last Change: 2012-05-30

rem Configuration:
set rf_lib_dir="R:\lib\local"
set rf_R_base="R:\lib\R\R-patched"

rem Read environment variables from text file
FOR /F "tokens=*" %%i in ('type %rf_lib_dir%\rfTools\wintools\environment_variables.txt') do SET %%i

rem Which default R installation should we use?
set PATH="%rf_R_base%\bin";"%rf_R_base%\bin\x64";%PATH%

cd R:\

rem Start shell
%windir%\system32\cmd.exe
