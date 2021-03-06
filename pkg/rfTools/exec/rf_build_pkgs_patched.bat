@cls
rem @echo off

rem rf Windows Toolchain: rf_build_pkgs_patched.bat
rem R-Forge Windows build/check system script initiating R-Forge package building
rem In previous versions this file was called 'automake.bat'
rem Licence GPL-3
rem Author: Stefan Theussl
rem Last Change: 2012-05-30

rem Configuration:
set rf_run_dir=R:\run\build\patched
set rf_lib_dir="R:\lib\local"
set rf_R_base=R:\lib\R\R-patched

rem Read environment variables from text file
FOR /F "tokens=*" %%i in ('type %rf_lib_dir%\rfTools\wintools\environment_variables.txt') do SET %%i

rem logs are written to  (.Rout stuff)
IF NOT EXIST %rf_run_dir% mkdir %rf_run_dir%
R:
cd %rf_run_dir%

rem copy current build script to run directory
cp -f %rf_lib_dir%\rfTools\exec\rf_build_packages.R .


rem use R-patched as build flavor
set PATH=%rf_R_base%\bin;%rf_R_base%\bin\x64;%PATH%
rem echo %PATH%

%rf_R_base%\bin\x64\R CMD BATCH --no-restore --no-save rf_build_packages.R %rf_run_dir%\rf_build_packages.Rout
