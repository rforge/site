@cls
@echo off

rem rf Windows Toolchain: rf_update_R.bat
rem This script updates/installs the current (patched/devel) version of R
rem In previous versions this file was called 'R_install.bat'
rem Licence GPL-3
rem Author: Stefan Theussl
rem Last Change: 2010-10-28

rem Read environment variables from text file
FOR /F "tokens=*" %%i in ('type R:\bin\environment_variables.txt') do SET %%i

rem Install R for Windows and update sources

rem ##################################
rem R-devel
rem ##################################

rem Download and refresh R

rm -rf R:\lib\R\R-devel
cd %TMPDIR%
R:\share\cygwin\bin\wget.exe -O R-devel.exe http://cran.wu.ac.at/bin/windows/base/R-devel.exe
chmod a+x R-devel.exe
R-devel.exe /LOADINF="R:\lib\R\r-devel.inf" /SILENT
rm -f R-devel.exe

rem copy over configs

cp -f R:\lib\scripts\etc\i386\Makevars.site R:\lib\R\R-devel\etc\i386\
cp -f R:\lib\scripts\etc\x64\Makevars.site R:\lib\R\R-devel\etc\x64\
cp -f R:\lib\scripts\etc\i386\Renviron.site R:\lib\R\R-devel\etc\i386\
cp -f R:\lib\scripts\etc\x64\Renviron.site R:\lib\R\R-devel\etc\x64\

rem Update SVN
rem svn up R:\R\svn\R-devel

rem ##################################
rem R-patched
rem ##################################

rem Download and refresh R

rm -rf R:\lib\R\R-patched
cd %TMPDIR%
rem for the time being R prerelease instead of R-patched.exe 
R:\share\cygwin\bin\wget.exe -O R-patched.exe http://cran.wu.ac.at/bin/windows/base/R-patched.exe
rem R-patched.exe R-2.15.0alpha-win.exe

chmod a+x R-patched.exe
R-patched.exe /LOADINF="R:\lib\R\r-patched.inf" /SILENT
rm -f R-patched.exe

rem copy over configs

cp -f R:\lib\scripts\etc\i386\Makevars.site R:\lib\R\R-patched\etc\i386\
cp -f R:\lib\scripts\etc\x64\Makevars.site R:\lib\R\R-patched\etc\x64\
cp -f R:\lib\scripts\etc\i386\Renviron.site R:\lib\R\R-patched\etc\i386\
cp -f R:\lib\scripts\etc\x64\Renviron.site R:\lib\R\R-patched\etc\x64\


rem Get R base sources

rem R:\share\cygwin\bin\wget.exe ftp://ftp.stat.math.ethz.ch/Software/R/R-patched.tar.gz
rem tar xzf R-patched.tar.gz
rem rm -rf R:\lib\R\src\R-patched
rem mv R-patched R:\lib\R\src\R-patched
rem rm -f R-patched.tar.gz

rem prepare for build (not needed anymore since 2.15?)

rem cd R:\lib\R\src\R-patched\src\gnuwin32

rem cp -r R:\lib\Rextras\src\gnuwin32\bitmap .
rem cp -r R:\lib\Rextras\src\gnuwin32\unicode .
rem cp -r R:\lib\Rextras\Tcl ..\..\
rem chmod -R u+rwx .
rem chmod -R u+rwx ../../Tcl
rem cp -r R:\compiler\Rtools\extras\MkRules .

rem  build the distribution

rem make Rpwd.exe
rem make rsync-recommended
rem make -j4 all

rem temporarily until R is installed from sources add libpng to install dir

rem cp -r R:\lib\R\src\R-patched\src\gnuwin32 R:\lib\R\R-patched\src\gnuwin32