#!/bin/sh
## R-Forge package building
## just calls R and the R-Forge infrastructure package which includes
## all the necessary functions
## theussl 2009-01
##

## Environment variables indicating availability of services
export _R_CHECK_HAVE_FAME_=FALSE
export _R_CHECK_HAVE_ODBC_=FALSE
export _R_CHECK_HAVE_ORACLE_=FALSE
export _R_CHECK_HAVE_MYSQL_=FALSE
export _R_CHECK_HAVE_PADI_=FALSE
export _R_CHECK_HAVE_POSTGRES_=FALSE
export _R_CHECK_HAVE_SQLLITE_=FALSE
## as some packages open a lot of browser and ghostview windows'
export R_BROWSER=FALSE
export R_PDFVIEWER=FALSE

umask 002

## Before we start we set limits for all new processes
## max cpu time, default unlimited -> we set it to 10m
cpu_time_limit=600
ulimit -t $cpu_time_limit

## the rest has no effect to build process (at least on mac)
## core size (size of a core dump of a program). default 0
##ulimit -c 0
## main memory limit, default unlimited -> we set it to 256MB
##ulimit -m 262144
## virtual memory (swap space), default unlimited -> we set it to 256MB
##ulimit -v 262144
## main memory limit, default unlimited -> we set it to 256MB
##ulimit -l 262144 
## max user processes, default 266 (Linux unlimited) -> we set it to 200
##ulimit -u 200
## max file size, default unlimited -> we set it to 256MB
##ulimit -f 524288

## Where is the package library?
export R_LIBS="/Users/rforge/lib/R"
## On MacOSX gfortran is an external package, where are the links to it
export PATH=/usr/local/bin:$PATH
## on Leopard we have to export X11 bin directory
export PATH=/usr/X11/bin:$PATH

## Base R-Forge directory
R_dir=/srv/R
## R build flavor
R_flavor="R-$1"

if [[ ! ($R_flavor == "R-devel" || $R_flavor == "R-patched" || $R_flavor == "R-release") ]] ; then
echo "Run $0 using with the first argument either 'devel', 'patched' or 'release' and
the second the Mac OSX flavor 'leopard' or 'tiger'!"
exit 1
fi

if [[ $R_flavor == "R-devel" ]] ; then
    export _R_CHECK_LICENSE_=TRUE
fi

## R mac flavor
R_MAC_OS_flavor=$2
if [[ $R_MAC_OS_flavor == "leopard" ]] ; then
    R_MAC_OS_flavor=leopard
    R_MAC_FLAVOR_ARCH=x86_64
    R_exec_args="--arch=$R_MAC_FLAVOR_ARCH"
else 
    if [[ $R_MAC_OS_flavor == "tiger" ]] ; then
	R_MAC_OS_flavor=tiger
	R_MAC_FLAVOR_ARCH=x86_32
    else
	echo "No Mac flavor given. Run $0 without arguments for help!"
	exit 1
    fi
fi


## our stop_list (exported with rforge_generate_stoplist)
stoplist=$R_dir/lib/check_R_stoplist

## Having different versions of R on Mac only works via the Current symlink
R_VERSIONS_DIR=/Library/Frameworks/R.framework/Versions
date=`date +%y-%m-%d`

BUILD=`find ${R_dir}/${R_MAC_OS_flavor}/${R_flavor} -mindepth 1 -maxdepth 1 | grep ./build. | sort | tail -n1`
echo "trying to use $R_flavor in $BUILD" 

R_FALLBACK=${R_VERSIONS_DIR}/2.14

R_installation_dir=${BUILD}

if [[ -d $R_installation_dir ]] ; then
    (cd $R_VERSIONS_DIR && rm -f Current && ln -s $R_installation_dir/Library/Frameworks/R.framework/Versions/Current Current)
    echo "Building universal binaries for ${R_MAC_OS_flavor} using $R_flavor"
else
    (cd $R_VERSIONS_DIR && rm -f Current && ln -s ${R_FALLBACK} Current)
    echo "WARNING: No build in $R_installation_dir using ${R_FALLBACK} to build ${R_flavor} ${R_MAC_OS_flavor} binaries"
fi

## points always to Current in Versions
R_bin="/Library/Frameworks/R.framework/Resources/bin/R"

${R_bin} --vanilla --slave $R_exec_args < ~/bin/rf_build_packages

(cd $R_VERSIONS_DIR && rm -f Current && ln -s ${R_FALLBACK} Current)
echo "All builds generated. Setting 'Current' back to  ${R_FALLBACK}."

exit 0
