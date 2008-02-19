#!/bin/sh
## a script for building various versions of R
## based on kurt hornik's check-R
## theussl 2007-02
##
## TODO: win32
 
## set debugging
debug=0

## R-flavors to build
R_flavors='R-devel R-patched R-release'

## local R directory
R_dir=/srv/R

## local CRAN mirror
CRAN_rsync=${R_dir}/CRAN

## CRAN's src/contrib directory
CRAN_dir=${CRAN_rsync}/src/contrib

## Set permissions right
umask 002

do_cleanup_and_exit () {
  exit ${1-0}
}

#(

if [[ ${debug} == 1 ]] ; then
echo "Start building R"
date
echo "--------------------------------------"
fi

## change to build dir
test -d ${R_dir} || mkdir ${R_dir} || do_cleanup_and_exit 1


for R_flavor in ${R_flavors}; do
    if [[ ${debug} == 1 ]] ; then echo "$R_flavor ..." ; fi
    cd ${R_dir}
    test -d ${R_flavor} || mkdir ${R_flavor} || do_cleanup_and_exit 1
    cd ${R_flavor}
    test -d src || mkdir src || do_cleanup_and_exit 1
    case "${R_flavor}" in
	R-devel)
	    url=ftp://ftp.stat.math.ethz.ch/Software/R/R-devel.tar.gz ;;
	R-patched)
      ## <NOTE>
      ## Adjust as needed ...
	    url=ftp://ftp.stat.math.ethz.ch/Software/R/R-patched.tar.gz
      ## url=http://cran.at.r-project.org/src/base-prerelease/R-latest.tar.gz
      ## </NOTE>
	    ;;
	R-release)
	    url=http://cran.r-project.org/src/base/R-latest.tar.gz ;;
    esac
    mv src src.save
    (mkdir tmp &&
	cd tmp &&
	touch stamp &&
	wget -O - --retr-symlinks ${url} | tar zxmf - &&
	entry=`find . -newer stamp -type d -mindepth 1 -maxdepth 1` &&
	mv ${entry} ../src &&
	cd .. &&
	rm -rf tmp) || (rm -rf tmp; mv src.save src;
	continue )
    if [[ -s src.save/SVN-REVISION ]]; then
	revision_new=`tail -n1 src/SVN-REVISION | cut -f2 -d ":"`
	revision_old=`tail -n1 src.save/SVN-REVISION | cut -f2 -d ":"`
	if [[ ${debug} == 1 ]] ; then echo "new: ${revision_new} old: ${revision_old}" ; fi
	if [[ ${revision_new} == ${revision_old} ]] ; then
	    rm -rf src
	    mv src.save src
	fi
    else
        ## Link recommended packages.
	(cd src; \
	CRAN_RSYNC="${CRAN_rsync}" ./tools/rsync-recommended)

        ## Rebuild R.
	date=`date +%y-%m-%d`
	BUILD=build.$date
	rm -rf $BUILD
	mkdir $BUILD
	(cd $BUILD && ../src/configure ${configure_args} ${compilers} \
	    && make && make check && rm -f $R_dir/bin/$R_flavor && ln -s \
	    $R_dir/$R_flavor/$BUILD/bin/R $R_dir/bin/$R_flavor) || do_cleanup_and_exit 1
	mkdir $BUILD/Packages
	
    fi
    R_HOME=`./build/bin/R RHOME`
    ## Update ${R_flavor} sources.
    ## Actually, we should check whether flavor of source and target agree.

    R_VERSION=`cut -f1 -d' ' src/VERSION`
    if test "`cut -f2 -d' ' src/VERSION`" = "Patched"; then
	R_VERSION=`echo ${R_VERSION} | sed 's/\.[0-9]*$//'`
	R_VERSION="${R_VERSION}-patched"
    fi
       
    if [[ ${debug} == 1 ]] ; then  
	echo "Built version $R_VERSION with home $R_HOME"
	date
	echo .
    fi

done
#) | mail -s "R-build" theussl@ai.wu-wien.ac.at

exit 0

