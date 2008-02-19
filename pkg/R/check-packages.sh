#!/bin/sh
## a script for building packages with R
## based on kurt hornik's check-R
## theussl 2007-02
##
## TODO: win32
 
## set debugging
debug=1

## R-flavors
R_flavors='R-devel R-patched R-release'
R_build_flavor='R-patched'

## local R directory
R_dir=/srv/R
## local CRAN mirror
CRAN_rsync=${R_dir}/CRAN
## CRAN's src/contrib directory
CRAN_dir=${CRAN_rsync}/src/contrib
## local R-Forge mirror
R_FORGE=${R_dir}/pkgs
## provide R-Forge packages in
R_Forge_contrib_dir=${R_dir}/R-Forge/src/contrib 
## R scripts directory.
R_scripts_dir=${R_dir}/lib/Scripts
## R profile for checking.
R_profile=${R_scripts_dir}/check_profile.R
## where the work is done
check_dir=${R_dir}/R.check
## Check date in ISO 8601 format.  GNU specific ...
check_date=`date -Idate`
## Check result files
check_results_files="SUMMARY check.csv summary.rds time_c.out time_i.out"

## Set permissions right
umask 022

## Start a virtual framebuffer X server and use this for DISPLAY so that
## we can run package tcltk and friends.  We use the PID of the check
## process as the server number so that the checks for different flavors
## get different servers.
PATH=${PATH}:/usr/bin/X11
Xvfb :${$} -screen 0 1280x1024x24 &
Xvfb_pid=`ps auxw | grep "Xvfb :${$}" | grep -v grep | awk '{ print $2 }'`
export DISPLAY=:${$}

test -d $check_dir || mkdir $check_dir || do_cleanup_and_exit 1
cd $check_dir
## If there is an old Xvfb/check process remaining, kill it:
test -f Xvfb.pid && kill -9 `cat Xvfb.pid`
## Save the new pid.
echo ${Xvfb_pid} > Xvfb.pid

if test -n "${BASH_VERSION}"; then
  ## No process is allowed more than 10 minutes
  ulimit -t 600
fi

export _R_CHECK_WEAVE_VIGNETTES_=no
export _R_CHECK_SUBDIRS_STRICT_=yes
## We really need 'false' for R < 2.4.0 ...
export _R_CHECK_FORCE_SUGGESTS_=false

do_cleanup_and_exit () {
  kill -9 ${Xvfb_pid}
  exit ${1-0}
}

## START MAIL LOG
(
test -d ${check_dir} || mkdir ${check_dir} || do_cleanup_and_exit 1


## Structure inside $check_dir: subdirs for each flavor.  Within these,
## R sources are in 'src', R is built in 'build', packages are in
## 'PKGS', and results are saved in 'Results'.

for R_flavor in ${R_flavors}; do
    
    cd ${check_dir}
    test -d ${R_flavor} || mkdir ${R_flavor} || do_cleanup_and_exit 1
    
    cd ${R_flavor}
    
    rm -rf src
    cp -r $R_dir/$R_flavor/src src || continue
    
    ## Save old check results.
    for f in ${check_results_files}; do
	test -f "${f}.prev" && rm -f "${f}.prev"
	test -f "${f}"      && mv "${f}" "${f}.prev"
    done

    for d in Results Results/${check_date}; do
	test -d ${d} || mkdir ${d} || do_cleanup_and_exit 1
    done
    
    R_HOME=`${R_dir}/bin/${R_flavor} RHOME`
    
    ## Add check profile settings.
    ## Currently, adehabitat on aragorn.ci.tuwien.ac.at cannot allocate
    ## enough colors for X11/png with X11colortype "true", argh.
    if test -r "${R_profile}"; then
	(echo; cat "${R_profile}") > Rprofile
    fi

    R_base_pkgs=
    if test -f src/share/make/vars.mk; then
	R_base_pkgs=`grep '^R_PKGS_BASE *=' src/share/make/vars.mk | \
    sed 's/.*=//'`
    fi

    ## Packages.
    test -d PKGS.prev && rm -rf PKGS.prev
    test -d PKGS      && mv PKGS PKGS.prev
    mkdir PKGS
    cd PKGS
    
    ## checked out packages 
    cp -r $R_FORGE/* .
 
    ## initialize package list
    all_pkgs=
    ## add recommended packages to package list
    ##dir=${CRAN_dir}/${R_VERSION}/Recommended
    ##if test -d ${dir}; then
	##for f in ${dir}/*.tar.gz; do tar zxf ${f}; done
	##CRAN_vspec_recommended_pkgs=`cd ${dir}; ls *.tar.gz | sed 's/_.*//'`
	##all_pkgs="${all_pkgs} ${CRAN_vspec_recommended_pkgs}"
    ##fi
    ## add 'other' packages to package list
    ##dir=${CRAN_dir}/${R_VERSION}/Other
    ##if test -d ${dir}; then
##	for f in ${dir}/*.tar.gz; do tar zxf ${f}; done
	##CRAN_vspec_other_pkgs=`cd ${dir}; ls *.tar.gz | sed 's/_.*//'`
	##all_pkgs="${all_pkgs} ${CRAN_vspec_other_pkgs}"
    ##fi
    
    #CRAN_main_pkgs=`cd ${CRAN_dir}; ls *.tar.gz`
    #for p in ${R_base_pkgs} ; do #\
	#${CRAN_vspec_recommended_pkgs} \
	#${CRAN_vspec_other_pkgs}
	
      #CRAN_main_pkgs=`echo "${CRAN_main_pkgs}" | sed "s/^${p}_.*//"`
    #done
    
    #CRAN_main_pkgs=`echo "${CRAN_main_pkgs}" | sed 's/_.*//'`
    
    ## list exported svn reps of R-Forge
    rforge_pkgs=`ls`

    #all_pkgs="${all_pkgs} ${CRAN_main_pkgs} ${rforge_pkgs}"
    #all_pkgs=`echo "${all_pkgs}" | tr ' ' '\n' | sort | uniq`
    
    ## see check-R for reasons
    pkgs_install_fake_cannot_run="BRugs|ROracle|RmSQL|RScaLAPACK|RWinEdt|Rlsf|Rmpi|httpRequest|mimR|prim|rcdd|rcom|rpvm|snowFT|sound|spectrino|taskPR|tdm|titan|wnominate|xlsReadWrite"
    pkgs_install_fake_too_long="RJaCGH|aster|ks|np|pscl|sna|tgp|twang"
    pkgs_install_fake_regexp="^(${pkgs_install_fake_cannot_run}|${pkgs_install_fake_too_long})\$"
    pkgs_install_no_regexp='^(CoCo|GOSim|GeneNT|LMGene|NORMT3|ProbeR|RBloomberg|RGrace|SAGx|SLmisc|bcp|caMassClass|lsa|pcalg|tcltk2)$'

  
    pkgs_rforge_install_yes=`echo "${rforge_pkgs}" \
  | egrep -v "${pkgs_install_fake_regexp}" \
  | egrep -v "${pkgs_install_no_regexp}"`
    #pkgs_install_yes="${pkgs_rforge_install_yes}"
    pkgs_rforge_install_fake=`echo "${rforge_pkgs}" | egrep "${pkgs_install_fake_regexp}"`
    pkgs_rforge_install_no=`echo "${rforge_pkgs}" | egrep "${pkgs_install_no_regexp}"`

    ## Installation first ...
    ## Note that installing to the default library tree updates the HTML
    ## indices, which is very time consuming (as we install one package at a
    ## time to safeguard against limits on the size of the command line).
    ## Hence, we install the packages to a different library tree
    ## (${R_HOME}/Packages).
    
    ## R binary
    R_bin=${R_dir}/bin/${R_flavor}

    echo ${pkgs_rforge_install_yes} > rforge_pkgs_ul.dat
    export CRAN_dir=$CRAN_dir
    ${R_bin} --vanilla --slave <<-EOF
	source("${R_scripts_dir}/packages.R")
        source("${R_scripts_dir}/R_Forge_utils.R")
	dir <- file_path_as_absolute(getwd())
	write_PACKAGES_from_source_dirs(dir) ## create PACKAGES of R-Forge
        dir_cran <- Sys.getenv("CRAN_dir")
        avail_cran <- available.packages(contriburl = sprintf("file:///%s", dir_cran))
        avail_rforge <- available.packages(contriburl = sprintf("file:///%s", dir))
        avail<-rbind(avail_rforge,avail_cran)
        pkgs <- scan("rforge_pkgs_ul.dat", character(), quiet = TRUE)
	pkgs <- pkgs[pkgs %in% rownames(avail_rforge)]
        pkgs_suggested <- resolve_suggests(pkgs, avail)
        pkgs_suggested <- pkgs_suggested[pkgs_suggested %in% rownames(avail)]
        pkgs_to_resolve_deps <- unique(c(pkgs,pkgs_suggested))
        pkgs_all <- resolve_dependencies(pkgs_to_resolve_deps, avail)
        DL <- utils:::.make_dependency_list(pkgs_all, avail)
        pkgs_cran <- setdiff(pkgs_all,pkgs)
        writeLines(pkgs_cran, "cran_pkgs_ol.dat")
	writeLines(utils:::.find_install_order(pkgs_all, DL),"rforge_pkgs_ol.dat")
EOF

    ## copy and extract packages not on R-Forge to local dir
    pkgs_cran=`cat cran_pkgs_ol.dat`
    pkgs_cran_install_yes=`echo "${pkgs_cran}" \
  | egrep -v "${pkgs_install_fake_regexp}" \
  | egrep -v "${pkgs_install_no_regexp}"`
    pkgs_cran_install_fake=`echo "${pkgs_cran}" | egrep "${pkgs_install_fake_regexp}"`
    pkgs_cran_install_no=`echo "${pkgs_cran}" | egrep "${pkgs_install_no_regexp}"`
    for p in ${pkgs_cran_install_yes} ${pkgs_cran_install_fake}; do tar zxf ${CRAN_dir}/${p}_*.tar.gz; done

    ## install packages considering install order
    pkgs_install=`cat rforge_pkgs_ol.dat` 
    pkgs_install_yes=`echo "${pkgs_install}" \
  | egrep -v "${pkgs_install_fake_regexp}" \
  | egrep -v "${pkgs_install_no_regexp}"`
    pkgs_install_fake=`echo "${pkgs_install}" | egrep "${pkgs_install_fake_regexp}"`
    pkgs_install_no=`echo "${pkgs_install}" | egrep "${pkgs_install_no_regexp}"`

    ## custom install dir of packages
    pkgs_library=${check_dir}/${R_flavor}/library
    ## clean and setup testing library
    rm -rf ${pkgs_library} 
    mkdir ${pkgs_library}
    
    for p in ${pkgs_install_yes}; do
	echo -n "${p}: " >> ../time_i.out
	/usr/bin/time -o ../time_i.out -a \
	    env R_LIBS="${pkgs_library}" \
	    ${R_bin} CMD INSTALL ${p} >${p}-install.out 2>&1
    done

    ## <NOTE>
    ## Need to actually provide fake installs (otherwise, dependencies
    ## cannot be honored).  Checking with --install=fake fake-installs
    ## again, which could perhaps be eliminated.
    for p in ${pkgs_install_fake}; do
	echo -n "${p}: " >> ../time_i.out
	/usr/bin/time -o ../time_i.out -a \
	    env R_LIBS="${pkgs_library}" \
	    ${R_bin} CMD INSTALL --fake ${p} >${p}-install.out 2>&1
    done
    ## </NOTE>

    ## And now the testing ...
    for p in ${pkgs_rforge_install_yes}; do
	echo -n "${p}: " >> ../time_c.out
	/usr/bin/time -o ../time_c.out -a \
	    env R_LIBS="${pkgs_library}" ${R_bin} CMD check \
	    --install="check:${p}-install.out" \
	    --library="${pkgs_library}" ${p}
	rm -f ${p}-install.out
    done

    for p in ${pkgs_rforge_install_fake}; do
	echo -n "${p}: " >> ../time_c.out
	/usr/bin/time -o ../time_c.out -a \
	    env R_LIBS="${pkgs_library}" \
	    ${R_bin} CMD check --install=fake ${p}
    done
    for p in ${pkgs_rforge_install_no}; do
	echo -n "${p}: " >> ../time_c.out
	/usr/bin/time -o ../time_c.out -a \
	    env R_LIBS="${pkgs_library}" \
	    ${R_bin} CMD check --install=no ${p}
    done
    ## ... and copy the package/bundle DESCRIPTION metadata over to the
    ## directories with the check results.
    for d in *.Rcheck; do
	/usr/bin/install -m 644 `basename ${d} .Rcheck`/DESCRIPTION \
	    ${d}/00package.dcf
    done
    
    
    ## Summary and check db
get_dcf_field () {
    ## Get one field including all continuation lines from a DCF file.
    ## Usage:
    ##   get_dcf_field FIELD FILE
    ws="[ 	]"		# space and tab
    (sed -n "/^${1}:/,/^[^ ]/{p;}" ${2} | \
	sed -n "/^${1}:/{s/^${1}:${ws}*//;p;}
            /^${ws}/{s/^${ws}*//;p;}") #|
    #sed "s/[ 	
#]*$//"
}
    
    (echo "Package,Version,Priority,Maintainer,Status,Comment"
	for d in *.Rcheck; do
	    package=`basename ${d} .Rcheck`
	    version=`get_dcf_field Version ${package}/DESCRIPTION | head -1`
	    priority=`get_dcf_field Priority ${package}/DESCRIPTION | head -1`
	    maintainer=`get_dcf_field Maintainer ${package}/DESCRIPTION | head -1`
	    warnings=`grep 'WARNING$' ${d}/00check.log`
	    errors=`grep 'ERROR' ${d}/00check.log`
	    if test -n "${errors}"; then
		status=ERROR
	    elif test -n "${warnings}"; then
		status=WARN
	    else
		status=OK
	    fi
	    if echo ${package} | egrep "${pkgs_install_fake_regexp}" >/dev/null; then
		comment="[--install=fake]"
	    elif echo ${package} | egrep "${pkgs_install_no_regexp}" >/dev/null; then
		comment="[--install=no]"
	    else
		comment=
	    fi
	    echo "${package},${version},${priority},\"${maintainer}\",${status},${comment}"
	    done) > ../check.csv # | sed 's/
#/ /' 
        
    (for d in *.Rcheck; do
	package=`basename ${d} .Rcheck`
	if test "${R_flavor}" = r-release; then
	    problems=`egrep -e 'ERROR$' ${d}/00check.log`
	else
	    problems=`egrep -e \
      '(^\*   Rd files|^\*   non-standard|(WARNING|ERROR)$)' \
      ${d}/00check.log`
	fi
	if test -n "${problems}"; then
	    echo "${package}"
	    egrep -e '^Maintainer:' "${package}/DESCRIPTION"
	    echo "${problems}"
	fi
	done) > ../SUMMARY
    
    cd ..				# Back in ${check_dir}/${R_flavor} now.
    ${R_bin} --vanilla --slave <<-EOF
	source("${R_scripts_dir}/check.R")
	summary <- check_summarize_flavor("${check_dir}", "${R_flavor}")
	.saveRDS(summary, file.path("${check_dir}", "${R_flavor}", "summary.rds"))
	EOF
    for f in ${check_results_files}; do
	cp "${f}" "Results/${check_date}"
    done
    
    for f in check.csv; do
	if test -f "${f}.prev"; then
	    diff "${f}.prev" "${f}" > "${f}.diff"
	    test -s "${f}.diff" || rm -f "${f}.diff"
	fi
	if test -f "${f}.diff"; then
	    ${R_bin} --vanilla --slave <<-EOF
	source("${R_scripts_dir}/check.R")
	db <- check_results_diffs(file.path("${check_dir}", "${R_flavor}"))
	sink("${f}.diff")
	writeLines("Changes in check status (S) and/or version (V):\n")
	print(db)
	sink()
	EOF
	    #mail -s "[CRAN-check] ${R_flavor}/`hostname` ${f} changes on `date -Iseconds`" \
		#${check_results_mail_recipients} < "${f}.diff"
	    #rm -f "${f}.diff"
	fi 
    done
	
    ##### end loop $R_flavor
done

## now independent from check process (own cron job)
## build source packages and make available for download
#/usr/local/bin/build-packages.sh

) | mail -s "R.check: R-Forge" stefan.theussl@wu-wien.ac.at

do_cleanup_and_exit
