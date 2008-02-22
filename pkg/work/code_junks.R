
## stop list, fake install, ... from check-R: do we need this on R-Forge

#all_pkgs="${all_pkgs} ${CRAN_main_pkgs} ${rforge_pkgs}"
#all_pkgs=`echo "${all_pkgs}" | tr ' ' '\n' | sort | uniq`
    
## see check-R for reasons
## pkgs_install_fake_cannot_run="BRugs|ROracle|RmSQL|RScaLAPACK|RWinEdt|Rlsf|Rmpi|httpRequest|mimR|prim|rcdd|rcom|rpvm|snowFT|sound|spectrino|taskPR|tdm|titan|wnominate|xlsReadWrite"
##    pkgs_install_fake_too_long="RJaCGH|aster|ks|np|pscl|sna|tgp|twang"
##    pkgs_install_fake_regexp="^(${pkgs_install_fake_cannot_run}|${pkgs_install_fake_too_long})\$"
##    pkgs_install_no_regexp='^(CoCo|GOSim|GeneNT|LMGene|NORMT3|ProbeR|RBloomberg|RGrace|SAGx|SLmisc|bcp|caMassClass|lsa|pcalg|tcltk2)$'

  
##    pkgs_rforge_install_yes=`echo "${rforge_pkgs}" \
##  | egrep -v "${pkgs_install_fake_regexp}" \
##  | egrep -v "${pkgs_install_no_regexp}"`
    #pkgs_install_yes="${pkgs_rforge_install_yes}"
##    pkgs_rforge_install_fake=`echo "${rforge_pkgs}" | egrep "${pkgs_install_fake_regexp}"`
##    pkgs_rforge_install_no=`echo "${rforge_pkgs}" | egrep "${pkgs_install_no_regexp}"`


    ## Installation first ...
    ## Note that installing to the default library tree updates the HTML
    ## indices, which is very time consuming (as we install one package at a
    ## time to safeguard against limits on the size of the command line).
    ## Hence, we install the packages to a different library tree
    ## (${R_HOME}/Packages).
    
    ## R binary
##    R_bin=${R_dir}/bin/${R_flavor}

    echo ${pkgs_rforge_install_yes} > rforge_pkgs_ul.dat
    export CRAN_dir=$CRAN_dir

${R_bin} --vanilla --slave <<-EOF


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





## remaining check stuff


## Structure inside $check_dir: subdirs for each flavor.  Within these,
## R sources are in 'src', R is built in 'build', packages are in
## 'PKGS', and results are saved in 'Results'.

## LAST preparation before checking

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
