
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
