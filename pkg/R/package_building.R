## The package build infrastructure

## from Unix shell script


## R build flavor
#R_build_flavor='R-patched'

## local R directory
#R_dir=/srv/R
## local R-Forge mirror
#R_FORGE=${R_dir}/pkgs
## provide R-Forge packages in
#R_Forge_contrib_dir=${R_dir}/R-Forge/src/contrib 
## R scripts directory.
#R_scripts_dir=${R_dir}/lib/Scripts
## R profile for checking.
#R_profile=${R_scripts_dir}/check_profile.R
## R check dir
#check_dir=${R_dir}/R.check
## R build log dir
#R_buildlog_dir=${check_dir}/buildlog

#export _R_CHECK_WEAVE_VIGNETTES_=no
#export _R_CHECK_SUBDIRS_STRICT_=yes
## We really need 'false' for R < 2.4.0 ...
#export _R_CHECK_FORCE_SUGGESTS_=false

## R-Forge specific texmf (submitted from users)
#export TEXMFLOCAL=/srv/R/share/texmf

## Set permissions right
#umask 022




## from the Windows build environment

##maj.version <- Sys.getenv("maj.version")
##if(maj.version == "") stop("env.var maj.version is missing!!!")


## source("d:/Rcompile/CRANpkg/make/CRANbinaries.R")
## source("d:/Rcompile/CRANpkg/make/CRANcheckSummaryWin.R")
## source("d:/Rcompile/CRANpkg/make/maintainers.R")

##options(warn=1)

##libdir="c:\\srv\\R\\lib\\pkgs", path_to_local_library,
##path_to_pkg_src="c:\\srv\\R\\pkgs",      path_to_pkg_src
##path_to_pkg_log="c:\\srv\\rsync\\R.check\\buildlogs", path_to_pkg_log
##winbindir=paste("c:\\srv\\rsync\\R-Forge\\bin\\windows\\contrib\\",maj.version,sep=""), path_to_binary_contrib_dir
##donotcompile = "c:\\srv\\R\\lib\\scripts\\DoNotCompile", stoplist
##email="stefan.theussl@wu-wien.ac.at" email

## OLD way:
## When using a binary distribution (Windows, Mac) we don't need to
## install every package from source, we keep a complete local CRAN
## install updated


## update package repository
update_package_library<- function(pkgs, repository_url, lib, ...){
  ## first update all installed packages if necessary
  update.packages(lib=lib, repos = repository_url, ask = FALSE)
  ## install missing packages
  pkgs_installed <- installed.packages(lib = lib)
  pkgs_to_install <- set.diff(pkgs, pkgs_installed)
  install.packages(pkgs_to_install, lib = lib, repos = repository_url,
                   ask = FALSE, ...)
}

## remove certain pkgs from a pkg list
remove_excluded_pkgs <- function(pkgs, to_remove){
  excluded <- sapply(pkgs, "[", 1) %in% to_remove
  pkgs[!excluded]
}


## checks if directory exists---if not: creates it if  desired
check_directory <- function(dir, fix = FALSE){
  out <- TRUE
  if(!file.exists(dir)){
    out <- FALSE
    if(fix){
      dir.create(dir)
      if(!file.exists(dir))
        stop(paste("Cannot create directory", dir,"!"))
    }
  }
  out
}

## check, if packages can be installed to local library
check_local_library <- function(lib){
  ## look, if library is locked
  lock <- paste(lib,"00LOCK", sep="/")
  if(file.exists(lock))
     system(paste("rm -rf", lock))
}

provide_packages_in_contrib <- function(contrib_dir, platform){

  file_types <- c(Linux = ".tar.gz", MacOSX = ".tgz", Windows = ".zip")
  file_type <- file_types[platform]
  pkg_types <- c(Linux = "source", MacOSX = "mac.binary", Windows = "win.binary")
  pkg_type <- pkg_types[platform]
  ## Hard coding fields (we are indexing by number, so hard-coding important here!)
  fields <- c("Package", "Bundle", "Priority", "Version", "Depends", "Suggests", "Imports", "Contains")

  files <- dir()
  files <- files[grep(file_type,files)]
  for(i in files)
    file.copy(i, contrib_dir, overwrite = TRUE)
  ## remove old PACKAGES file
  old_dir <- file_path_as_absolute(getwd())
  setwd(contrib_dir)
  system(paste("rm -f PACKAGES*"))
  ## now find out which packages have old versions
  tmp<-dir()
  splitted <- strsplit(tmp, "_")
  packages <- sapply(splitted, "[", 1)
  ind <- 1:length(packages)
  versno <- unlist(strsplit(sapply(splitted, "[", 2), file_type))
  duplicated_pkgs <- duplicated(packages)
  if(any(duplicated_pkgs)){
  ## look for duplicated packages and remove older version
    for(i in packages[duplicated_pkgs]){     
      ind_package_to_remove <- ind[packages==i][version_order(versno[packages==i])[1]]
      system(paste("rm -f", tmp[ind_package_to_remove]))
    }
  } 
  ## Write a new PACKAGES and PACKAGES.gz file
  write_PACKAGES(dir = contrib_dir, fields = fields, type = pkg_type)
  ## Unix: wd <- getwd(); tools:::write_PACKAGES(wd,type="source")
  ## Under Windows additionally
  shell(paste("gzip -c", file.path(contrib_dir, "PACKAGES"), 
              ">", file.path(contrib_dir, "PACKAGES.gz")))
  ## back to old directory
  setwd(old_dir)
}

## Windows: mail_prog sendEmail
## send_host: Rbuild@xmhera.wu-wien.ac.at
## relay_host: statmath.wu-wien.ac.at

notify_admins <- function(packages, donotcompile, email, send_host, server, mail_prog, relay_host){
  attachment <- "Rnightlybuildmail.txt"
  donotcompiletxt <- paste(donotcompile, collapse="\n")
  write(c(paste("R-Forge", platform, "Build Log:"), " ",
          "Disk status:", " ",
          system("df -h", intern = TRUE), " ",
          "This packages have been kept back (stop list):", " ",
          donotcompiletxt, " ",
          paste("The binaries/sources of the following",
                length(unique(packages)),
                "packages are now available on R-Forge:"), " ",
          unique(packages)),
        file = attachment)
  if(mail_prog == "sendEmail")
    system(paste(mail_prog, "-f", send_host, "-t", email,
                 "-u \"R-Forge: Nightly build\" -m LOG -a", attachment,
                 "-s", relay_host))
  system(paste("rm -f", attachment))  
}


## this is where the work is done
## takes a control object containing control parameters, the platform,
## architecture, ... as arguments
build_packages <- function(email,
                           path_to_pkg_src,
                           path_to_pkg_log,
                           path_to_pkg_root,
                           stoplist = NULL,
                           path_to_local_library = NULL,
                           platform=c("Linux", "Windows", "MacOSX"),
                           architecture=c("x86_32", "x86_64"),
                           rforge_contrib.url="http://r-forge.r-project.org/src/contrib",
                           cran.url="http://cran.r-project.org",
                           control=list()){

  ## match arguments
  platform <- match.arg(platform) ## FIXME: automatically use info from .Platform?
  architecture <- match.arg(architecture)
  flavor
  maj.version <- paste(R.Version()$maj,unlist(strsplit(R.Version()$min,"[.]"))[1],sep=".")
  ## x86_32 on x86_64 allowed but not the other way round
  if((architecture=="x86_64") && (.Machine$sizeof.long == 4))
    stop("Building x86_64 binaries not possible on an x86_32 architecture") 

  ## handle different path separators
  path_separator <- c(unix = "/", windows = "\\")
  path_separator <- path_separator[.Platform$OS.type]
  
  ## We need the package tools for creating PACKAGES i.e.
  library("tools")

  ## check for necessary directories---create them if possible
  ## local package library
  check_directory(path_to_local_library, fix=TRUE)
  ## R-Forge package sources
  if(!check_directory(path_to_pkg_src))
    stop("Directory", path_to_pkg_src, "missing!")
  
  ## STOP LIST: packages which should not be compiled
  donotcompile <- if(file.exists(stoplist)){
    scan(stoplist, what = character(0)) 
  }else ""

  ## sourcepackages available from R-Forge---exported svn reps
  avail_src <- dir(path_to_pkg_src)
  pkgs_all <- pkgs <- avail_src
  ## platform specific packages or pkgs not avail in src
  ## (Important for Windows, Mac)
  pkgs_other = ""
  if(platform != "Linux"){
    avail_rforge <- available.packages(contriburl=rforge_contrib.url)
    pkgs <- avail_rforge[,1]
    pkgs_other <- setdiff(pkgs_all,pkgs)
  }

  ## Sort out packages that are on the exclude list
  pkgs <- remove_excluded_pkgs(pkgs, donotcompile)
  pkgs_other <- remove_excluded_pkgs(pkgs_other, donotcompile)

  
  ## TODO: Update Package Library How to resolve packages to be updated/installed
  ## update_package_library(cran.url = cran.url, libdir = libdir)

  
  ## test for build log dir and clean it
  check_directory(path_to_pkg_log, fix=TRUE)
  setwd(path_to_pkg_log)
  system(paste("rm -f *"))
  
  ## change to directory where the sources of R-Forge are in
  setwd(path_to_pkg_src)
  ## If we use a binary distribution like Windows or Mac we have to append
  ## the major version of R to the contrib directory
  if(platform!="Linux"){
    ##if(!any(dir("c:\\srv\\rsync\\R-Forge\\bin\\windows\\contrib\\")==maj.version))
    path_to_contrib_dir <- paste(path_to_pkg_root, "bin", .Platform$OS.type,
                                 "contrib", maj.version,
                                 sep=path_separator)
    check_directory(path_to_contrib_dir, fix=TRUE)
  }else path_to_contrib_dir <- paste(path_to_pkg_root, "src/contrib", sep=path_separator)

  ## delete 00LOCK, sometimes this interrupted the build process ...
  check_local_library(path_to_local_library)
  
  ## build packages 
  R <- paste(R.home(),"bin","R", sep=path_separator)
  setwd(path_to_pkg_src)

  ## LINUX BUILDS
  if(platform=="Linux"){
    for(pkg in pkgs){
      ## echo -n "${pkg}: " >> ${check_dir}/${R_flavor}/time_b.out
      ## /usr/bin/time -o ${check_dir}/${R_flavor}/time_b.out -a \
      ##env R_LIBS="${pkgs_library}" ${R_bin} CMD build ${pkg} \
      ##> ${R_buildlog_dir}/${pkg}-src-buildlog.txt
      ##done
      system(paste(R,"CMD build", pkg, ">",
                   paste(path_to_pkg_log,"\\",i, "-src-buildlog.txt", sep=""),
                   "2>&1"),
             invisible = TRUE)
    }
  }else if(platform=="Windows"){
    ## WINDOWS BUILDS
    for( i in pkgs ){
      system(paste(R,"CMD INSTALL --build", i, ">",
                   paste(path_to_pkg_log,"\\",i, "-win32-buildlog.txt", sep=""),
                   "2>&1"),
             invisible = TRUE)
    }
    ## build binaries which are not available as src tarball
    for( i in pkgs_other ){
      system(paste(R, "CMD INSTALL --build", i, ">",
                   paste(path_to_pkg_log,"\\", i, "-win32-buildlog.txt", sep=""),
                   "2>&1"),
             invisible = TRUE)
    }
  }else if(platform=="Mac"){
    ## MacOSX BUILDS
    warning("Not implemented yet")
  }else stop(paste("Strange platform: ", platform, "! I'm confused ...", sep = ""))

  
  ## provide built packages in corresponding contrib dir
  
  provide_packages_in_contrib(path_to_contrib_dir, platform)
  
  ## send email to R-Forge maintainer which packages successfully were built
  notify_admins(packages, donotcompile, email, send_host, server, mail_prog, relay_host)
    
}



## Perhaps for cleanup
## Clean /tmp dir
##  system("rm -rf c:\\tmp\\*")



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
