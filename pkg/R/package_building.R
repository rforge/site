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
##srcdir="c:\\srv\\R\\pkgs",      path_to_pkg_src
##buildlogdir="c:\\srv\\rsync\\R.check\\buildlogs", path_to_pkg_log
##winbindir=paste("c:\\srv\\rsync\\R-Forge\\bin\\windows\\contrib\\",maj.version,sep=""), path_to_binaries
##donotcompile = "c:\\srv\\R\\lib\\scripts\\DoNotCompile", stoplist
##email="stefan.theussl@wu-wien.ac.at" email


## update CRAN package repository
update_CRAN_packages <- function(cran.url, libdir){
  update.packages(lib=libdir,repos=cran.url,ask=FALSE)
}


## this is where the work is done
build_packages <- function(email,
                           path_to_pkg_src,
                           path_to_pkg_log,
                           path_to_binaries,
                           stoplist = NULL,
                           path_to_local_library = NULL,
                           platform=c("Windows", "MacOSX"),
                           architecture=c("x86_32", "x86_64"),
                           rforge_contrib.url="http://r-forge.r-project.org/src/contrib",
                           cran.url="http://cran.r-project.org",
                           control=list()){
  
  ## We need the package tools for creating PACKAGES i.e.
  library("tools")
  # Hard-coding fields (we are indexing by number, so hard-coding is important here!)
  fields <- c("Package", "Bundle", "Priority", "Version", "Depends", "Suggests", "Imports", "Contains")

  ## Update Package Library (TODO: automatic install of brandnew packages)
  update_CRAN_packages(cran.url=cran.url,libdir=libdir)

  ## STOP LIST: packages which should not be compiled
  donotcompile <- 
    if(file.exists(stoplist)){
      scan(stoplist, what = character(0)) 
    }else ""
  # what sourcepackages are available from R-Forge?
  avail_src <- dir(srcdir)
  pkgs_all <- avail_src
  avail_rforge <- available.packages(contriburl=rforge_contrib.url)
  pkgs <- avail_rforge[,1]
  pkgs_win <- setdiff(pkgs_all,pkgs)
  
  ## Sort out packages that is on the exclude list
  excluded <- sapply(pkgs, "[", 1) %in% donotcompile
  pkgs <- pkgs[!excluded]
  excluded <- sapply(pkgs_win, "[", 1) %in% donotcompile
  pkgs_win <- pkgs_win[!excluded]
  ##write(c(" ", "DoNotCompile:", "=============", brandnew[sdn]), 
  ##      file = Infofile, append = TRUE)
  ## clear build log dir
  setwd(buildlogdir)
  system(paste("rm -f *"))  
  ## change to directory where the sources of R-Forge are in
  setwd(srcdir)
  ## create dir 'maj.version' if not existing
  if(!any(dir("c:\\srv\\rsync\\R-Forge\\bin\\windows\\contrib\\")==maj.version))
    system(paste("mkdir",winbindir))
  ## delete 00LOCK, sometimes this interrupted the build process ...
  setwd(libdir)
  system(paste("rm -rf 00LOCK")) 


  cd ${R_FORGE}
rm -f ${R_buildlog_dir}/*-buildlog.txt
rforge_pkgs=`ls`

R_flavor=$R_build_flavor
R_bin=${R_dir}/bin/${R_flavor}
## custom install dir of packages
pkgs_library=${check_dir}/${R_flavor}/library



  ## build packages 
  if test -d $R_Forge_contrib_dir ; then
  cd ${R_FORGE}
  for pkg in ${rforge_pkgs} ; do
  ## for gforge we need lowercase pkg names [not needed anymore?]
  ## lc_pkg=`echo "${pkg}" | tr "[:upper:]" "[:lower:]"`
  ## building process
  echo -n "${pkg}: " >> ${check_dir}/${R_flavor}/time_b.out
  /usr/bin/time -o ${check_dir}/${R_flavor}/time_b.out -a \
  env R_LIBS="${pkgs_library}" ${R_bin} CMD build ${pkg} \
  > ${R_buildlog_dir}/${pkg}-src-buildlog.txt
  done




  ## build binaries WINDOWS
  setwd(srcdir)
  for( i in pkgs ){
    system(paste("Rcmd INSTALL --build", i, ">",
      paste(buildlogdir,"\\",i, "-win32-buildlog.txt", sep=""), "2>&1"),
      invisible = TRUE)
  }
  ## build binaries which are not available as src binary
  for( i in pkgs_win ){
    system(paste("Rcmd INSTALL --build", i, ">",
      paste(buildlogdir,"\\", i, "-win32-buildlog.txt", sep=""), "2>&1"),
      invisible = TRUE)
  }


  ## provide built packages in corresponding contrib dir
  rm -f ${R_Forge_contrib_dir}/*
  mv *.tar.gz ${R_Forge_contrib_dir}

  ## windows
  zipfiles <- dir()
  zipfiles <- zipfiles[grep(".zip",zipfiles)]
  for(i in zipfiles)
    file.copy(i, winbindir, overwrite = TRUE)
  ## remove old PACKAGES file
  setwd(winbindir)
  system(paste("rm -f PACKAGES*"))
  ## now find out which packages have old versions
  tmp<-dir()
  splitted <- strsplit(tmp, "_")
  packages <- sapply(splitted, "[", 1)
  ind <- 1:length(packages)
  versno <- unlist(strsplit(sapply(splitted, "[", 2), ".zip"))
  duplicated_pkgs <- duplicated(packages)
  if(any(duplicated_pkgs)){
  ## look for duplicated packages and remove older version
    for(i in packages[duplicated_pkgs]){     
      ind_package_to_remove <- ind[packages==i][version_order(versno[packages==i])[1]]
      system(paste("rm -f", tmp[ind_package_to_remove]))
    }
  } 
  ## Write a new PACKAGES and PACKAGES.gz file
  write_PACKAGES(dir = winbindir, fields = fields, type = "win.binary")
  ## Unix: wd <- getwd(); tools:::write_PACKAGES(wd,type="source")
  ## Under Windows additionally
  shell(paste("gzip -c", file.path(winbindir, "PACKAGES"), 
              ">", file.path(winbindir, "PACKAGES.gz")))
  
  
  ## send email to R-Forge maintainer which packages successfully build
  attachment <- "Rnightlybuildmail.txt"
  donotcompiletxt <- paste(donotcompile, collapse="\n")
  write(c("R-Forge Windows Build Log:", " ",
          "Disk status:", " ",
          system("df -h", intern = TRUE), " ",
          "This packages have been kept back (stop list):", " ",
          donotcompiletxt, " ",
          paste("The Windows binaries of the following",
          length(unique(packages)), "packages are now available on R-Forge:"), " ", unique(packages)),
          file = attachment)
  system(paste("sendEmail -f Rbuild@xmhera.wu-wien.ac.at -t", email,
              "-u \"R-Forge: Nightly build\" -m LOG -a", attachment,"-s statmath.wu-wien.ac.at"))
  system(paste("rm -f", attachment))  
}

build_binaries()



## Perhaps for cleanup
## Clean /tmp dir
##  system("rm -rf c:\\tmp\\*")

provide_packages_in_contrib <- function(platform, control){
  
}
