## The package build infrastructure on R-Forge

## We need the package tools for creating PACKAGES i.e.
library("tools")

## TODO LIST (there are more points in the functions):

## file URLs of local mirrors: contriburl = sprintf("file:///%s", dir)
  ## do not download from cran.r-prokect.org
## should we include a tmp directory
  ## if so a cleanup is necessary
  ## Clean /tmp dir
  ##  system("rm -rf c:\\tmp\\*")
## How can we get flavor from R environment
  ## can be useful when sending reportsa
## do we need the following things used in shell scripts
  ## R profile for checking.
  #R_profile=${R_scripts_dir}/check_profile.R
  #export _R_CHECK_WEAVE_VIGNETTES_ = no
  #export _R_CHECK_SUBDIRS_STRICT_ = yes
  ## Set permissions right
  #umask 022
  ## from the Windows build environment
  ## source("d:/Rcompile/CRANpkg/make/CRANbinaries.R")
  ## source("d:/Rcompile/CRANpkg/make/CRANcheckSummaryWin.R")
  ## source("d:/Rcompile/CRANpkg/make/maintainers.R")
  ##options(warn=1)


## this is where the work is done
## takes a control object containing control parameters, the platform,
## architecture, ... as arguments
build_packages <- function(email,
                           platform           = c("Linux", "Windows", "MacOSX"),
                           architecture       = c("x86_32", "x86_64"),
                           rforge_contrib_url = "http://r-forge.r-project.org/src/contrib",
                           cran_url           = "http://cran.r-project.org",
                           control=list()){

  ## INITIALIZATION
  
  ## match arguments
  platform <- match.arg(platform) ## FIXME: automatically use info from .Platform?
  architecture <- match.arg(architecture)
  maj.version <- paste(R.Version()$maj,unlist(strsplit(R.Version()$min,"[.]"))[1],sep=".")
  ## x86_32 on x86_64 allowed but not the other way round
  if((architecture=="x86_64") && (.Machine$sizeof.long == 4))
    stop("Building x86_64 binaries not possible on an x86_32 architecture") 
  ## handle different path separators
  path_separator <- c(unix = "/", windows = "\\")
  path_separator <- path_separator[.Platform$OS.type]
  ## check for necessary directories---create them if possible
  path_to_pkg_src <- control$path_to_pkg_src
  path_to_pkg_log <- control$path_to_pkg_log
  path_to_pkg_root <- control$path_to_pkg_root
  path_to_local_library <- control$path_to_local_library
  stoplist <- control$stoplist
  ## local package library
  if(!check_directory(path_to_local_library, fix=TRUE))
    stop(paste("There is no directory", dir,"!"))
  ## R-Forge package sources
  if(!check_directory(path_to_pkg_src))
    stop("Directory", path_to_pkg_src, "missing!")
  ## test for build log dir and clean it
  check_log_directory(path_to_pkg_log, path_separator, type = "build")
  ## check if package root directory (the directory containing
  ## the src/contrib or bin/windows/contrib) exists.
  if(!check_directory(path_to_pkg_root, fix=TRUE))
    stop(paste("There is no directory", dir,"!"))
  ## get current working directory -> set back at FINALIZATION step
  old_wd <- getwd()


  ## PACKAGE SIGHTING
  
  ## STOP LIST: packages which should not be compiled
  donotcompile <- if(file.exists(stoplist)){
    scan(stoplist, what = character(0)) 
  }else ""
  ## sourcepackages available from R-Forge---exported svn reps
  avail_src <- dir(path_to_pkg_src)
  pkgs_all <- pkgs <- avail_src
  ## platform specific packages or pkgs not avail as src tarball but
  ## can be exported from SVN repository (indicator for Windows or Mac package ?)
  pkgs_other = ""
  if(platform != "Linux"){
    avail_rforge <- available.packages(contriburl = rforge_contrib.url)
    pkgs <- avail_rforge[, 1]
    pkgs_other <- setdiff(pkgs_all, pkgs)
  }
  ## Sort out packages that are on the exclude list
  pkgs <- remove_excluded_pkgs(pkgs, donotcompile)
  pkgs_other <- remove_excluded_pkgs(pkgs_other, donotcompile)
  
  ## PACKAGE DB UPDATE

  ## FIXME: is it sufficient what we are doing here?

  update_package_library(pkgs, path_to_pkg_src, cran_url, lib)

  ## LAST PREPARATION BEFORE PACKAGE BUILD
  
  ## change to directory where the sources of R-Forge are in
  setwd(path_to_pkg_src)
  ## set up CONTRIB directory
  ## If we use a BINARY distribution like WINDOWS or MAC we have to append
  ## the major version of R to the contrib directory otherwise use /src/contrib
  if(platform!="Linux"){
    ##if(!any(dir("c:\\srv\\rsync\\R-Forge\\bin\\windows\\contrib\\")==maj.version))
    path_to_contrib_dir <- paste(path_to_pkg_root, "bin", .Platform$OS.type,
                                 "contrib", maj.version,
                                 sep=path_separator)
  }else {
    ## UNIX SOURCE directory
    path_to_contrib_dir <- paste(path_to_pkg_root, "src/contrib", sep=path_separator)
  }
  if(!check_directory(path_to_contrib_dir, fix=TRUE, recursive=TRUE))
      stop(paste("There is no directory", dir,"!"))
  ## delete 00LOCK, sometimes this interrupted the build process ...
  check_local_library(path_to_local_library)
  ## where is our R binary?
  R <- paste(R.home(), "bin", "R", sep=path_separator)
  setwd(path_to_pkg_src)
  ## Set environment variables which are necessary for building (or creating vignettes)
  Sys.setenv(R_LIBS = path_to_local_lib)
  
  ## PACKAGE BUILDING

  ## TODO: Timings
  ## LINUX BUILDS
  if(platform=="Linux"){
    ## We need a virtual framebuffer
    pid <- start_virtual_X11_fb()
    ## Set TEXMFLOCAL environment variables in case we have
    ## personalized style files (building vignettes)
    path_to_local_texmf <- control$path_to_local_texmf
    if(file.exists(path_to_local_texmf))
      Sys.setenv(TEXMFLOCAL=path_to_local_texmf)
    for(pkg in pkgs){
      system(paste(R,"CMD build", pkg, ">",
                   paste(path_to_pkg_log, path_separator, pkg, "-src-buildlog.txt" , sep=""),
                   "2>&1"))
    }
    close_virtual_X11_fb(pid)
  }else if(platform=="Windows"){
    ## WINDOWS BUILDS
    for( pkg in pkgs ){
      system(paste(R,"CMD INSTALL --build", pkg, ">",
                   paste(path_to_pkg_log, path_separator, pkg, "-win-",
                         architecture, "-buildlog.txt", sep=""),
                   "2>&1"),
             invisible = TRUE)
    }
    ## build binaries which are not available as src tarball (maybe Windows binaries)
    for( i in pkgs_other ){
      system(paste(R, "CMD INSTALL --build", pkg, ">",
                   paste(path_to_pkg_log, path_separator, pkg, "-mac-",
                         architecture, "-buildlog.txt", sep=""),
                   "2>&1"),
             invisible = TRUE)
    }
  }else if(platform=="MacOSX"){
    ## MacOSX BUILDS
    stop("Not implemented yet!")
  }else stop(paste("Strange platform: ", platform, "! I'm confused ...", sep = ""))

  ## FINAL STEPS
  
  ## provide built packages in corresponding contrib dir
  pkgs_provided <- provide_packages_in_contrib(path_to_pkg_src, path_to_contrib_dir, platform)
  ## send email to R-Forge maintainer which packages successfully were built
  notify_admins(pkgs_provided, donotcompile, email, platform, control)
  ## go back to old working directory
  setwd(old_wd)
  TRUE
}
