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
                           rforge_url         = "http://R-Forge.R-project.org",
                           cran_url           = "http://CRAN.R-project.org",
                           bioc_url           = "http://bioconductor.org/packages/release/bioc",
                           control            = list()){

  ## INITIALIZATION
  writeLines("Start build process ...")
  ## match arguments
  platform <- match.arg(platform) ## FIXME: automatically use info from .Platform?
  architecture <- match.arg(architecture)
  maj.version <- paste(R.Version()$maj,unlist(strsplit(R.Version()$min,"[.]"))[1],sep=".")
  ## x86_32 on x86_64 allowed but not the other way round
  if((architecture=="x86_64") && (.Machine$sizeof.long == 4))
    stop("Building x86_64 binaries not possible on an x86_32 architecture") 
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
  check_log_directory(path_to_pkg_log, type = "build")
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
  ## write 
  if(!is.null(donotcompile)){
  
  }
  
  ## sourcepackages available from R-Forge---exported svn reps
  pkgs_all <- available.packages(contriburl =
                                         sprintf("file:///%s", path_to_pkg_src))[, 1]
  ## platform specific packages or pkgs not avail as src tarball but
  ## can be exported from SVN repository (indicator for Windows or Mac package ?)
  pkgs_other = ""
  if(platform != "Linux"){
    ## where are the source tarballs already available from R-Forge?
    path_to_pkg_tarballs <- control$path_to_pkg_tarballs
    if(!check_directory(path_to_pkg_tarballs))
      stop("Directory", path_to_pkg_tarballs, "missing!") 
    avail_rforge <- available.packages(contriburl = contrib.url(paste("file:", path_to_pkg_tarballs, sep = ""), type = "source"))
    avail_src_pkgs <- avail_rforge[, 1]
    ## FIXME: what if there are packages as tarball available which doesn't exist as pure source?!
    ##        see also Windows building below (the try construct)
    pkgs_other <- setdiff(pkgs_all, avail_src_pkgs)
  }
  ## Sort out packages that are on the exclude list
  pkgs <- remove_excluded_pkgs(pkgs_all, donotcompile)
  pkgs_other <- remove_excluded_pkgs(pkgs_other, donotcompile)
  
  ## PACKAGE DB UPDATE

  ## FIXME: is it sufficient what we are doing here?
  update_package_library(pkgs, path_to_pkg_src, c(cran_url, bioc_url), path_to_local_library, platform)

  ## LAST PREPARATION BEFORE PACKAGE BUILD
  
  ## change to directory where the sources of R-Forge are in
  setwd(path_to_pkg_src)
  ## set up CONTRIB directory
  ## If we use a BINARY distribution like WINDOWS or MAC we have to append
  ## the major version of R to the contrib directory otherwise use /src/contrib
  if(platform == "Windows"){
    ##if(!any(dir("c:\\srv\\rsync\\R-Forge\\bin\\windows\\contrib\\")==maj.version))
    path_to_contrib_dir <- file.path(path_to_pkg_root, "bin", .Platform$OS.type,
                                 "contrib", maj.version)
  }else if(platform == "MacOSX"){
    path_to_contrib_dir <- file.path(path_to_pkg_root, "bin", "macosx", "universal",
                                 "contrib", maj.version)
  }else {
    ## UNIX SOURCE directory
    path_to_contrib_dir <- file.path(path_to_pkg_root, "src/contrib")
  }
  if(!check_directory(path_to_contrib_dir, fix=TRUE, recursive=TRUE))
      stop(paste("There is no directory", dir,"!"))
  ## delete 00LOCK, sometimes this interrupted the build process ...
  check_local_library(path_to_local_library)
  ## where is our R binary?
  R <- file.path(R.home(), "bin", "R")
  ## Set environment variables which are necessary for building (or creating vignettes)
  Sys.setenv(R_LIBS = path_to_local_library)

  ##############################################################################
  ## PACKAGE BUILDING
  ##############################################################################

  ## LINUX BUILDS ##############################################################
  if(platform=="Linux"){
    ## We need a virtual framebuffer
    pid <- start_virtual_X11_fb()
    ## Set TEXMFLOCAL environment variables in case we have
    ## personalized style files (building vignettes)
    path_to_local_texmf <- control$path_to_local_texmf
    if(file.exists(path_to_local_texmf))
      Sys.setenv(TEXMFLOCAL=path_to_local_texmf)
    ## Initialize timings
    timings <- numeric(length(pkgs))
    names(timings) <- pkgs
    for(pkg in pkgs){
      ## Prolog
      pkg_buildlog <- get_buildlog(path_to_pkg_log, pkg, platform, architecture = "all")
      write_prolog(pkg, pkg_buildlog, path_to_pkg_src, type = "build", what = "tarball", std.out = TRUE)

      ## BUILD
      timings[pkg] <- system.time(system(paste(R, "CMD build", pkg, 
                                               ">>", pkg_buildlog, "2>&1")))["elapsed"]

      ## Epilog
      write_epilog(pkg_buildlog, timings[pkg], std.out = TRUE)
    }
    close_virtual_X11_fb(pid)
  }else if(platform == "Windows"){
    ## WINDOWS BUILDS ##########################################################
    ## Initialize timings
    timings <- numeric(length(pkgs))
    names(timings) <- pkgs
    
    for( pkg in avail_src_pkgs ){
      ## Prolog
      pkg_buildlog <- get_buildlog(path_to_pkg_log, pkg, platform, architecture)
      write_prolog(pkg, pkg_buildlog, path_to_pkg_src, type = "build", what = "binary", std.out = TRUE)

      ## BUILD
      ## timer start
      proc_start <- proc.time()

      ## look out for version number	
      try(pkg_version_local <- packageDescription(pkg, lib.loc = ".")$Version, silent = TRUE)
      if(inherits(pkg_version_local, "try-error"))
        pkg_version_local <- "0.0"
      pkg_version_src   <- avail_rforge[Package = pkg, "Version"]
      ## if the version of available src tarball is the same (or newer) than local sources
      ## then build from it (as it already contains the package vignette).
      if(package_version(pkg_version_src) >= package_version(pkg_version_local)){
      system(paste(paste(R, "cmd", sep=""), "INSTALL --build", 
                   file.path(path_to_pkg_tarballs, "src", "contrib",
                             paste(pkg, "_", pkg_version_src, ".tar.gz", sep = "")),
                   ">>", pkg_buildlog, "2>&1"), invisible = TRUE)
      }else { 
      ## Otherwise it is a brandnew version and we build it directly from source
      ## first we have to build the tarball (important for vignettes)
      system(paste(paste(R, "cmd", sep = ""), "build", pkg, 
                   ">>", pkg_buildlog, "2>&1"), invisible = TRUE)
      ## then build the binary
      system(paste(paste(R, "cmd", sep = ""), "INSTALL --build", 
                   paste(pkg, "_", pkg_version_local, ".tar.gz", sep = ""), 
                   ">>", pkg_buildlog, "2>&1"), invisible = TRUE)
      ## and finally delete the tarball
      file.remove(paste(pkg, "_", pkg_version_local, ".tar.gz", sep = ""))              
      }
      ## save timing
      timings[pkg] <- c(proc.time() - proc_start)["elapsed"]
      
      ## Epilog
      write_epilog(pkg_buildlog, timings[pkg], std.out = TRUE)
    }
    ## build binaries which are not available as src tarball (maybe Windows binaries)
    for( pkg in pkgs_other ){
      ## Prolog
      pkg_buildlog <- get_buildlog(path_to_pkg_log, pkg, platform, architecture)
      write_prolog(pkg, pkg_buildlog, path_to_pkg_src, type = "build", what = "binary", std.out = TRUE)

      ## timer start
      proc_start <- proc.time()

      ## does the pkg exist?
      if(!file.exists(pkg))
        next
      ## look out for version number
      pkg_version_local <- packageDescription(pkg, lib.loc = ".")$Version
      
      ## first we have to build the tarball (important for vignettes)
      system(paste(paste(R, "cmd", sep = ""), "build", pkg, 
                   ">>", pkg_buildlog, "2>&1"), invisible = TRUE)
      system(paste(paste(R, "cmd", sep = ""), "INSTALL --build", 
                   paste(pkg, "_", pkg_version_local, ".tar.gz", sep = ""), 
                   ">>", pkg_buildlog, "2>&1"), invisible = TRUE)
      ## save timing
      timings[pkg] <- c(proc.time() - proc_start)["elapsed"]

      ## Epilog
      write_epilog(pkg_buildlog, timings[pkg], std.out = TRUE)
    }
    ## delete 00LOCK, sometimes this interrupted the build process ...
    check_local_library(path_to_local_library)
  }else if(platform == "MacOSX"){
    ## MacOSX BUILDS ###########################################################
    ## Set TEXMFLOCAL environment variables in case we have
    ## personalized style files (building vignettes)
    path_to_local_texmf <- control$path_to_local_texmf
    if(file.exists(path_to_local_texmf))
      Sys.setenv(TEXMFLOCAL = path_to_local_texmf)
    ## Initialize timings
    timings <- numeric(length(avail_src_pkgs))
    names(timings) <- avail_src_pkgs

    ## BUILDING FROM PKG TARBALLS
    for(pkg in avail_src_pkgs){
      ## Prolog
      pkg_buildlog <- get_buildlog(path_to_pkg_log, pkg, platform, architecture = "all")
      write_prolog(pkg, pkg_buildlog, path_to_pkg_src, type = "build", what = "binary", std.out = TRUE)

      ## timer start
      proc_start <- proc.time()
      ## path to pkg buildlog 

      ## look out for version number	
      try(pkg_version_local <- packageDescription(pkg, lib.loc = ".")$Version, silent = TRUE)
      if(inherits(pkg_version_local, "try-error"))
        pkg_version_local <- "0.0"
      pkg_version_src   <- avail_rforge[Package = pkg, "Version"]
      ## if the version of available src tarball is the same (or newer) than local sources
      ## then build from it (as it already contains the package vignette).
      if(package_version(pkg_version_src) >= package_version(pkg_version_local)){
            ## make temporary directory
	    tmpdir <- paste(sample(c(letters, 0:9), 10, replace = TRUE), collapse = "")
      	    check_directory(tmpdir, fix = TRUE)
      	    ## first look if there is a src directory because then we know that we have
      	    ## to compile something ...
      	    if(file.exists(file.path(".", pkg, "src"))){
      	      ## compile an x86_32 binary
      	      system(paste("R_ARCH=/i386", R, "CMD INSTALL -l", tmpdir, 
	      file.path(path_to_pkg_tarballs, "src", "contrib", paste(pkg, "_",
                         pkg_version_src, ".tar.gz", sep = "")),
                     ">>", pkg_buildlog, "2>&1"))
      	      ## compile a PPC binary
      	      system(paste("R_ARCH=/ppc", R, "CMD INSTALL -l", tmpdir, "--libs-only", 
	      		 file.path(path_to_pkg_tarballs, "src", "contrib", paste(pkg, "_",
                         pkg_version_src, ".tar.gz", sep = "")), 
                     ">>", pkg_buildlog, "2>&1"))

      	    }else {
              ## R only packages can be installed in one rush
              system(paste(R, "CMD INSTALL -l", tmpdir, 
                         file.path(path_to_pkg_tarballs, "src", "contrib", paste(pkg, "_",
                         pkg_version_src, ".tar.gz", sep = "")), 
                     ">>", pkg_buildlog, "2>&1"))
            }
      	    ## combine everything to universal binary
      	    if(file.exists(file.path(tmpdir, pkg, "DESCRIPTION"))){
              system(paste("tar czvf", paste(pkg, "_", pkg_version_src, ".tgz", sep = ""), 
                     "-C", tmpdir, pkg, 
                     ">>", pkg_buildlog, "2>&1"))
            }
      	    ## remove temporary directory	
      	    system(paste("rm -rf", tmpdir))

      }else {
        ## Otherwise it is a brandnew version and we build it directly from local source
        ## first we have to build the tarball (important for vignettes)
        system(paste(R, "CMD", "build", pkg, 
                   ">>", pkg_buildlog, "2>&1"), invisible = TRUE)
        ## then build the binary
        ## make temporary directory
	tmpdir <- paste(sample(c(letters, 0:9), 10, replace = TRUE), collapse = "")
      	check_directory(tmpdir, fix = TRUE)
      	## first look if there is a src directory because then we know that we have
      	## to compile something ...
      	if(file.exists(file.path(".", pkg, "src"))){
      	      ## compile an x86_32 binary
      	      system(paste("R_ARCH=/i386", R, "CMD INSTALL -l", tmpdir, 
	             paste(pkg, "_", pkg_version_local, ".tar.gz", sep = ""),
                     ">>", pkg_buildlog, "2>&1"))
      	      ## compile a PPC binary
      	      system(paste("R_ARCH=/ppc", R, "CMD INSTALL -l", tmpdir, "--libs-only", 
	      	     paste(pkg, "_", pkg_version_local, ".tar.gz", sep = ""), 
                     ">>", pkg_buildlog, "2>&1"))

      	}else {
              ## R only packages can be installed in one rush
              system(paste(R, "CMD INSTALL -l", tmpdir, 
                     paste(pkg, "_", pkg_version_local, ".tar.gz", sep = ""), 
                     ">>", pkg_buildlog, "2>&1"))
        }
      	## combine everything to universal binary
      	if(file.exists(file.path(tmpdir, pkg, "DESCRIPTION"))){
              system(paste("tar czvf", paste(pkg, "_", pkg_version_local, ".tgz", sep = ""), 
                     "-C", tmpdir, pkg, 
                     ">>", pkg_buildlog, "2>&1"))
        }
      	## remove temporary directory	
      	system(paste("rm -rf", tmpdir))
	
        ## and finally delete the tarball
        file.remove(paste(pkg, "_", pkg_version_local, ".tar.gz", sep = ""))
      }
      ## save timing
      timings[pkg] <- c(proc.time() - proc_start)["elapsed"]

      ## Epilog
      write_epilog(pkg_buildlog, timings[pkg], std.out = TRUE)
    } #</FOR>
    ## BUILDING FROM SOURCES
    ## build binaries which are not available as src tarball (maybe MacOS binaries)
    for( pkg in pkgs_other ){
      ## Prolog
      pkg_buildlog <- get_buildlog(path_to_pkg_log, pkg, platform, architecture = "all")
      write_prolog(pkg, pkg_buildlog, path_to_pkg_src, type = "build", what = "binary", std.out = TRUE)

      ## timer start
      proc_start <- proc.time()
      
      ## does the pkg exist?
      if(!file.exists(pkg))
        next
      ## look out for version number
      pkg_version_local <- packageDescription(pkg, lib.loc = ".")$Version
      ## first we have to build the tarball (important for vignettes)
      system(paste(R, "CMD", "build", pkg, 
                   ">>", pkg_buildlog, "2>&1"), invisible = TRUE)
      ## make temporary directory
      tmpdir <- paste(sample(c(letters, 0:9), 10, replace = TRUE), collapse = "")
      check_directory(tmpdir, fix = TRUE)
      ## first look if there is a src directory because then we know that we have
      ## to compile something ...
      if(file.exists(file.path(".", pkg, "src"))){
      	      ## compile an x86_32 binary
      	      system(paste("R_ARCH=/i386", R, "CMD INSTALL -l", tmpdir, 
	      paste(pkg, "_", pkg_version_local, ".tar.gz", sep = ""),
                     ">>", pkg_buildlog, "2>&1"))
      	      ## compile a PPC binary
      	      system(paste("R_ARCH=/ppc", R, "CMD INSTALL -l", tmpdir, "--libs-only", 
	      		 paste(pkg, "_", pkg_version_local, ".tar.gz", sep = ""), 
                     ">>", pkg_buildlog, "2>&1"))

      }else {
              ## R only packages can be installed in one rush
              system(paste(R, "CMD INSTALL -l", tmpdir, 
                         paste(pkg, "_", pkg_version_local, ".tar.gz", sep = ""), 
                     ">>", pkg_buildlog, "2>&1"))
      }
      ## combine everything to universal binary
      if(file.exists(file.path(tmpdir, pkg, "DESCRIPTION"))){
              system(paste("tar czvf", paste(pkg, "_", pkg_version_local, ".tgz", sep = ""), 
                     "-C", tmpdir, pkg, 
                     ">>", pkg_buildlog, "2>&1"))
      }
      ## remove temporary directory	
      system(paste("rm -rf", tmpdir))
	
      ## and finally delete the tarball
      file.remove(paste(pkg, "_", pkg_version_local, ".tar.gz", sep = ""))
      ## save timing
      timings[pkg] <- c(proc.time() - proc_start)["elapsed"]

      ## Epilog
      write_epilog(pkg_buildlog, timings[pkg], std.out = TRUE)
    } #</FOR>
  }else stop(paste("Strange platform: ", platform, "! I'm confused ...", sep = ""))

  ## FINAL STEPS
  writeLines("Send email to R-Forge maintainer and cleanup ...")
  ## provide built packages in corresponding contrib dir
  pkgs_provided <- provide_packages_in_contrib(path_to_pkg_src, path_to_contrib_dir, platform)
  ## send email to R-Forge maintainer which packages successfully were built
  notify_admins(pkgs_provided, donotcompile, email, platform, control, 
                about = "build", timings = timings)
  ## go back to old working directory
  setwd(old_wd)
  writeLines("Done.")
  TRUE
}
