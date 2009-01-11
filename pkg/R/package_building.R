## The package build infrastructure on R-Forge

## We need the package tools for creating PACKAGES i.e.
library("tools")

## FIXME: unifying architectures
## I.e., using a control object containing all relevant information and looping over all packages
## deciding at the function call for the build which architecture to use.

## TODO LIST (there are more points in Uwe's functions):

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

  if(!inherits(control, "R-Forge_control"))
    stop("No R-Forge control object given")
  ## INITIALIZATION
  writeLines("Start build process ...")
  ## match arguments
  platform <- match.arg(platform) ## FIXME: automatically use info from .Platform?
  architecture <- match.arg(architecture)
  maj.version <- paste(R.Version()$maj, unlist(strsplit(R.Version()$min, "[.]"))[1], sep=".")
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
  ## FIXME: probably too strict at the moment. E.g., RWinEdt does not get build
  ## when checking packages the stoplist includes additional arguments to check process
  if(file.exists(stoplist)){
    check_args <- read.csv(stoplist, stringsAsFactors = FALSE)
  }else check_args <- NULL
  no_install <- grep("--install=no", check_args[["check_args"]])
  donotcompile <- check_args[no_install, "Package"]
  no_vignettes <- check_args[grep("--no-vignettes", check_args[["check_args"]]), "Package"]

  if( length(donotcompile) ){
    for(pkg in donotcompile){
      arch <- "all"
      if(platform == "Windows")
        arch <- architecture
      pkg_buildlog <- get_buildlog(path_to_pkg_log, pkg, platform, architecture = arch)
      write_stoplist_notification(pkg, pkg_buildlog, "build", std.out = TRUE)
    }
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
    avail_rforge <- available.packages(contriburl = contrib.url(paste("file:", path_to_pkg_tarballs, sep = ""), type = "source"), fields = "Revision")
    avail_src_pkgs <- avail_rforge[, 1]
    ## we take only tarballs into account which are hosted in R-Forge SVN reps
    avail_src_pkgs <- avail_src_pkgs[avail_src_pkgs %in% pkgs_all]
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
    path_to_contrib_dir <- file.path(path_to_pkg_root, "bin", "macosx", "leopard",
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
  ## Set TEXMFLOCAL environment variables in case we have
  ## personalized style files (building vignettes)
  path_to_local_texmf <- control$path_to_local_texmf
  if(file.exists(path_to_local_texmf))
    Sys.setenv(TEXMFLOCAL=path_to_local_texmf)

  ##############################################################################
  ## PACKAGE BUILDING
  ##############################################################################

  ## LINUX BUILDS ##############################################################
  if(platform == "Linux"){
    ## We need a virtual framebuffer
    pid <- start_virtual_X11_fb()

    ## Initialize timings
    timings <- numeric(length(pkgs))
    names(timings) <- pkgs
    
    ## Building ...
    for(pkg in pkgs){
      ## Prolog
      pkg_buildlog <- get_buildlog(path_to_pkg_log, pkg, platform, architecture = "all")
      write_prolog(pkg, pkg_buildlog, path_to_pkg_src, type = "build", what = "tarball", std.out = TRUE)
      
      build_args <- if(pkg %in% no_vignettes)
        "--no-vignettes" else ""

      ## BUILD
      timings[pkg] <- system.time(
                                  .build_tarball_from_sources_linux(pkg, R, pkg_buildlog, build_args)
      )["elapsed"]

      ## Epilog
      write_epilog(pkg_buildlog, timings[pkg], std.out = TRUE)
    }
    
    ## Cleanup
    close_virtual_X11_fb(pid)

  ## WINDOWS BUILDS ##########################################################
  }else if(platform == "Windows"){

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

      ## look out for version and revision number	
      pkg_version_local <- get_package_version_from_sources(pkg)
      pkg_version_src   <- avail_rforge[Package = pkg, "Version"]
      pkg_revision_local <- get_package_revision_from_sources(pkg)
      pkg_revision_src  <- avail_rforge[Package = pkg, "Revision"]
      
      ## if the version of available src tarball is the same (or newer) than local sources
      ## then build from it (as it already contains the package vignette).
      if( pkg_revision_src >= pkg_revision_local ){

        .build_binary_from_tarball_win(pkg, pkg_version_src, path_to_pkg_tarballs, R, pkg_buildlog)     
      
      ## Otherwise it is a brandnew version and we build it directly from uncompressed package sources    
      } else { 
        
	 build_args <- if(pkg %in% no_vignettes)
        "--no-vignettes" else ""
	
        .build_binary_from_sources_win(pkg, pkg_version_local, R, pkg_buildlog, build_args)

      }
      ## save timing
      timings[pkg] <- c(proc.time() - proc_start)["elapsed"]
      
      ## Epilog
      write_epilog(pkg_buildlog, timings[pkg], std.out = TRUE)
    }
    
    ## build binaries which are not available as src tarball (maybe Windows only packages?)
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
      pkg_version_local <- get_package_version_from_sources(pkg)
      
      build_args <- if(pkg %in% no_vignettes)
        "--no-vignettes" else ""
	
      .build_binary_from_sources_win(pkg, pkg_version_local, R, pkg_buildlog, build_args)

      ## save timing
      timings[pkg] <- c(proc.time() - proc_start)["elapsed"]

      ## Epilog
      write_epilog(pkg_buildlog, timings[pkg], std.out = TRUE)
    }

    ## Cleanup
    ## delete 00LOCK, sometimes this interrupted the build process ...
    check_local_library(path_to_local_library)

  ## MacOSX BUILDS ###########################################################
  }else if(platform == "MacOSX"){

    ## We need a virtual framebuffer
    pid <- start_virtual_X11_fb()

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
      pkg_version_local <- get_package_version_from_sources(pkg)
      pkg_version_src   <- avail_rforge[Package = pkg, "Version"]
      pkg_revision_local <- get_package_revision_from_sources(pkg)
      pkg_revision_src  <- avail_rforge[Package = pkg, "Revision"]

      ## if the version of available src tarball is equal (or newer than) the version of local sources
      ## then build from it (as it already contains the package vignette).
      if( pkg_revision_src >= pkg_revision_local ){

        .build_binary_from_tarball_mac(pkg, pkg_version_src, path_to_pkg_tarballs, R, pkg_buildlog)

      ## Otherwise it is a brandnew version and we build it directly from local source
      } else {
      
        build_args <- if(pkg %in% no_vignettes)
          "--no-vignettes" else ""

        .build_binary_from_sources_mac(pkg, pkg_version_local, R, pkg_buildlog, build_args)

      }
      ## save timing
      timings[pkg] <- c(proc.time() - proc_start)["elapsed"]

      ## Epilog
      write_epilog(pkg_buildlog, timings[pkg], std.out = TRUE)
    } #</FOR>

    ## BUILDING FROM SOURCES
    ## build binaries which are not available as src tarball (MacOS only packages?)
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
      pkg_version_local <- get_package_version_from_sources(pkg)

      build_args <- if(pkg %in% no_vignettes)
        "--no-vignettes" else ""


      .build_binary_from_sources_mac(pkg, pkg_version_local, R, pkg_buildlog, build_args)

      ## save timing
      timings[pkg] <- c(proc.time() - proc_start)["elapsed"]

      ## Epilog
      write_epilog(pkg_buildlog, timings[pkg], std.out = TRUE)

    } #</FOR>

    ## Cleanup: close framebuffer
    close_virtual_X11_fb(pid)
  } else stop(paste("Strange platform: ", platform, "! I'm confused ...", sep = ""))

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
  invisible(TRUE)
}

## OS: Linux (would also work on other POSIX systems?)
## input: uncompressed package sources (the exported pkg directories) 
## output: compressed package sources <package_name>_<version>.tar.gz
## FIXME: currently sources and resulting tarball are in the current working dir
.build_tarball_from_sources_linux <- function(pkg, R, pkg_buildlog, build_args = ""){
  system(paste(R, "CMD build", build_args, pkg, 
               ">>", pkg_buildlog, "2>&1"))
  pkg_version <- get_package_version_from_sources(pkg)
  invisible(paste(pkg, "_", pkg_version, ".tar.gz", sep = ""))
}

## OS: Windows
## input: uncompressed package sources (the exported pkg directories) 
## output: compressed package binary <package_name>_<version>.zip
## FIXME: currently sources and resulting tarball are in the current working dir
.build_binary_from_sources_win <- function(pkg, pkg_version, R, pkg_buildlog, build_args = ""){
  ## first we have to build the tarball (important for vignettes)
  system(paste(paste(R, "cmd", sep = ""), "build", build_args, pkg, 
                   ">>", pkg_buildlog, "2>&1"), invisible = TRUE)
  ## then build the binary
  system(paste(paste(R, "cmd", sep = ""), "INSTALL --build", 
                   paste(pkg, "_", pkg_version, ".tar.gz", sep = ""), 
                   ">>", pkg_buildlog, "2>&1"), invisible = TRUE)
  ## and finally delete the tarball
  file.remove(paste(pkg, "_", pkg_version, ".tar.gz", sep = ""))
  invisible(paste(pkg, "_", pkg_version, ".zip", sep = ""))
}

## OS: Windows
## input: package tarball (<package_name>_<version>.tar.gz)
## output: compressed package binary <package_name>_<version>.zip
## FIXME: currently sources and resulting tarball are in the current working dir
.build_binary_from_tarball_win <- function(pkg, pkg_version, path_to_pkg_tarballs, R, pkg_buildlog){
  system(paste(paste(R, "cmd", sep=""), "INSTALL --build", 
               file.path(path_to_pkg_tarballs, "src", "contrib",
                         paste(pkg, "_", pkg_version, ".tar.gz", sep = "")),
               ">>", pkg_buildlog, "2>&1"), invisible = TRUE)
  invisible(paste(pkg, "_", pkg_version, ".zip", sep = ""))
}

## OS: Mac OS X
## input: uncompressed package sources (the exported pkg directories) 
## output: compressed package binary <package_name>_<version>.tgz
## FIXME: currently sources and resulting tarball are in the current working dir
.build_binary_from_sources_mac <- function(pkg, pkg_version, R, pkg_buildlog, build_args = ""){
  ## first we have to build the tarball (important for vignettes)
  pkg_tarball <- .build_tarball_from_sources_linux(pkg, R, pkg_buildlog, build_args)

  ## make temporary directory
  tmpdir <- .make_tmp_directory()

  ## first look if there is a src directory because then we know that we have
  ## to compile something ...
  if(.check_whether_package_code_contains_makefile_or_configure(pkg)){
    ## compile an x86_32 binary
    system(paste("R_ARCH=/i386", R, "CMD INSTALL -l", tmpdir, 
	             pkg_tarball, ">>", pkg_buildlog, "2>&1"))
    ## compile a PPC binary
    system(paste("R_ARCH=/ppc", R, "CMD INSTALL -l", tmpdir, "--libs-only", 
	      	     pkg_tarball, ">>", pkg_buildlog, "2>&1"))

  }else {
    ## R only packages can be installed in one rush
    system(paste(R, "CMD INSTALL -l", tmpdir, 
                     pkg_tarball, ">>", pkg_buildlog, "2>&1"))
  }
  
  ## combine everything to universal binary
  pkg_binary <- .make_universal_mac_binary(pkg, pkg_version,  pkg_buildlog, tmpdir)

  ## remove temporary directory
  .cleanup_mac(tmpdir)

  ## and finally delete the tarball
  file.remove(pkg_tarball)
  
  invisible(pkg_binary)
}

## OS: Mac OSX
## input: package tarball (<package_name>_<version>.tar.gz)
## output: compressed package binary <package_name>_<version>.tgz
## FIXME: currently sources and resulting tarball are in the current working dir
.build_binary_from_tarball_mac <- function(pkg, pkg_version, path_to_pkg_tarballs, R, pkg_buildlog){
  ## make temporary directory
  tmpdir <- .make_tmp_directory()

  ## first look if there is a src directory because then we know that we have
  ## to compile something ...
  if(.check_whether_package_code_contains_makefile_or_configure(pkg)){
    ## compile an x86_32 binary
    system(paste("R_ARCH=/i386", R, "CMD INSTALL -l", tmpdir, 
                 file.path(path_to_pkg_tarballs, "src", "contrib", paste(pkg, "_",
                 pkg_version, ".tar.gz", sep = "")),
                 ">>", pkg_buildlog, "2>&1"))
    ## compile a PPC binary
    system(paste("R_ARCH=/ppc", R, "CMD INSTALL -l", tmpdir, "--libs-only", 
   	         file.path(path_to_pkg_tarballs, "src", "contrib", paste(pkg, "_",
                 pkg_version, ".tar.gz", sep = "")), 
                ">>", pkg_buildlog, "2>&1"))

  }else {
    ## R only packages can be installed in one rush
    system(paste(R, "CMD INSTALL -l", tmpdir, 
                 file.path(path_to_pkg_tarballs, "src", "contrib", paste(pkg, "_",
                 pkg_version, ".tar.gz", sep = "")), 
                 ">>", pkg_buildlog, "2>&1"))
  }
  
  pkg_binary <- .make_universal_mac_binary(pkg, pkg_version, pkg_buildlog, tmpdir)

  ## Cleanup
  .cleanup_mac(tmpdir)

  invisible(pkg_binary)
}

.make_tmp_directory <- function(where = "."){
  dirname <- paste(sample(c(letters, 0:9), 10, replace = TRUE), collapse = "")
  check_directory(file.path(where, dirname), fix = TRUE)
  dirname
}

## simple check if there is an src directory
.check_whether_package_contains_code_to_compile <- function(pkg, dir = "."){
  if(!file.exists(file.path(dir, pkg)))
    warning(paste("Package", pkg, "does not exist in", dir, "!"))
  file.exists(file.path(dir, pkg, "src"))
}
	
## check if package has a Makefile or a configure script,
## necessary for building mac packages -> Do we have to set 'arch=' variable?
.check_whether_package_code_contains_makefile_or_configure <- function(pkg, dir = "."){
  if(!file.exists(file.path(dir, pkg)))
    warning(paste("Package", pkg, "does not exist in", dir, "!"))
  files <- c("Makefile", "configure")
  files_to_test <- c(file.path(dir, pkg, files), file.path(dir, pkg, "src", files))
  any(file.exists(files_to_test))
}

## checks if there is an installed package in the given path and builds the .tgz
.make_universal_mac_binary <- function(pkg, pkg_version,  pkg_buildlog, dir = "."){
  if(file.exists(file.path(dir, pkg, "DESCRIPTION"))){
    system(paste("tar czvf", paste(pkg, "_", pkg_version, ".tgz", sep = ""), 
                 "-C", dir, pkg, 
                 ">>", pkg_buildlog, "2>&1"))
    return(paste(pkg, "_", pkg_version, ".tgz", sep = ""))
  }
  NA
}

.cleanup_mac <- function(dir){
  if( file.exists( file.path(".", dir)) )
    system( paste("rm -rf", file.path(".", dir)) )
}

get_package_version_from_sources <- function(pkg, library = ".") 
  sapply(pkg, .get_package_version_from_sources, library)

.get_package_version_from_sources <- function(pkg, library){
  suppressWarnings(pkg_version <- tryCatch(packageDescription(pkg, lib.loc = library)$Version, error = identity))
  if(inherits(pkg_version, "error") | is.null(pkg_version)){
    warning(paste("Could not retrieve version number from package", pkg, ". Setting to 0L!"))
    pkg_version <- "0.0"
  }
  pkg_version
}

get_package_revision_from_sources <- function(pkg, library = ".")
  sapply(pkg, .get_package_revision_from_sources, library)

.get_package_revision_from_sources <- function(pkg, library){
  suppressWarnings(pkg_revision <- tryCatch(packageDescription(pkg, lib.loc = library)$Revision, error = identity))
  if(inherits(pkg_revision, "error") | is.null(pkg_revision)){
    warning(paste("Could not retrieve revision number from package", pkg, ". Setting to 0L!"))
    pkg_revision <- 0L
  }
  pkg_revision <- as.integer(pkg_revision)
  if(is.na(pkg_revision))
    pkg_revision <- 0L
  pkg_revision
}
