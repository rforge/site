## Package Build Infrastructure for R-Forge
## Author: Stefan Theussl


## TODO LIST (there are more points in Uwe's functions):

## file URLs of local mirrors: contriburl = sprintf("file:///%s", dir)
## do not download from cran.r-project.org
## should we include a tmp directory
## if so a cleanup is necessary
## Clean /tmp dir
##  system("rm -rf c:\\tmp\\*")

## this is where the work is done
## takes a control object containing control parameters, the platform,
## architecture, ... as arguments
build_packages <- function(email,
                           platform       = c("Linux", "Windows", "MacOSX"),
                           architecture   = c("x86_32", "x86_64"),
                           rforge_url     = "http://R-Forge.R-project.org",
                           cran_url       = "http://CRAN.R-project.org",
                           bioc_url       =
                             "http://bioconductor.org/packages/release/bioc",
                           bioc_data      =
                             "http://bioconductor.org/packages/release/data/annotation",
                           bioc_experiment=
                             "http://bioconductor.org/packages/release/data/experiment",
                           omega_hat_url  = "http://www.omegahat.org/R",
                           control        = list(),
                           Ncpus = 1L){

  if(!inherits(control, "R-Forge_control"))
    stop("No R-Forge control object given")
  ## INITIALIZATION
  writeLines("Start build process ...")
  ## match arguments
  ## FIXME: automatically use info from .Platform?
  platform <- match.arg(platform)
  architecture <- match.arg(architecture)
  maj.version <- paste(R.Version()$maj,
                       unlist(strsplit(R.Version()$min, "[.]"))[1], sep=".")
  ## x86_32 on x86_64 allowed but not the other way round
  if((architecture=="x86_64") && (.Machine$sizeof.pointer == 4))
    stop("Building x86_64 binaries not possible on an x86_32 architecture")
  ## check for necessary directories---create them if possible
  path_to_pkg_src <- control$path_to_pkg_src
  path_to_pkg_log <- control$path_to_pkg_log
  path_to_pkg_root <- control$path_to_pkg_root
  path_to_local_library <- control$path_to_local_library
  stoplist <- control$stoplist
  ## local package library
  if(!check_directory(path_to_local_library, fix=TRUE))
    stop(paste("There is no directory", path_to_local_library,"!"))
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
  ## BLACK_LIST: packages which cause severe problems (removed from R CMD build)
  ## TODO: blacklist hardcoded at the moment
  blacklist <- c("RepitoolsExamples")

  ## FIXME: probably too strict at the moment. E.g., RWinEdt does not get build
  ## when checking packages the stoplist includes additional arguments to check
  ## process
  if(file.exists(stoplist)){
    check_args <- read.csv(stoplist, stringsAsFactors = FALSE)
  }else check_args <- NULL
  ##if(length(no_install))
  ##  no_install <- check_args[no_install, 1]

  ## for Linux builds we don't want to build vignettes when checkargs have:
  no_install   <- check_args[ grep("--install=no",
                                   check_args[["check_args"]]), "Package" ]
  fake_install <- check_args[ grep("--install=fake",
                                   check_args[["check_args"]]), "Package" ]
  no_vignettes <- check_args[ grep("--no-vignettes",
                                   check_args[["check_args"]]), "Package" ]

  ## donotcompile <- no_install
  donotcompile <- blacklist
  if(platform %in% c("Windows", "MacOSX")){
    donotcompile <- unique(c(donotcompile, no_install))
  }

  if( length(donotcompile) ){
    for(pkg in donotcompile){
      arch <- "all"
      if(platform %in% c("Windows", "MacOSX")){
        arch <- architecture
      }
      pkg_buildlog <- get_buildlog(path_to_pkg_log, pkg,
                                   platform, architecture = arch)
      write_stoplist_notification(pkg, pkg_buildlog, "build", std.out = TRUE)
    }
  }

  ## set up CONTRIB directory
  ## If we use a BINARY distribution like WINDOWS or MAC we have to append
  ## the major version of R to the contrib directory otherwise use /src/contrib
  if(platform == "Windows"){
    path_to_contrib_dir <- file.path(path_to_pkg_root, "bin", .Platform$OS.type,
                                 "contrib", maj.version)

  }else if(platform == "MacOSX"){
    macosx_branch <- c(x86_64="leopard", x86_32="universal")
    path_to_contrib_dir <- file.path(path_to_pkg_root, "bin", "macosx",
                                     macosx_branch[architecture],
                                     "contrib", maj.version)
  }else {
    ## UNIX SOURCE directory (./src/contrib)
    path_to_contrib_dir <- contrib.url(path_to_pkg_root, type = "source")
  }
  if(!check_directory(path_to_contrib_dir, fix=TRUE, recursive=TRUE))
      stop(paste("There is no directory", path_to_contrib_dir,"!"))

  if(platform != "Linux"){
    ## sources to be considered in dependency check
    URL_pkg_sources <- contrib.url(sprintf("file:%s", path_to_pkg_root),
                                   type = "source")

    ## where are the source tarballs already available from R-Forge?
    path_to_pkg_tarballs <- control$path_to_pkg_tarballs
    if( !check_directory(path_to_pkg_tarballs) )
      stop( "Directory", path_to_pkg_tarballs, "missing!" )

    ## create package data base holding information about available repositories
    pkg_db_src <- create_package_db_src(svn = sprintf("file:%s",
                                          file.path(path_to_pkg_tarballs,
                                                    "src", "contrib")),
                                        src = sprintf("file:%s",
                                          path_to_contrib_dir))
    avail_src_pkgs <- pkg_db_src$svn[, 1]

    pkgs <- remove_excluded_pkgs(avail_src_pkgs, donotcompile)
    ## we take only tarballs into account which are hosted in R-Forge SVN reps
    ##avail_src_pkgs <- avail_src_pkgs[avail_src_pkgs %in% pkgs]
  } else {
    ## Packages exported from R-Forge's SVN repositories
    pkgs_all <- available.packages2(contriburl =
                                    sprintf("file://%s", path_to_pkg_src))[, 1]
    ## Sort out packages that are on the exclude list
    pkgs <- remove_excluded_pkgs(pkgs_all, donotcompile)

    ## sources to be considered in dependency check
    URL_pkg_sources <- sprintf("file://%s", path_to_pkg_src)

    ## create package data base holding information about available repositories
    pkg_db_src <- create_package_db_src(svn = sprintf("file://%s",
                                          path_to_pkg_src),
                                        src = contrib.url(sprintf("file://%s", path_to_pkg_root),
                                          type = "source"))
  }

  ## PACKAGE DB UPDATE

  ## FIXME: is it sufficient what we are doing here?
  other_repositories <- NULL

  if(platform == "Windows"){
    ## include Brian Ripley's Windows Repository
    other_repositories <- "http://www.stats.ox.ac.uk/pub/RWin"
  }

  update_package_library(c(pkgs), URL_pkg_sources, c(cran_url,
                                                     c(bioc_url, bioc_data, bioc_experiment),
                                                     omega_hat_url,
                                                     other_repositories),
                         path_to_local_library, platform, Ncpus = Ncpus)

  ##############################################################################
  ## LAST PREPARATION STEPS BEFORE PACKAGE BUILDING
  ##############################################################################

  ## change to directory where the sources of R-Forge are in
  setwd(path_to_pkg_src)

  ## delete 00LOCK, sometimes this interrupted the build process ...
  check_local_library(path_to_local_library)
  ## where is our R binary?
  #R <- ifelse( platform == "Windows",
  #             file.path(R.home(), "bin", "i386", "R"),
  #             file.path(R.home(), "bin", "R") )
  R <- file.path( R.home(), "bin", "R" )
  ## Set environment variables which are necessary for building
  ## (or creating vignettes)
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
      pkg_buildlog <- get_buildlog(path_to_pkg_log, pkg, platform,
                                   architecture = "all")
      write_prolog(pkg, pkg_buildlog, pkg_db_src, type = "build",
                   what = "tarball", std.out = TRUE)

      ## timer start
      proc_start <- proc.time()

      ## build only a new tarball if there is a new revision in the SVN
      ## FIXME: we probably need to check this at SVN export
      tarball_revision <-
        tryCatch(as.integer(pkg_db_src$src[pkg, "Repository/R-Forge/Revision"]),
                 error = identity)
      build <- TRUE
      if(!inherits(tarball_revision, "error")){
        source_revision <-
          as.integer(pkg_db_src$svn[pkg, "Repository/R-Forge/Revision"])
        if(!any(is.na(c(source_revision, tarball_revision))))
          if(source_revision <= tarball_revision){
            status <-
              .copy_tarball_from_repository(pkg,
                                            contrib.url(path_to_pkg_root),
                                            path_to_pkg_src,
                                            pkg_db_src$src[pkg, "Version"],
                                            pkg_buildlog)
            build <- !status
          }
      }

      if(build){
        build_args <- if( pkg %in% c(no_vignettes, no_install, fake_install) )
          "--no-vignettes"
        else
          ""
        ## build tarball from sources
        .build_tarball_from_sources_linux(pkg, R, pkg_buildlog, build_args)
      }

      ## save timing
      timings[pkg] <- c(proc.time() - proc_start)["elapsed"]

      ## Epilog
      write_epilog(pkg_buildlog, timings[pkg], std.out = TRUE)
    }

    ## Cleanup
    close_virtual_X11_fb(pid)

  ## WINDOWS BUILDS ##########################################################
  }else if(platform == "Windows"){

    ## Initialize timings
    timings <- numeric(length(avail_src_pkgs))
    names(timings) <- avail_src_pkgs

    for( pkg in pkgs ){
      ## Prolog
      pkg_buildlog <- get_buildlog(path_to_pkg_log, pkg, platform, architecture)
      write_prolog(pkg, pkg_buildlog, pkg_db_src,
                   type = "build", what = "binary", std.out = TRUE)

      ## BUILD
      ## timer start
      proc_start <- proc.time()

      ## get current package version (tarball, thus svn in pkg!)
      pkg_version_src   <- pkg_db_src$svn[pkg, "Version"]

      ## FIXME: some packages do not get a Revision flag. Why?
      binary_revision <-
        tryCatch(as.integer(pkg_db_src$src[pkg, "Repository/R-Forge/Revision"]),
                 error = identity)
      build <- TRUE
      ## for devel we build all packages
      if( !(R.version$status == "Under development (unstable)") ){
        ## otherwise we build only new revisions
        ## FIXME: reverse depend tests et al have to be considered
        if( !inherits(binary_revision, "error") ){
          tarball_revision <-
            as.integer( pkg_db_src$svn[pkg, "Repository/R-Forge/Revision"] )
          if( !any(is.na(c(tarball_revision, binary_revision))) )
            if( tarball_revision <= binary_revision ){
              status <-
                .copy_binary_from_repository( pkg,
                                             path_to_contrib_dir,
                                             path_to_pkg_src,
                                             pkg_db_src$src[pkg, "Version"],
                                             pkg_buildlog)
              build <- !status
            }
        }
      }

      if(build){
        ## now build the package from package tarball
        .build_binary_from_tarball_win(pkg,
                                       pkg_version_src,
                                       path_to_pkg_tarballs,
                                       R,
                                       pkg_buildlog)
      }

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
    for(pkg in pkgs){
      ## Prolog
      pkg_buildlog <- get_buildlog(path_to_pkg_log, pkg, platform, architecture)
      write_prolog(pkg, pkg_buildlog, pkg_db_src,
                   type = "build", what = "binary", std.out = TRUE)

      ## timer start
      proc_start <- proc.time()
      ## path to pkg buildlog

       ## get current package version (tarball, thus svn in pkg!)
      pkg_version_src   <- pkg_db_src$svn[pkg, "Version"]

      ## FIXME: some packages do not get a Revision flag. Why?
      binary_revision <-
        tryCatch(as.integer(pkg_db_src$src[pkg, "Repository/R-Forge/Revision"]),
                 error = identity)
      build <- TRUE
      ## for devel we build all packages
      if( !(R.version$status == "Under development (unstable)") ){
        ## otherwise we build only new revisions
        ## FIXME: reverse depend tests et al have to be considered
        if( !inherits(binary_revision, "error") ){
          tarball_revision <-
            as.integer( pkg_db_src$svn[pkg, "Repository/R-Forge/Revision"] )
          if( !any(is.na(c(tarball_revision, binary_revision))) )
            if( tarball_revision <= binary_revision ){
              status <-
                .copy_binary_from_repository( pkg,
                                              path_to_contrib_dir,
                                              path_to_pkg_src,
                                              pkg_db_src$src[pkg, "Version"],
                                              pkg_buildlog)
              build <- !status
            }
        }
      }

      if(build){
        ## now build the package from package tarball
        .build_binary_from_tarball_mac(pkg,
                                       pkg_version_src,
                                       path_to_pkg_tarballs,
                                       R,
                                       pkg_buildlog)
      }

      ## save timing
      timings[pkg] <- c(proc.time() - proc_start)["elapsed"]

      ## Epilog
      write_epilog(pkg_buildlog, timings[pkg], std.out = TRUE)
    } #</FOR>

    ## Cleanup: close framebuffer
    close_virtual_X11_fb(pid)
  } else stop(paste("Strange platform: ", platform, "! I'm confused ...",
                    sep = ""))

  ## FINAL STEPS
  writeLines("Send email to R-Forge maintainer and cleanup ...")
  ## provide built packages in corresponding contrib dir
  pkgs_provided <- provide_packages_in_contrib(path_to_pkg_src,
                                               path_to_contrib_dir, platform)
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
## Changelog 2011-04-28: --compact-vignettes --resave-data=best added to save disk space
.build_tarball_from_sources_linux <- function(pkg, R, pkg_buildlog, build_args = ""){
  files <- Sys.glob(c(file.path(pkg, "data", "*.rda"), file.path(pkg, "data", "*.RData"), file.path(pkg, "R", "sysdata.rda")))
  rdas <- tools::checkRdaFiles(files)
  if(!all(rdas$compress %in% c("bzip2", "xz")))
     build_args <- sprintf("--resave-data=best %s", build_args)
  system(paste(R, "CMD build --compact-vignettes", build_args, pkg,
               ">>", pkg_buildlog, "2>&1"))
  pkg_version <- get_package_version_from_sources(pkg)
  invisible(paste(pkg, "_", pkg_version, ".tar.gz", sep = ""))
}

.copy_tarball_from_repository <- function(pkg, pkg_root, pkg_src,
                                          pkg_version, pkg_buildlog){
  cat("Package up to date. Not building ...\n", file = pkg_buildlog,
      append = TRUE)
  file.copy(file.path(pkg_root, paste(pkg, "_", pkg_version, ".tar.gz",
                                      sep = "")), pkg_src, overwrite = TRUE)
}

.copy_binary_from_repository <- function(pkg, pkg_root, pkg_src,
                                          pkg_version, pkg_buildlog){
  cat("Package up to date. Not building ...\n", file = pkg_buildlog,
      append = TRUE)
  ## FIXME: is there an easier way to retrieve the file suffix?
  filesuffix <- ifelse( .Platform$OS.type == "unix", ".tgz", ".zip")
  file.copy(file.path(pkg_root, paste(pkg, "_", pkg_version, filesuffix,
                                      sep = "")), pkg_src, overwrite = TRUE)
}

## OS: Windows
## input: uncompressed package sources (the exported pkg directories)
## output: compressed package binary <package_name>_<version>.zip
## FIXME: currently sources and resulting tarball are in the current working dir
.build_binary_from_sources_win <- function(pkg, pkg_version, R, pkg_buildlog, build_args = ""){
  ## first we have to build the tarball (important for vignettes)
  system(paste(R, "CMD", "build", build_args, pkg,
                   ">>", pkg_buildlog, "2>&1"), invisible = TRUE)
  ## then build the binary
  system(paste(R, "CMD", "INSTALL --build --pkglock",
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
  shell(paste(R, "CMD", "INSTALL --build",
               file.path(path_to_pkg_tarballs, "src", "contrib",
                         paste(pkg, "_", pkg_version, ".tar.gz", sep = "")),
                 ">>", pkg_buildlog, "2>&1"), invisible = TRUE, shell = "cmd")
  ##               ">>", pkg_buildlog))#, invisible = TRUE)
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

 dirname <- paste(sample(c(letters, 0:9), 10, replace = TRUE), collapse = "")  ## first look if there is a src directory because then we know that we have
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

  ## re-link dynlibs (see http://cran.r-project.org/bin/macosx/RMacOSX-FAQ.html#Building-universal-package)
  minor_version <-  paste( R.Version()$maj, unlist(strsplit(R.Version()$min, "[.]"))[1], sep="." )
  ## gfortran
  system( sprintf("for lib in `ls %s/%s/libs/*/*.so`; do install_name_tool -change /usr/local/lib/libgfortran.2.dylib /Library/Frameworks/R.framework/Versions/%s/Resources/lib/libgfortran.2.dylib $lib ; done", tmpdir, pkg, minor_version) )
  ## gcc
  system( sprintf("for lib in `ls %s/%s/libs/*/*.so`; do install_name_tool -change /usr/local/lib/libgcc_s.1.dylib /Library/Frameworks/R.framework/Versions/%s/Resources/lib/libgcc_s.1.dylib $lib ; done", tmpdir, pkg, minor_version) )

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
  suppressWarnings(pkg_revision <- tryCatch(packageDescription(pkg, lib.loc = library)$"Repository/R-Forge/Revision", error = identity))
  if(inherits(pkg_revision, "error") | is.null(pkg_revision)){
    warning(paste("Could not retrieve revision number from package", pkg, ". Setting to 0L!"))
    pkg_revision <- 0L
  }
  pkg_revision <- as.integer(pkg_revision)
  if(is.na(pkg_revision))
    pkg_revision <- 0L
  pkg_revision
}
