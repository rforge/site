## Package Build Infrastructure for R-Forge
## Author: Stefan Theussl

## Constructor for R-Forge control files
rf_build_control <- function(path_to_pkg_src, path_to_pkg_log, path_to_pkg_root,
                              path_to_local_texmf, path_to_local_library,
                              path_to_local_pkg_libs, stoplist,
                              mail_domain_name_of_sender, mail_relay_server,
                              mail_programme = "mail",
                              path_to_check_dir = "",
                              cpu_time_limit = 600){
  structure(list(cpu_time_limit = cpu_time_limit,
                 mail_domain_name_of_sender = mail_domain_name_of_sender,
                 mail_programme = mail_programme,
                 mail_relay_server = mail_relay_server,
                 path_to_check_dir = path_to_check_dir,
                 path_to_local_library = path_to_local_library,
                 path_to_local_pkg_libs = path_to_local_pkg_libs,
                 path_to_local_texmf = path_to_local_texmf,
                 path_to_pkg_log  = path_to_pkg_log,
                 path_to_pkg_root = path_to_pkg_root,
                 path_to_pkg_src  = path_to_pkg_src,
                 stoplist = stoplist),
            class = "rf_build_control")
}

is.rf_build_control <- function(x)
  inherits(x, "rf_build_control")

rf_takeover_prepared_build <- function(stmp, build_root, type = "src"){
  stopifnot( type %in% c("src", "mac", "win") )
  ## take the oldest available for build
  if(type == "src"){
    btgz <- grep("^build_.*?.tar.gz$", dir(stmp), value = TRUE)
  } else {
    btgz <- grep("^SRC.build_.*?.tar.gz$", dir(stmp), value = TRUE)
  }
  ## if no build to take up -> exit
  if( any(is.na(btgz)) )
    return(NULL)
  if( !length(btgz) )
      return(NULL)

  check_tgz <- function(btgz){
    ## rename it to *.processing or create lock file .processing.<OStype>
    ptgz <- switch(type,
     "src" =  paste(btgz, "processing", sep = "."),
     "mac" =  paste(btgz, "processing", "MAC", sep = "."),
     "win" =  paste(btgz, "processing", "WIN", sep = ".") )
    ## if given submission is already being processed exit and return NULL
    if( file.exists(file.path(stmp, ptgz)) )
      return( NULL )
    ## submission already processed
    if( type == "mac" && file.exists(file.path(stmp, gsub("SRC.", "MAC.", btgz))) )
      return( NULL )
    if( type == "win" && file.exists(file.path(stmp, gsub("SRC.", "WIN.", btgz))) )
      return( NULL )
    ptgz
  }
  ptgz <- lapply( btgz, check_tgz)
  nn <- !unlist(lapply(ptgz, is.null))
  if (!any(nn))
    return(NULL)
  btgz <- btgz[nn][1]
  ptgz <- unlist(ptgz[nn])[1]

  if(type == "src"){
    file.rename( file.path(stmp, btgz), file.path(stmp, ptgz) )
  } else {
    file.create( file.path(stmp, ptgz) )
  }
  ## uncompress staging area
  TAR <- Sys.getenv("TAR")
  WINDOWS <- .Platform$OS.type == "windows"
  if (!nzchar(TAR)) {
    TAR <- if (WINDOWS)
      "tar --no-same-owner --force-local"
    else "internal"
  }
  if( type == "src" ){
    res <- utils::untar( file.path(stmp, ptgz), compressed = "gzip", tar = TAR, exdir = build_root)
  } else {
    res <- utils::untar( file.path(stmp, btgz), compressed = "gzip", tar = TAR, exdir = build_root)
  }
  if (res) {
    stop("unpackaging staging area failed.")
  }
  ## src_dir
  if( type == "src" ){
    src_dir <- gsub( ".tar.gz.processing", "", ptgz )
  } else {
    src_dir <- gsub( "SRC.", "", gsub(".tar.gz", "", btgz) )
  }
  src_dir
}

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
rf_build_packages <- function(pkg_status,
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
                              omega_hat_url  = "http://www.omegahat.net/R",
                              control        = list(),
                              Ncpus = 1L){

  if( !is.rf_build_control(control) )
    stop("No R-Forge build control object given")

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
  path_to_local_pkg_libs <- control$path_to_local_pkg_libs
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
  ## local pkg libraries for forced R-Forge depends
  if(!check_directory( path_to_local_pkg_libs, fix=TRUE))
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

    avail_src_pkgs <- utils::available.packages(URL_pkg_sources)

    pkgs <- remove_excluded_pkgs(rownames(avail_src_pkgs), donotcompile)
  } else {
    ## Packages exported from R-Forge's SVN repositories
    pkgs_all <- utils::available.packages(contriburl =
                                          sprintf("file://%s", path_to_pkg_src), filters = "duplicates")[, 1]
    ## Sort out packages that are on the exclude list
    pkgs <- remove_excluded_pkgs(pkgs_all, donotcompile)

    ## sources to be considered in dependency check
    URL_pkg_sources <- sprintf("file://%s", path_to_pkg_src)
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
                         path_to_local_library, path_to_local_pkg_libs, platform, Ncpus = Ncpus, rforge_url = rforge_url)

  ##############################################################################
  ## LAST PREPARATION STEPS BEFORE PACKAGE BUILDING
  ##############################################################################

  ## change to directory where the sources of R-Forge are in
  setwd(path_to_pkg_src)

  ## delete 00LOCK, sometimes this interrupted the build process ...
  check_local_library(path_to_local_library)
  ## where is our R binary?
  R <- file.path( R.home(), "bin", "R" )
  if( platform == "Windows" )
      R <- file.path(R.home(), "bin", "x64", "R")
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

    no_build_vignettes <- c(no_vignettes, no_install, fake_install)

    build_src <- function(pkg, architecture, avail_src_pkgs, no_build_vignettes, path_to_pkg_log, pkg_status, platform, R){
        ## Prolog
        pkg_buildlog <- rfTools:::get_buildlog(path_to_pkg_log, pkg, platform, architecture = "all")
        rfTools:::write_prolog(pkg, pkg_buildlog, pkg_status,
                    type = "build", what = "tarball", std.out = TRUE)

        build_args <- if( pkg %in% no_build_vignettes)
            "--no-build-vignettes"
        else
            ""

        ## timer start
        proc_start <- proc.time()

        ## build tarball from sources
        .build_tarball_from_sources_linux(pkg, R, pkg_buildlog, build_args)

        ## Epilog and timings
        timing <- c(proc.time() - proc_start)["elapsed"]

        write_epilog(pkg_buildlog, timing, std.out = TRUE)
        timing
    }
    if(Ncpus > 1L){
        timings <- parallel::mclapply(pkgs, FUN = build_src, architecture, avail_src_pkgs,  no_build_vignettes, path_to_pkg_log, pkg_status, platform, R, mc.cores = Ncpus)
        timings <- structure( unlist(timings), names = pkgs )
    } else {
        ## Initialize timings
        timings <- numeric(length(pkgs))
        names(timings) <- pkgs

        for(pkg in pkgs)
            timings[pkg] <- build_src(pkg, architecture, avail_src_pkgs, no_build_vignettes, path_to_pkg_log, pkg_status, platform, R)
    }

    ## Cleanup
    close_virtual_X11_fb(pid)

  }else{
      ## BINARIES ##########################################################

      ## Get additional build flags based on Uwe's lists
      ## merge-multiarch
      mma_list <- character()
      con <- tryCatch(url("http://developer.r-project.org/CRAN/QA/Uwe/make/config/MergeMultiarch", open = "r"), error = identity)
      if(! inherits(con, "error")){
          mma_list <- unique(c(mma_list, readLines(con)))
          close(con)
      }

      ## force-biarch
      fb_list <- character()
      con <- tryCatch(url("http://developer.r-project.org/CRAN/QA/Uwe/make/config/ForceBiarch", open = "r"), error = identity)
      if(! inherits(con, "error")){
          fb_list <- tryCatch( readLines(con), error = function(x) NA)
          close(con)
      }

      ## R-Forge (">=") dependencies installed to pkg_libs
      pkg_libs <- NULL
      if( file.exists(path_to_local_pkg_libs) )
          pkg_libs <- list.files(path_to_local_pkg_libs)


      ## WINDOWS BUILDS ##########################################################
      if(platform == "Windows"){

          ## Initialize timings
          timings <- numeric(length(pkgs))
          names(timings) <- pkgs

          for( pkg in pkgs ){
              ## Prolog
              pkg_buildlog <- get_buildlog(path_to_pkg_log, pkg, platform, architecture)
              write_prolog(pkg, pkg_buildlog, pkg_status,
                   type = "build", what = "binary", std.out = TRUE)

              ## BUILD
              ## timer start
              proc_start <- proc.time()

              ## get current package version (tarball, thus svn in pkg!)
              pkg_version_src   <- avail_src_pkgs[pkg, "Version"]

              mergemultiarch <- FALSE
              forcebiarch <- FALSE
              build_in_pkglib <- FALSE
              if(pkg %in% mma_list)
                  mergemultiarch <- TRUE
              if(pkg %in% fb_list)
                  forcebiarch <- TRUE
              if(pkg %in% pkg_libs)
                  build_in_pkglib <- TRUE
              ## now build the package from package tarball
              .build_binary_from_tarball_win(pkg,
                                     pkg_version_src,
                                     path_to_pkg_root,
                                     R,
                                     pkg_buildlog,
                                     pkg_libs = path_to_local_pkg_libs,
                                     mergemultiarch = mergemultiarch,
                                     forcebiarch = forcebiarch,
                                     build_in_pkglib = build_in_pkglib
                                     )

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

          ## BUILDING FROM PKG TARBALLS
          build_bin_mac <- function(pkg, architecture, avail_src_pkgs, path_to_pkg_log, path_to_pkg_root, pkg_status, platform, R, path_to_local_pkg_libs, pkg_libs){
              ## Prolog
              pkg_buildlog <- rfTools:::get_buildlog(path_to_pkg_log, pkg, platform, architecture)
              rfTools:::write_prolog(pkg, pkg_buildlog, pkg_status,
                    type = "build", what = "binary", std.out = TRUE)

              ## timer start
              proc_start <- proc.time()

              mergemultiarch <- FALSE
              forcebiarch <- FALSE
              build_in_pkglib <- FALSE
              if(pkg %in% mma_list)
                  mergemultiarch <- TRUE
              if(pkg %in% fb_list)
                  forcebiarch <- TRUE
              if(pkg %in% pkg_libs)
                  build_in_pkglib <- TRUE

              ## now build the package from package tarball
              rfTools:::.build_binary_from_tarball_mac( pkg,
                                                  avail_src_pkgs[pkg, "Version"],
                                                  path_to_pkg_root,
                                                  R,
                                                  pkg_buildlog,
                                                  pkg_libs = path_to_local_pkg_libs,
                                                  mergemultiarch = mergemultiarch,
                                                  forcebiarch = forcebiarch,
                                                  build_in_pkglib = build_in_pkglib)

              ## Epilog and timings
              timing <- c(proc.time() - proc_start)["elapsed"]
              write_epilog(pkg_buildlog, timing, std.out = TRUE)
              timing
          }

          if(Ncpus > 1L){
              timings <- parallel::mclapply(pkgs, FUN = build_bin_mac, architecture, avail_src_pkgs, path_to_pkg_log, path_to_pkg_root, pkg_status, platform, R, path_to_local_pkg_libs, pkg_libs, mc.cores = Ncpus)
              timings <- structure( unlist(timings), names = pkgs )
          } else {
              ## Initialize timings
              timings <- numeric(length(pkgs))
              names(timings) <- pkgs

              for(pkg in pkgs)
                  timings[pkg] <- build_bin_mac(pkg, architecture, avail_src_pkgs, path_to_pkg_log, path_to_pkg_root, pkg_status, platform, R, path_to_local_pkg_libs, pkg_libs)
          }

          ## Cleanup: close framebuffer
          close_virtual_X11_fb(pid)
      } else stop(paste("Strange platform: ", platform, "! I'm confused ...",
                    sep = ""))
  }

  ## FINAL STEPS
  writeLines("Send email to R-Forge maintainer and cleanup ...")
  ## provide built packages in corresponding contrib dir
  pkgs_provided <- provide_packages_in_contrib(path_to_pkg_src,
                                               path_to_contrib_dir, platform)
  ## send email to R-Forge maintainer which packages successfully were built
  ## FIXME: make a log file for later examination
  #notify_admins(pkgs_provided, donotcompile, email, platform, control,
  #              about = "build", timings = timings)
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
## input: package tarball (<package_name>_<version>.tar.gz)
## output: compressed package binary <package_name>_<version>.zip
## FIXME: currently sources and resulting tarball are in the current working dir
.build_binary_from_tarball_win <- function(pkg, pkg_version, path_to_pkg_tarballs, R, pkg_buildlog, pkg_libs,  mergemultiarch = FALSE, forcebiarch = FALSE, build_in_pkglib = FALSE){
    Rbuild <- paste(R, "CMD", "INSTALL --build")

    if( mergemultiarch )
        Rbuild <- paste(Rbuild, "--merge-multiarch")
    if( forcebiarch )
        Rbuild <- paste(Rbuild, "--force-biarch")
    if( build_in_pkglib )
        Rbuild <- paste(Rbuild, sprintf("--library=%s", file.path(pkg_libs, pkg)))

    shell(paste(Rbuild,
               file.path(path_to_pkg_tarballs, "src", "contrib",
                         paste(pkg, "_", pkg_version, ".tar.gz", sep = "")),
                 ">>", pkg_buildlog, "2>&1"), invisible = TRUE, shell = "cmd")

    invisible(paste(pkg, "_", pkg_version, ".zip", sep = ""))
}

## OS: Mac OS X
## input: package tarball (<package_name>_<version>.tar.gz)
## output: compressed package binary <package_name>_<version>.tgz
## FIXME: currently sources and resulting tarball are in the current working dir
.build_binary_from_tarball_mac <- function(pkg, pkg_version, path_to_pkg_tarballs, R, pkg_buildlog, pkg_libs, mergemultiarch = FALSE, forcebiarch = FALSE, build_in_pkglib = FALSE){
    Rbuild <- paste(R, "CMD", "INSTALL --build")

    if( mergemultiarch )
        Rbuild <- paste(Rbuild, "--merge-multiarch")
    if( forcebiarch )
        Rbuild <- paste(Rbuild, "--force-biarch")
    if( build_in_pkglib )
        Rbuild <- paste(Rbuild, sprintf("--library=%s", file.path(pkg_libs, pkg)))

    system(paste(Rbuild,
               file.path(path_to_pkg_tarballs, "src", "contrib",
                         paste(pkg, "_", pkg_version, ".tar.gz", sep = "")),
                 ">>", pkg_buildlog, "2>&1"), invisible = TRUE)

    ## FIXME: still needed?
    ## make temporary directory
    ##  tmpdir <- .make <- tmp <- directory()

    ## re-link dynlibs (see http://cran.r-project.org/bin/macosx/RMacOSX-FAQ.html#Building-universal-package)
    #minor_version <-  paste( R.Version()$maj, unlist(strsplit(R.Version()$min, "[.]"))[1], sep="." )
    ## gfortran
    #system( sprintf("for lib in `ls %s/%s/libs/*/*.so`; do install_name_tool -change /usr/local/lib/libgfortran.2.dylib /Library/Frameworks/R.framework/Versions/%s/Resources/lib/libgfortran.2.dylib $lib ; done", tmpdir, pkg, minor_version) )
    ## gcc
    #system( sprintf("for lib in `ls %s/%s/libs/*/*.so`; do install_name_tool -change /usr/local/lib/libgcc_s.1.dylib /Library/Frameworks/R.framework/Versions/%s/Resources/lib/libgcc_s.1.dylib $lib ; done", tmpdir, pkg, minor_version) )

    #pkg_binary <- .make_universal_mac_binary(pkg, pkg_version, pkg_buildlog, tmpdir)

    ## Cleanup
    #.cleanup_mac(tmpdir)
    ##
    #invisible(pkg_binary)
    invisible(paste(pkg, "_", pkg_version, ".tgz", sep = ""))
}

## update package repository
## OLD way:
## When using a binary distribution (Windows, Mac) we don't need to
## install every package from source, we keep a complete local CRAN-
## install updated
update_package_library <- function(pkgs, path_to_pkg_src, repository_url, lib, pkg_libs, platform, rforge_url = "http://R-Forge.R-project.org", ...){
  writeLines(sprintf("Updating package library %s ...", lib))
  if((platform == "Linux") | (platform == "MacOSX")){
    ## Start a virtual framebuffer X server and use this for DISPLAY so that
    ## we can run package tcltk and friends.
    pid <- start_virtual_X11_fb()
  }
  ## first update all installed packages if necessary
  ## debug  cat(sprintf("Arguments to update.packages(): lib = %s, repos = %s, ask = FALSE, checkBuilt = TRUE", lib, paste(repository_url, collapse = ", ")))
  update.packages(lib.loc = lib, repos = repository_url, ask = FALSE, checkBuilt = TRUE, ...)

  writeLines("Done.")
  writeLines("Resolve dependency structure ...")
  ## pkg list and dependency structure
  pkgs_dep <- resolve_dependency_structure(pkgs, repository_url, path_to_pkg_src)

  ## packages installed in library
  pkgs_installed <- installed.packages(lib.loc = lib)

  ## update packages installed from R-Forge and not available from the other standard repositories
  avail_repos <- available.packages(contriburl = contrib.url(repository_url))
  avail_rforge <- available.packages( contriburl = contrib.url(rforge_url) )
  if(length(avail_rforge)){
    rf_only <- rownames(pkgs_installed)[rownames(pkgs_installed) %in% avail_rforge[,1][!(avail_rforge[,1] %in% unique(avail_repos[,1]))]]
    if(length(rf_only))
      update.packages(lib.loc = lib, repos = rforge_url, ask = FALSE, checkBuilt = TRUE, oldPkgs=rf_only, ...)
  }

  ## Temporarily All packages are installed
  ## install those packages which are only available from R-Forge, the rest
  ## should be installed from CRAN or other repositories
  ## TODO: considering the install order
  pkgs_to_install <- setdiff(pkgs_dep[["ALL"]], rownames(pkgs_installed))
  pkgs_to_install <- pkgs_to_install[pkgs_to_install %in% unique(avail_repos[,1])]
  pkgs_to_install_rforge <- setdiff(pkgs_dep[["R_FORGE"]], unique(c(pkgs_to_install, rownames(pkgs_installed))))
  writeLines("Done.")

  if(length(pkgs_to_install)){
    writeLines("Install missing packages from third party repositories ...")
    if(platform == "Windows"){
        install.packages(pkgs = as.character(na.omit(pkgs_to_install)), lib = lib, contriburl = contrib.url(repository_url, type = "binary"), ...)
    } else {
        install.packages(pkgs = as.character(na.omit(pkgs_to_install)), lib = lib, contriburl = contrib.url(repository_url), ...)
    }
    writeLines("Done.")
  } 

  if(length(pkgs_to_install_rforge)){
    writeLines("Install missing packages from R-Forge ...")
    install.packages(pkgs_to_install_rforge, lib = lib, contriburl = contrib.url(rforge_url, type = "source"), type = "source", ...)
    writeLines("Done.")
  }

  ## If dependencies on pkgs hosted on CRAN *and* on R-Forge are to be
  ## resolved requiring a higher version number (flagged as >= in
  ## Depends, Imports, or Suggests in the DESCRIPTION file) than what
  ## is available on CRAN we create a new packages library including
  ## the corresponding package(s) from R-Forge.

  forced_deps <- .pkgs_forced_rforge_depends( available.packages(contriburl = path_to_pkg_src), avail_repos, avail_rforge )
  .make_pkg_libs( forced_deps, pkg_libs, path_to_pkg_src, repository_url, rforge_url, lib  )

  if((platform == "Linux") | (platform == "MacOSX")){
    ## Close the virtual framebuffer X server
    close_virtual_X11_fb(pid)
  }
  writeLines("Done.")
}

## this function resolves the dependency structure of source pkgs and returns a list
## containing ALL packages, packages hosted on other repositories and the install order
## of ALL packages.
## Additionally suggested packages are included, as they are probably needed when
## building package vignettes
resolve_dependency_structure <- function(pkgs, repository_url, path_to_pkg_src, rforge_url = "http://download.r-forge.r-project.org"){
  ## look out for available packages
  avail_repos <- available.packages(contriburl =
                                   contrib.url(repository_url))
  avail_rforge <- available.packages(contriburl = contrib.url(rforge_url))
  avail_batch <- available.packages(contriburl = path_to_pkg_src)
  avail <- rbind(avail_batch, avail_rforge, avail_repos)
  ## What packages do we need from external repository
  pkgs <- pkgs[pkgs %in% rownames(avail_batch)]
  pkgs_suggested <- resolve_suggests(pkgs, avail)
  pkgs_suggested <- pkgs_suggested[pkgs_suggested %in% rownames(avail)]
  pkgs_to_resolve_deps <- unique(c(pkgs, pkgs_suggested))
  pkgs_all <- resolve_dependencies(pkgs_to_resolve_deps, avail)
  pkgs_repos <- setdiff(pkgs_all, pkgs)
  pkgs_rforge <- setdiff(pkgs_all, rownames(avail_repos))
  ## FIXME: what to do with packages hosted on both R-Forge and other repos ...
  DL <- utils:::.make_dependency_list(pkgs_all, avail)
  pkgs_install_order <- utils:::.find_install_order(pkgs_all, DL)

  ## return a vector with packages and install order
  list(ALL = pkgs_all, REPOS = pkgs_repos, R_FORGE = pkgs_rforge, INSTALL_ORDER = pkgs_install_order)
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


## Function definition of version ordering functions
## given two version number, this function returns the order of them
version_order <- function(x){
  if(!is.character(x))
    stop("A version number has to be of type character!")
  if(! length(x) == 2)
    warning("More than 2 version numbers detected: The order is calculated only from the first 2 elements in the vector!")
  ## coerce to numeric version
  y <- numeric_version(x[1])
  z <- numeric_version(x[2])

  if(y <= z){
    return(c(1,2))
  } else
    return(c(2,1))
}

## resolve dependencies helper
resolve_dependencies <- function(pkgs, available){
  pkgs_all <- pkgs
  pkgs_old <- NULL
  while(!(length(pkgs_old)==length(pkgs_all))){
    pkgs_old <- pkgs_all
    DL <- unlist(utils:::.make_dependency_list(pkgs_all, available))
    #browser()
    pkgs_all <- unique(c(pkgs_all,DL))
    pkgs_all <- pkgs_all[pkgs_all %in% available[,1]]
  }
  pkgs_all
}

.make_suggests_list <- function(pkgs, available){
  if (!length(pkgs))
    return(NULL)
  if (is.null(available))
    stop(gettextf("'%s' must be supplied", available), domain = NA)
  info <- available[pkgs, "Suggests", drop = FALSE]
  x <- apply(info, 1, utils:::.clean_up_dependencies)
  if (length(pkgs) == 1) {
    x <- list(as.vector(x))
    names(x) <- pkgs
  }
  ## bundles are defunct in 2.11 ...
  bundles <- tryCatch(utils:::.find_bundles(available), error = identity)
  if(! inherits(bundles, "error") ){
    x <- lapply(x, function(x) if (length(x)) {
      for (bundle in names(bundles)) x[x %in% bundles[[bundle]]] <- bundle
      x <- x[!x %in% c("R", "NA")]
      unique(x)
    }
    else x)
  }
  x
}

resolve_suggests <- function(pkgs, available){
  pkgs_suggested <- unlist(.make_suggests_list(pkgs,available))
  pkgs_suggested
}

## remove certain pkgs from a pkg list
remove_excluded_pkgs <- function(pkgs, to_remove){
  excluded <- sapply(pkgs, "[", 1) %in% to_remove
  pkgs[!excluded]
}

## checks if directory exists---if not: creates it if  desired
check_directory <- function(dir, fix = FALSE, ...){
  out <- TRUE
  if(!file.exists(dir)){
    out <- FALSE
    if(fix){
      dir.create(dir, ...)
      if(file.exists(dir))
        return(TRUE)
    }
  }
  out
}

## check, if packages can be installed to local library
check_local_library <- function(lib){
  ## look, if library is locked
  lock <- file.path(lib, "00LOCK")
  if(file.exists(lock))
     system(paste("rm -rf", lock))
}

## check, if packages can be installed to local library
## TODO: rotate (gzip and copy to backup location) old logs
##       to keep track of history
check_log_directory <- function(dir, type = c("build", "check")){
  type <- match.arg(type)
  if(!check_directory(dir, fix=TRUE))
    stop(paste("There is no directory", dir, "!"))
  old_wd <- getwd()
  setwd(dir)
  if(type == "build"){
    suffix <- "buildlog.txt"
  }else {
    suffix <- "checklog.txt"
  }
  files <- list.files(dir, pattern = suffix)
  file.remove(files)
  setwd(old_wd)
}

## Start a virtual framebuffer X server and use this for DISPLAY so that
## we can run package tcltk and friends.  We use a random PID
## as the server number so that the checks for different flavors
## get different servers.
start_virtual_X11_fb <- function(){
  ## FIXME: if /usr/bin/X11 exists -> then setting path not necessary
  Sys.setenv(PATH=paste(Sys.getenv("PATH"), ":/usr/bin/X11", sep=""))
  xvfb_screen <- floor(runif(1,1000,9999))
  system(paste("Xvfb :", xvfb_screen, " -screen 0 1280x1024x24 &", sep=""))
  pid <- as.integer(system(paste("ps auxw | grep \"Xvfb :", xvfb_screen,
                      "\" | grep -v grep | awk '{ print $2 }'", sep=""), intern = TRUE))
  Sys.setenv(DISPLAY=paste(":", xvfb_screen, sep = ""))
  pid
}

close_virtual_X11_fb <- function(pid){
  system(paste("kill -9", pid))
  Sys.unsetenv("DISPLAY")
}

provide_packages_in_contrib <- function(build_dir, contrib_dir, platform){

  file_types <- c(Linux = ".tar.gz", MacOSX = ".tgz", Windows = ".zip")
  file_type <- file_types[platform]
  pkg_types <- c(Linux = "source", MacOSX = "mac.binary", Windows = "win.binary")
  pkg_type <- pkg_types[platform]
  ## Hard coding fields (we are indexing by number, so hard-coding important here!)
  ## R-Forge repository offers additional "Revision" field.
  fields <- c(tools:::.get_standard_repository_db_fields(), "Repository", "Repository/Project", "Repository/R-Forge/Revision", "Repository/R-Forge/DateTimeStamp")
  ## Remember old working directory
  old_dir <- file_path_as_absolute(getwd())
  setwd(build_dir)

  ## first delete old packages
  system(sprintf("rm -f %s", file.path(contrib_dir, sprintf("*%s", file_type))))
  files <- dir()
  files <- files[grep(sprintf("%s$", file_type), files)]
  for(i in files){
    ## copy package to contrib directory
    file.copy(i, contrib_dir, overwrite = TRUE)
    ## delete package from build directory
    system(paste("rm -f", i))
  }
  ## remove old PACKAGES file
  setwd(contrib_dir)
  system(paste("rm -f PACKAGES*"))
  ## now find out which packages have old versions DEPRECATED! -> to be removed
  tmp <- dir()
  splitted <- strsplit(tmp, "_")
  packages <- sapply(splitted, "[", 1)
  if(length(packages)){
    ind <- 1L:length(packages)
    versno <- unlist(strsplit(sapply(splitted, "[", 2), file_type))
    duplicated_pkgs <- duplicated(packages)
    if(any(duplicated_pkgs)){
      ## look for duplicated packages and remove older version
      for(i in packages[duplicated_pkgs]){
        ind_package_to_remove <- ind[packages==i][version_order(versno[packages==i])[1]]
        system(paste("rm -f", tmp[ind_package_to_remove]))
      }
    }
  }
  ## Write a new PACKAGES and PACKAGES.gz file
  write_PACKAGES(dir = contrib_dir, fields = fields, type = pkg_type)

  ## back to old directory
  setwd(old_dir)
  unique(packages)
}

## compute dependencies on packages double hosted on CRAN (primary
## repository for resolving deps) and R-Forge. Returns those packages
## in the batch of packages scheduled for building/checking which are
## forced to be installed from R-Forge. The list elements are the
## corresponding R-Forge dependencies.
.pkgs_forced_rforge_depends <- function( avail_batch, avail_repos, avail_rforge,
                                 fieldstocheck = c("Depends", "Imports", "Suggests") ){
  pkg_info <- lapply( rownames(avail_batch),
                      function(pkg) tools:::.split_description(na.omit(avail_batch[pkg,])) )
  dep_info <- lapply( pkg_info, function(info) unlist(lapply(fieldstocheck, function(field)
                                                             if(length(info[[field]]))
                                                             lapply(info[[field]], function(x)
                                                                    if(length(x$op))
                                                                    if(x$op %in% c(">=", ">")) x$version)), recursive = FALSE) )
  names(dep_info) <- rownames(avail_batch)
  true_deps <- lapply(dep_info, .check_if_rforge_version_needed, avail_repos = avail_repos, avail_rforge = avail_rforge )
  notnull <- unlist(lapply( true_deps, function(x) length(x) > 0 ))

  true_deps[notnull]
}

.check_if_rforge_version_needed <- function( x, avail_repos, avail_rforge ){
  out <- unlist(lapply(names(x), function(dep) {
    if(length(x[[dep]])){
      if(dep %in% rownames(avail_repos))
        return(tryCatch(x[[dep]] > avail_repos[dep, "Version"], error = function(x) TRUE) && tryCatch(x[[dep]] <= avail_rforge[dep, "Version"], error = function(x) FALSE))
      else
        FALSE
   } else
     FALSE }))
  if(length(out))
    return(names(x)[out])
  NULL
}


.make_pkg_libs <- function( x, pkg_libs, path_to_pkg_src, repository_url, rforge_url, lib, ...){
  lapply( names(x), function(pkg) {
    dir.create( file.path(pkg_libs, pkg), showWarnings = FALSE )
    pkgs_dep <- resolve_dependency_structure( pkg, repository_url, path_to_pkg_src )
    for(dep in pkgs_dep[["ALL"]])
      file.symlink( file.path(lib, dep), file.path(pkg_libs, pkg, dep) )
    for(fdep in x[[pkg]]){
      file.remove(file.path(pkg_libs, pkg, fdep))
      install.packages(pkgs = fdep, lib = file.path(pkg_libs, pkg), contriburl = contrib.url(rforge_url), ...)
    }
  } )
  invisible(TRUE)
}


