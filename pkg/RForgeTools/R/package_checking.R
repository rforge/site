## code for checking R packages 
## originally based on kurt hornik's check-R
## completely rewritten in R by theussl 2008-03

check_packages <- function( email,
                            platform         = c("Linux", "Windows", "MacOSX"),
                            architecture     = c("x86_32", "x86_64"),
                            rforge_url       = "http://R-Forge.R-project.org",
                            cran_url         = "http://CRAN.R-project.org",
                            bioc_url         = "http://bioconductor.org/packages/release/bioc",
                            bioc_data        = "http://bioconductor.org/packages/release/data/annotation",
                            omega_hat_url    = "http://www.omegahat.org/R",
                            global_check_arg = NULL,
                            check_time_limit = 600,
                            control          = list()
                           ){
  if( !inherits(control, "R-Forge_control") )
    stop( "No R-Forge control object given" )

  ## INITIALIZATION

  ## match arguments
  platform <- match.arg( platform ) ## FIXME: automat. use info from .Platform?
  architecture <- match.arg( architecture )
  maj.version <- paste(R.Version()$maj, unlist(strsplit(R.Version()$min, "[.]"))[1], sep=".")
  ## x86_32 on x86_64 allowed but not the other way round
  if((architecture=="x86_64") && (.Machine$sizeof.long == 4))
    stop("Building x86_64 binaries not possible on an x86_32 architecture") 
  ## check for necessary directories---create them if possible
  path_to_pkg_src <- control$path_to_pkg_src
  print(path_to_pkg_src)
  path_to_pkg_log <- control$path_to_pkg_log
  print(path_to_pkg_log)
  path_to_pkg_root <- control$path_to_pkg_root
  print(path_to_pkg_root)
  path_to_check_dir <- control$path_to_check_dir
  print(path_to_check_dir)
  path_to_local_library <- control$path_to_local_library
  print(path_to_local_library)

  stoplist <- control$stoplist
  ## local package library
  if(!check_directory(path_to_local_library, fix=TRUE))
    stop(paste("There is no directory", path_to_local_library,"!"))
  ## check directory, this is where the work is done
  if(!check_directory(path_to_check_dir, fix=TRUE))
    stop(paste("There is no directory", path_to_check_dir,"!"))
  ## R-Forge package sources
  if(!check_directory(path_to_pkg_src))
    stop("Directory", path_to_pkg_src, "missing!")
  ## test for check log dir and clean it
  check_log_directory(path_to_pkg_log, type = "check")
  ## check if package root directory (the directory containing
  ## the src/contrib or bin/windows/contrib) exists.
  if(!check_directory(path_to_pkg_root, fix=TRUE))
    stop(paste("There is no directory", path_to_pkg_root,"!"))
  ## source tarballs
  URL_pkg_sources <- contrib.url(sprintf("file:///%s", path_to_pkg_root), type = "source")
  ## get current working directory -> set back at FINALIZATION step
  old_wd <- getwd()

  ## PACKAGE SIGHTING
  
  ## STOP LIST: packages which should not be compiled
  ## when checking packages the stoplist includes additional arguments to check process
  if(file.exists(stoplist)){
    check_args <- read.csv(stoplist, stringsAsFactors = FALSE)
  }else check_args <- NULL
  ## Source packages from R-Forge to be checked: successfully build tarballs
  pkgs <- available.packages(contriburl = URL_pkg_sources)[, 1]
  
  ## PACKAGE DB UPDATE ##

  ## FIXME: is it sufficient what we are doing here?
  update_package_library(pkgs, URL_pkg_sources,
                         c(cran_url, bioc_url, omega_hat_url),
                         path_to_local_library, platform)
  
  ## LAST PREPARATION BEFORE CHECKING - DIRECTORIES

  ## create package data base holding information about available repositories
  pkg_db_src <- create_package_db_src(svn = sprintf("file:///%s",
                                                    path_to_pkg_src),
                                      src = URL_pkg_sources)

  ## prepare check results directories PKGS, save old check results in PKGS_pre
  if( file.exists( file.path(path_to_check_dir, "PKGS_pre") ) )
    unlink( file.path(path_to_check_dir, "PKGS_pre"), recursive = TRUE )
  if( file.exists( file.path(path_to_check_dir, "PKGS") ) )
    system( sprintf("mv %s %s", file.path(path_to_check_dir, "PKGS"),
                                file.path(path_to_check_dir, "PKGS_pre")) )
  ## check/create directory, this is where the work is done
  if( !check_directory(file.path(path_to_check_dir, "PKGS"), fix=TRUE) )
    stop( sprintf("There is no directory '%s'!",
                  file.path(path_to_check_dir, "PKGS")) )
  ## change to directory where the check output should go
  setwd(file.path(path_to_check_dir, "PKGS"))

  ## LAST PREPARATION BEFORE CHECKING - MISCELLANEOUS
  
  ## delete 00LOCK, sometimes this interrupted the build process ...
  check_local_library(path_to_local_library)
  ## where is our R binary?
  R <- file.path(R.home(), "bin", "R")
  ## Set environment variables which are necessary for checking
  Sys.setenv(R_LIBS = path_to_local_library)
  ## Set TEXMFLOCAL environment variables in case we have
  ## personalized style files (building vignettes)
  path_to_local_texmf <- control$path_to_local_texmf
  if(file.exists(path_to_local_texmf))
    Sys.setenv(TEXMFLOCAL=path_to_local_texmf)
  if( platform %in% c("Linux", "MacOSX") ){
    ## Start a virtual framebuffer X server and use this for DISPLAY so that
    ## we can run package tcltk and friends.  
    pid <- start_virtual_X11_fb()
  }
  
  ## PACKAGE CHECKING
  
  ## note that we don't check packages fully again if they take too long
  old_timings_file <- file.path(path_to_check_dir, "time_c.out")
  if( file.exists(old_timings_file) ){
    check_too_long <- get_packages_exceeding_check_time_limit( old_timings_file,
                                                               check_time_limit)
  }
  ## obviously we want to collect new timings for each pkg checked
  timings <- numeric( length(pkgs) )
  names( timings ) <- pkgs
  
  ## And now the testing ... (only R-Forge pkg tarballs!)
  for(pkg in pkgs){
    ## Prolog
    pkg_checklog <- paste(file.path(path_to_pkg_log, pkg), "-", platform, "-",
                          architecture, "-checklog.txt", sep="")
    write_prolog(pkg, pkg_checklog, pkg_db_src, type = "check",
                 what = "tarball", std.out = TRUE)
    
    ## additional arguments to R CMD check (--no-vignettes, --no-tests, etc.)
    check_arg <- get_check_args(pkg, check_args)
    ## FIXME: global check args
    ##        there should be a default check args for new packages
    ##        should be checked only if the admins allow it
    if( (!is.null(global_check_arg)) && (length(check_arg) == 0) )
      check_arg <- global_check_arg
    if( length(check_arg) )
      cat( sprintf("Additional arguments to R CMD check: %s\n", check_arg),
           file = pkg_checklog, append = TRUE )
    else if( pkg %in% check_too_long ){
      ## no run time checks if package check takes too long
      check_arg <- "--no-examples --no-tests --no-vignettes"
      cat( sprintf("Additional arguments to R CMD check: %s (reason: run time too long)\n", check_arg), file = pkg_checklog, append = TRUE )
    }
    
    pkg_url <- file.path( gsub("file:///", "",
                               pkg_db_src$src[pkg, "Repository"]),
                         sprintf("%s_%s.tar.gz", pkg,
                                 pkg_db_src$src[pkg, "Version"]) )
    ## NOTE: On Windows we should use shell() instead of system()
    ##       otherwise pipes and redirections fail (see also ?system)
    timings[pkg] <- ifelse( platform == "Windows",    
      system.time(shell(paste(R, "CMD check", check_arg, pkg_url, ">>",
                              pkg_checklog, "2>&1")))["elapsed"],
                  system.time(system(paste(R, "CMD check", check_arg, pkg_url,
                                           ">>", pkg_checklog, "2>&1"))
                              )["elapsed"] )
    ## Epilog
    write_epilog(pkg_checklog, timings[pkg], std.out = TRUE)
  }
  ## better implementation necessary:
  pkgs_checked <- " "
  
  ## FINALIZATION

  if( platform %in% c("Linux", "MacOSX") ){
    ## Close the virtual framebuffer X server 
    close_virtual_X11_fb( pid )
  }

  ## provide check.csv et al.
  finalize_check_results( path_to_check_dir, path_to_pkg_src, check_args,
                         timings )
  
  ## send email to R-Forge maintainer which packages successfully were built
  notify_admins( pkgs_checked, donotcompile, email, platform, control,
                 path_to_check_dir, timings = timings, about = "check" )
  ## go back to old working directory
  setwd(old_wd)
  TRUE
}
