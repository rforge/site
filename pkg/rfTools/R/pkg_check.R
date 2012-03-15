rf_check_packages <- function( pkg_status,
                               platform         = c("Linux", "Windows", "MacOSX"),
                               architecture     = c("x86_32", "x86_64"),
                               rforge_url       = "http://R-Forge.R-project.org",
                               cran_url         = "http://CRAN.R-project.org",
                               bioc_url         = "http://bioconductor.org/packages/release/bioc",
                               bioc_data        = "http://bioconductor.org/packages/release/data/annotation",
                               bioc_experiment  =
                              "http://bioconductor.org/packages/release/data/experiment",
                               omega_hat_url    = "http://www.omegahat.org/R",
                               global_check_arg = NULL,
                               check_time_limit = 600,
                               control          = list()
                              ){
  if( !is.rf_build_control(control) )
    stop( "No R-Forge control object given" )

  ## INITIALIZATION

  ## match arguments
  platform <- match.arg( platform ) ## FIXME: automat. use info from .Platform?
  architecture <- match.arg( architecture )
  maj.version <- paste(R.Version()$maj, unlist(strsplit(R.Version()$min, "[.]"))[1], sep=".")
  ## x86_32 on x86_64 allowed but not the other way round
  if((architecture=="x86_64") && (.Machine$sizeof.pointer == 4))
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
                         c(cran_url, bioc_url, bioc_experiment, omega_hat_url),
                         path_to_local_library, platform)

  ## LAST PREPARATION BEFORE CHECKING - DIRECTORIES

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
  check_too_long <- NULL
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
    write_prolog(pkg, pkg_checklog, pkg_status, type = "check",
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
    toreplace <- ifelse( platform == "Windows", "file:///", "file://" )
    pkg_url <- file.path( gsub(toreplace, "", URL_pkg_sources),
                         sprintf("%s_%s.tar.gz", pkg,
                                 pkg_status$outdated[[pkg]]$description["Version"]) )
    ## NOTE: On Windows we should use shell() instead of system()
    ##       otherwise pipes and redirections fail (see also ?system)
    timings[pkg] <- ifelse( platform == "Windows",
      system.time(system2(R, args = paste("CMD check --as-cran", check_arg, pkg_url), stdout = pkg_checklog, stderr = pkg_checklog))["elapsed"],
                  system.time(system(paste(R, "CMD check --as-cran", check_arg, pkg_url,
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
  #notify_admins( pkgs_checked, donotcompile, email, platform, control,
  #               path_to_check_dir, timings = timings, about = "check" )
  ## go back to old working directory
  setwd(old_wd)
  TRUE
}

## TODO: time_i.out
finalize_check_results <- function(check_results_dir,
                                   path_to_pkg_src,
                                   check_args,
                                   timings,
                                   check_results_files = c("SUMMARY",
                                                           "check.csv",
                                                           "time_c.out")){
  ## search check directory for check results
  ## check_results_dir <- "/srv/rsync/R-Forge.check/R-devel"
  ## path_to_pkg_src <- "/srv/R/pkgs"
  PKGS <-  file.path(check_results_dir, "PKGS")
  Rcheck <- dir(PKGS)
  Rcheck <- Rcheck[grep(".Rcheck$", Rcheck)]
  ## successfully checked
  ##pkgs <- gsub(".Rcheck$", "", Rcheck)
  path_to_install_logs <- file.path(check_results_dir, "installout")
  if(!file.exists(path_to_install_logs))
    dir.create(path_to_install_logs)
  ## provide DESCRIPTION metadata in directories with check results
  ## FIXME: only works on Linux as path_to_pkg_src is only there available
  for(dir in Rcheck){
    pkg <- gsub(".Rcheck$", "", dir)
    file.copy(file.path(path_to_pkg_src, pkg, "DESCRIPTION"),
              file.path(PKGS, dir, "00package.dcf"))
    file.copy(file.path(PKGS, dir, "00install.out"),
              file.path(path_to_install_logs, sprintf("%s_install.out", pkg)), overwrite = TRUE)
  }

  ## save old check results
  suffix <- ".prev"
  check_results_files <- file.path(check_results_dir, check_results_files)
  for(file in check_results_files){
    previous_file <- paste(file, suffix, sep = "")
    if(file.exists(previous_file))
      file.remove(previous_file)
    if(file.exists(file))
      file.rename(file, previous_file)
  }

  if(!check_directory(file.path(check_results_dir, "Results"), fix=TRUE))
    stop(paste("There is no directory", file.path(check_results_dir, "Results"),
               "!"))

  today <- format(Sys.time(), "%Y-%m-%d")
  results_archive <- file.path(check_results_dir, "Results", today)

  if(!check_directory(results_archive, fix=TRUE))
    stop(paste("There is no directory", results_archive,"!"))

  ## not yet included
  #PKGS_prev <- paste(PKGS, suffix, sep = "")
  #if(file.exists(PKGS_prev))
  #  unlink(PKGS_prev, recursive = TRUE)
  #if(file.exists(PKGS))
  #  file.rename(PKGS, PKGS_prev)
  ##test -d PKGS      && mv PKGS PKGS.prev
  ##mv Work/PKGS PKGS
  ## Move timings up.
  ##mv Work/time_c.out time_c.out
  ##mv Work/time_i.out time_i.out

  write_check_timings(file = file.path(check_results_dir, "time_c.out"),
                      Rcheck, timings)

  ## Create check.csv.
  write_check_csv(file = file.path(check_results_dir, "check.csv"), Rcheck,
                  PKGS, path_to_pkg_src, check_args)

  ## Create SUMMARY.
  write_check_SUMMARY(file = file.path(check_results_dir, "SUMMARY"), Rcheck,
                      PKGS, path_to_pkg_src)

  for(file in check_results_files){
    file.copy(file, results_archive)
  }

  write_check_diffs(check_results_dir)
}


## Create check.csv.
## for testing purposes: check_args <- read.csv("/srv/R/lib/check_R_stoplist", stringsAsFactors = FALSE)
write_check_csv <- function(Rcheck, check_dir, path_to_pkg_src, check_args, file = "check.csv"){
  fields <- c("Package", "Version", "Priority", "Maintainer", "Status", "Comment")
  len <- length(Rcheck)
  if(len){
    csv <- matrix(rep(NA, len * length(fields)), nrow = len)
    colnames(csv) <- fields
    for(i in 1:len){
      dir <- Rcheck[i]
      pkg <- gsub(".Rcheck$", "", dir)
      ## FIXME: When reading DCF files, we take the first result. E.g., BIOMOD returns the actual value plus NA. An encoding issue?
      suppressWarnings(version <- tryCatch(read.dcf(file.path(path_to_pkg_src, pkg, "DESCRIPTION"), "Version")[1], error = identity))
      if(inherits(version, "error"))
        version <- NA
      suppressWarnings(priority <- tryCatch(read.dcf(file.path(path_to_pkg_src, pkg, "DESCRIPTION"), "Priority")[1], error = identity))
      if(inherits(priority, "error"))
        priority <- NA
      suppressWarnings(maintainer <- tryCatch(read.dcf(file.path(path_to_pkg_src,
                                                                 pkg, "DESCRIPTION"), "Maintainer")[1], error = identity))
      if(inherits(maintainer, "error"))
        maintainer <- NA
      suppressWarnings(checklog <- tryCatch(readLines(file.path(check_dir, dir,
                                                                "00check.log")), error = identity))
      if(inherits(checklog, "error"))
        checklog <- "ERROR: no check log found (rfTools)"
      warnings <- grep("WARNING$", checklog, fixed = TRUE, useBytes = TRUE)
      errors <- grep("ERROR", checklog, fixed = TRUE, useBytes = TRUE)
      if(length(errors))
        status <- "ERROR"
      else
        if(length(warnings))
          status <- "WARN"
        else
          status <- "OK"

      args <- get_check_args(pkg, check_args)
      if(length(args))
        args <- sprintf("[%s]", args)
      else
        args <- ""
      if(length(args) > 1)
        args <- args[1]
      csv[i, ] <- c(pkg, version, priority, maintainer, status, args)
    }
    write.csv(csv, file, row.names = FALSE)
  }
  invisible(TRUE)
}


write_check_SUMMARY <- function(Rcheck, check_dir, path_to_pkg_src, file = "SUMMARY"){
  len <- length(Rcheck)
  SUMMARY <- character()
  for(i in 1:len){
    dir <- Rcheck[i]
    pkg <- gsub(".Rcheck$", "", dir)
    suppressWarnings(checklog <- tryCatch(readLines(file.path(check_dir, dir,
      "00check.log")), error = identity))
    if(inherits(checklog, "error"))
      checklog <- "ERROR: no check log found (RForgeTools)"
    problems <- grep("(^\\*\tRd files|^\'*\tnon-standard|(WARNING|ERROR)$)", checklog)
    suppressWarnings(maintainer <- tryCatch(read.dcf(file.path(path_to_pkg_src, pkg, "DESCRIPTION"), "Maintainer")[1], error = identity))
    if(inherits(maintainer, "error"))
      maintainer <- NA
    if(length(problems)){
      SUMMARY <- c(SUMMARY, pkg, sprintf("Maintainer: %s", maintainer), checklog[problems])
    }
  }
  writeLines(SUMMARY, file)
  invisible(TRUE)
}

write_check_timings <- function(Rcheck, timings, file = "time_c.out"){
  len <- length(Rcheck)
  time_c <- character(len)
  for(i in 1:len){
    dir <- Rcheck[i]
    pkg <- gsub(".Rcheck$", "", dir)
    time_c[i] <- sprintf("%s: %s", pkg, timings[pkg])
  }
  writeLines(time_c, file)
}

write_check_diffs <- function(check_dir, files = "check.csv"){
  files <- file.path(check_dir, files)
  for(file in files){
    if(file.exists(sprintf("%s.prev", file))){
      ifelse( .Platform$OS.type == "windows",
              shell(sprintf("diff %s.prev %s > %s.diff", file, file, file)),
              system(sprintf("diff %s.prev %s > %s.diff", file, file, file)) )
      ifelse( .Platform$OS.type == "windows",
             shell(sprintf("test -s %s.diff || rm -f %s.diff", file, file)),
             system(sprintf("test -s %s.diff || rm -f %s.diff", file, file)) )
    }

    if(file.exists(sprintf("%s.diff", file))){
      db <- check_results_diffs(file.path(check_dir))
      sink(sprintf("%s.diff", file))
      writeLines(c("Changes in check status (S) and/or version (V)",
                   do.call(sprintf,
                           c(list("using R %s.%s (%s-%s-%s r%s):\n"),
                             R.version[c("major", "minor", "year",
                                         "month", "day", "svn rev")]))))
      print(db)
      sink()
    }
  }
  invisible(TRUE)
}

.get_check_timings_from_file <- function( file ){
  stopifnot( file.exists(file) )
  timings <- read.dcf( file )
  structure( as.numeric(timings), names = colnames(timings) )
}

get_packages_exceeding_check_time_limit <- function( file, limit = 600 ){
  timings <- .get_check_timings_from_file( file )
  names(timings)[timings > limit]
}

get_check_args <- function(pkg, check_args){
  check_arg <- character()
  if(!is.null(check_args)){
    check_arg <- check_args[which(check_args["Package"] == pkg), "check_args"]
  }
  check_arg
}
