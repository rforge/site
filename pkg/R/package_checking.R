## code for checking R packages 
## originally based on kurt hornik's check-R
## completely rewritten in R by theussl 2008-03

check_packages <- function(email,
                           platform           = c("Linux", "Windows", "MacOSX"),
                           architecture       = c("x86_32", "x86_64"),
                           rforge_url = "http://R-Forge.R-project.org",
                           cran_url           = "http://CRAN.R-project.org",
                           control=list()
                           ){
  ## INITIALIZATION
  
  ## match arguments
  platform <- match.arg(platform) ## FIXME: automat. use info from .Platform?
  architecture <- match.arg(architecture)
  maj.version <- paste(R.Version()$maj,unlist(strsplit(R.Version()$min,"[.]"))[1],sep=".")
  flavor <- R.Version()$status
  ## x86_32 on x86_64 allowed but not the other way round
  if((architecture=="x86_64") && (.Machine$sizeof.long == 4))
    stop("Building x86_64 binaries not possible on an x86_32 architecture") 
  ## handle different file separators
  file_separator <- get_file_separator()
  ## check for necessary directories---create them if possible
  path_to_pkg_src <- control$path_to_pkg_src
  path_to_pkg_log <- control$path_to_pkg_log
  path_to_pkg_root <- control$path_to_pkg_root
  path_to_check_dir <- control$path_to_check_dir
  path_to_local_library <- control$path_to_local_library
  stoplist <- control$stoplist
  ## local package library
  if(!check_directory(path_to_local_library, fix=TRUE))
    stop(paste("There is no directory", dir,"!"))
  ## check directory, this is where the work is done
  if(!check_directory(path_to_check_dir, fix=TRUE))
    stop(paste("There is no directory", dir,"!"))
  ## R-Forge package sources
  if(!check_directory(path_to_pkg_src))
    stop("Directory", path_to_pkg_src, "missing!")
  ## test for check log dir and clean it
  check_log_directory(path_to_pkg_log, type = "check")
  ## check if package root directory (the directory containing
  ## the src/contrib or bin/windows/contrib) exists.
  if(!check_directory(path_to_pkg_root, fix=TRUE))
    stop(paste("There is no directory", dir,"!"))
  ## get current working directory -> set back at FINALIZATION step
  old_wd <- getwd()

  ## PACKAGE SIGHTING
  
  ## STOP LIST: packages which should not be compiled
  ## when checking packages the stoplist includes additional arguments to check process
  check_args <- if(file.exists(stoplist)){
    check_args <- read.csv(stoplist, stringsAsFactors = FALSE)
  }else check_args <- NULL
  ## sourcepackages available from R-Forge---exported svn reps
  avail_src <- dir(path_to_pkg_src)
  pkgs <- avail_src
  ## Sort out packages that are on the exclude list (TODO: not hardcoding in function!)
  donotcompile <- c("seriation")
  pkgs <- remove_excluded_pkgs(pkgs, donotcompile)
  
  ## PACKAGE DB UPDATE

  ## FIXME: is it sufficient what we are doing here?
  writeLines("Updating package library ...")
  update_package_library(pkgs, path_to_pkg_src, cran_url, path_to_local_library)
  writeLines("Done.")
  
  ## LAST PREPARATION BEFORE CHECKING

  ## change to directory where the check output should go
  setwd(path_to_check_dir)
  ## delete 00LOCK, sometimes this interrupted the build process ...
  check_local_library(path_to_local_library)
  ## where is our R binary?
  R <- paste(R.home(), "bin", "R", sep = file_separator)
  ## Set environment variables which are necessary for checking
  Sys.setenv(R_LIBS = path_to_local_library)
  ## Calculate dependency structure - Do we need package installation as we already have
  ## done an update of package db?
  ##dep_struct <- resolve_dependency_structure(pkgs, cran_url, path_to_pkg_src)
  ##pkgs_cran <- dep_struct["CRAN"]
  ##pkgs_to_install <- dep_struct["INSTALL_ORDER"]
  ## Set TEXMFLOCAL environment variables in case we have
  ## personalized style files (building vignettes)
  path_to_local_texmf <- control$path_to_local_texmf
  if(file.exists(path_to_local_texmf))
    Sys.setenv(TEXMFLOCAL=path_to_local_texmf)
  if(platform == "Linux"){
    ## Start a virtual framebuffer X server and use this for DISPLAY so that
    ## we can run package tcltk and friends.  
    pid <- start_virtual_X11_fb()
  }
  
  ## PACKAGE CHECKING
   
  ## Installation first ...
  ## Note that installing to the default library tree updates the HTML
  ## indices, which is very time consuming (as we install one package at a
  ## time to safeguard against limits on the size of the command line).
  ## Hence, we install the packages to a different library tree
  ## (${R_HOME}/Packages).
  #for(pkg in pkgs_to_install){
    #echo -n "${p}: " >> ../time_i.out
    #/usr/bin/time -o ../time_i.out -a \
  #  system(paste(R, "CMD INSTALL ", pkg, ">",
  #                 paste(path_to_pkg_log, file_separator, pkg, "-test-",
  #                       architecture, "-install.txt", sep=""),
  #                 "2>&1"))
  #}
  ## And now the testing ... (only R-Forge pkgs)
  timings <- numeric(length(pkgs))
  names(timings) <- pkgs
  for(pkg in pkgs){
    writeLines(paste("Checking package", pkg, "..."))
    check_arg <- character()
    if(!is.null(check_args))
      check_arg <- check_args[which(check_args["Package"] == pkg), "check_args"]
    timings[pkg] <- system.time(system(paste(R, "CMD check", check_arg, paste(path_to_pkg_src, pkg, sep = file_separator), ">",
                   paste(path_to_pkg_log, file_separator, pkg, "-", platform, "-",
                         architecture, "-checklog.txt", sep=""),
                 "2>&1")))["elapsed"]
    writeLines(paste("Done in", timings[pkg], "seconds."))
  }
  ## better implementation necessary:
  pkgs_checked <- " "

  ## FINALIZATION

  if(platform == "Linux"){
    ## Close the virtual framebuffer X server 
    close_virtual_X11_fb(pid)
  }
  
  ## send email to R-Forge maintainer which packages successfully were built
  notify_admins(pkgs_checked, donotcompile, email, platform, control, timings = timings, about = "check")
  ## go back to old working directory
  setwd(old_wd)
  TRUE
}
