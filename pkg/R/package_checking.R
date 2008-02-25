## code for checking packages with R
## originally based on kurt hornik's check-R
## theussl 2007-02

## from check-R
## R-flavors
#R_flavors='R-devel R-patched R-release'
#CRAN_rsync=${R_dir}/CRAN
## CRAN's src/contrib directory
#CRAN_dir=${CRAN_rsync}/src/contrib
## local R-Forge mirror
#R_FORGE=${R_dir}/pkgs
## provide R-Forge packages in
#R_Forge_contrib_dir=${R_dir}/R-Forge/src/contrib 

## R profile for checking.
#R_profile=${R_scripts_dir}/check_profile.R
## where the work is done
#check_dir=${R_dir}/R.check
## Check date in ISO 8601 format.  GNU specific ...
#check_date=`date -Idate`
## Check result files
#check_results_files="SUMMARY check.csv summary.rds time_c.out time_i.out"

## Set permissions right
#umask 022

##if test -n "${BASH_VERSION}"; then
  ## No process is allowed more than 10 minutes
 # ulimit -t 600
#fi

##export _R_CHECK_WEAVE_VIGNETTES_=no
##export _R_CHECK_SUBDIRS_STRICT_=yes
## We really need 'false' for R < 2.4.0 ...
##export _R_CHECK_FORCE_SUGGESTS_=false

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
  check_log_directory(path_to_pkg_log, path_separator, type = "build")
  ## check if the package check directory (the directory where the check
  ## process takes place) exists.
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
  pkgs <- avail_src
  ## Sort out packages that are on the exclude list
  pkgs <- remove_excluded_pkgs(pkgs, donotcompile)
  
  ## LAST PREPARATION BEFORE CHECKING

  setwd(path_to_check_dir)
  ## Further steps currently missing
  ## where is our R binary?
  R <- paste(R.home(), "bin", "R", sep=path_separator)
  setwd(path_to_pkg_src)
  ## Set environment variables which are necessary for checking
  Sys.setenv(R_LIBS = path_to_local_library)
  ## Calculate dependency structure
  dep_struct <- resolve_dependency_structure(pkgs, cran_url, path_to_pkg_src)
  pkgs_cran <- dep_struct["CRAN"]
  pkgs_to_install <- dep_struct["INSTALL_ORDER"]
  ## Start a virtual framebuffer X server and use this for DISPLAY so that
  ## we can run package tcltk and friends.  
  pid <- start_virtual_X11_fb()
 
  ## PACKAGE CHECKING
   
  ## Installation first ...
  ## Note that installing to the default library tree updates the HTML
  ## indices, which is very time consuming (as we install one package at a
  ## time to safeguard against limits on the size of the command line).
  ## Hence, we install the packages to a different library tree
  ## (${R_HOME}/Packages).
  ## TODO: Timings
  ##       fake install and checking
  for(pkg in pkgs_to_install){
    #echo -n "${p}: " >> ../time_i.out
    #/usr/bin/time -o ../time_i.out -a \
    system(paste(R, "CMD INSTALL ", pkg, ">",
                   paste(path_to_pkg_log, path_separator, pkg, "-test-",
                         architecture, "-install.txt", sep=""),
                   "2>&1"))
  }
  ## And now the testing ... (only R-Forge pkgs)
  for(pkg in pkgs){
    system(paste(R, "CMD check --library=", path_to_local_library, pkg, ">",
                   paste(path_to_pkg_log, path_separator, pkg, "-test-",
                         architecture, "-checklog.txt", sep=""),
                 "2>&1"))
  }
  ## better implementation necessary:
  pkgs_checked <- pkgs

  ## FINALIZATION
  
  close_virtual_X11_fb(pid)
  ## send email to R-Forge maintainer which packages successfully were built
  notify_admins(pkgs_checked, donotcompile, email, platform, control)
  ## go back to old working directory
  setwd(old_wd)
  TRUE
}






