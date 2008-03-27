## R-Forge package building
## R script for debugging build system
## theussl 2008-03
##

## instead of calling library("RForgeTools") we just load some needed source files

source("~/svn/RForgeTools/R/utilities.R")
source("~/svn/RForgeTools/R/update_package_db.R")

## IMPORTANT:
## setup control file
control <- list()
control$path_to_pkg_src            <- "/srv/R/tmp"             ## R-Forge pkg sourced tmp for testing purposes
control$path_to_pkg_log            <- "/home/theussl/log"             ## Log directory
control$path_to_pkg_root           <- "/home/theussl/test"          ## R-Forge root (contains /src ,/bin)
control$path_to_local_texmf        <- "/srv/R/share/texmf"      ## path to local texmf
control$path_to_local_library      <- Sys.getenv("R_LIBS")      ## path to local pkg library
control$stoplist                   <- ""                      ## path to stoplist
## mail configuration
control$mail_domain_name_of_sender <- system("hostname -f", intern = TRUE) ## "xmaragorn64.wu-wien.ac.at" 
control$mail_relay_server <- "statmath.wu-wien.ac.at"           ## only necessary with sendEmail
control$mail_programme <- "mail"                                ## on Windows: sendEmail

## start building ...
## typically here is a call 
## build_packages(email        = "stefan.theussl@wu-wien.ac.at",
##               platform     = "Linux",
##               architecture = "x86_64",
##               control      = control)

## function call will provide the following variables
platform <- "Linux"
architecture <- "x86_64"
rforge_url <- "http://R-Forge.R-project.org"
cran_url <- "http://CRAN.R-project.org"

## now first lines of code of build_packages

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
    avail_rforge <- available.packages(contriburl = contrib.url(rforge_url, type = "source"))
    pkgs <- avail_rforge[, 1]
    pkgs_other <- setdiff(pkgs_all, pkgs)
  }
  ## Sort out packages that are on the exclude list
  pkgs <- remove_excluded_pkgs(pkgs, donotcompile)
  pkgs_other <- remove_excluded_pkgs(pkgs_other, donotcompile)


## DEBUGING starts here
## We want to test package db update

## normally here is: update_package_library(pkgs, path_to_pkg_src, cran_url, path_to_local_library)
  repository_url <- cran_url
  lib <- path_to_local_library
  update.packages(lib = lib, repos = repository_url, ask = FALSE)
  
  ##source("${R_scripts_dir}/packages.R")
  ##source("${R_scripts_dir}/R_Forge_utils.R")
  ##dir <- file_path_as_absolute(getwd())

