## Constructor for R-Forge control files
R_Forge_control <- function(path_to_pkg_src, path_to_pkg_log, path_to_pkg_root,
                            path_to_local_texmf, path_to_local_library,
                            stoplist, 
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
                 path_to_local_texmf = path_to_local_texmf,
                 path_to_pkg_log  = path_to_pkg_log,
                 path_to_pkg_root = path_to_pkg_root,
                 path_to_pkg_src  = path_to_pkg_src,
                 stoplist = stoplist),
            class = "R-Forge_control")
}

## build package data base
## reads PACKAGES file in given src (tarball) and SVN repositories

## svn_url: package sources exported from SVN repository
## src_url: built package sources, the .tar.gz 
## win_url: built package binary (Windows), the .zip
## mac_url: built package binary (Mac), the .tgz 

create_package_db_src <- function(svn_url, src_url){
  fields <- .get_rforge_repository_db_fields()
  pkg_db <- list(svn = available.packages(svn_url, fields = fields),
                 src = available.packages(src_url, fields = fields))
  class(pkg_db) <- "pkg_db"
  pkg_db
}

create_package_db_all <- function(svn_url, src_url, win_url, mac_url){
  fields <- .get_rforge_repository_db_fields()
  pkg_db <- list(svn = available.packages(svn_url, fields = fields),
                 src = available.packages(src_url, fields = fields),
                 win = available.packages(win_url, fields = fields),
                 mac = available.packages(mac_url, fields = fields))
  class(pkg_db) <- "pkg_db"
  pkg_db
}

.get_rforge_repository_db_fields <- function(){
  c("Package", "Version", "Repository/R-Forge/Revision")
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

get_buildlog <- function(path_to_pkg_log, pkg, platform, architecture){
paste(file.path(path_to_pkg_log, pkg), "-", platform,
                               "-", architecture, "-buildlog.txt", sep = "")
}

write_prolog <- function(pkg, file, pkg_db, type = c("build", "check"), what = c("tarball", "binary"), std.out = FALSE){
  type <- match.arg(type)
  what <- match.arg(what)
  field = "Repository/R-Forge/Revision"
  if(what == "tarball"){
     pkg_revision <- tryCatch(pkg_db$src[pkg, field], error = identity)
     if(inherits(pkg_revision, "error"))
        pkg_revision <- NA
  }
  else {
     pkg_revision <- tryCatch(pkg_db$src[pkg, field], error = identity)
     if(inherits(pkg_revision, "error"))
        pkg_revision <- NA
  }
  ## R CMD build message
  if(type == "build")
    msg <- paste(date(), ": Building ", what, " for package ", pkg, " (SVN revision ", pkg_revision,
                 ")\n", sep = "")
  ## R CMD check message
  if(type == "check")
    msg <- paste(date(), ": Checking package ", pkg, " (SVN revision ", pkg_revision, ") ...\n", sep = "")
  cat(msg, file = file)
  ## additional information about R version not provided by R CMD build
  if(type == "build")
    cat(paste("using", R.Version()$version.string, "...\n\n"), file = file, append = TRUE)
  if(std.out)
    cat(msg)
}

write_epilog <- function(file, timing, std.out = FALSE){
  cat(paste("Run time:", round(timing, 2L), "seconds.\n"), file = file, append = TRUE)
  if(std.out)
    writeLines(paste("Done in", round(timing, 2L), "seconds."))
}

write_stoplist_notification <- function(pkg, file, type = c("build", "check"), std.out = FALSE){
  type <- match.arg(type)
  ## R CMD build message
  if(type == "build")
    msg <- paste(date(), ": Package ", pkg, " is currently on the stop list ...\n", sep = "")
  ## R CMD check message
  if(type == "check")
    msg <- paste(date(), ": Package ", pkg, " is currently on the stop list ...\n", sep = "")
  cat(msg, file = file)
  ## additional information about R version not provided by R CMD build
  if(type == "build")
    cat(paste("using", R.Version()$version.string, "...\n\n"), file = file, append = TRUE)
  if(std.out)
    cat(msg)
}

get_check_args <- function(pkg, check_args){
  check_arg <- character()
  if(!is.null(check_args)){
    check_arg <- check_args[which(check_args["Package"] == pkg), "check_args"]
  }
  check_arg
}


## DEPRECATED: only tarballs get checked
## returns the location given a package to be checked
## priority tarball, svn. criterium: highest revision
check_from_location <- function(pkg, pkg_db, field = "Repository/R-Forge/Revision"){
  svn_revision <- pkg_db$svn[pkg, field]
  src_revision <- tryCatch(pkg_db$src[pkg, field], error = identity)
  if(inherits(src_revision, "error"))
    src_revision <- NA
  if(!any(c(is.na(svn_revision), is.na(src_revision))))
    if(src_revision >= svn_revision){
      path <- gsub("file:///", "", pkg_db$src[pkg, "Repository"])
      return(file.path(path, sprintf("%s_%s.tar.gz", pkg, pkg_db$src[pkg, "Version"])))
    }
  path <- gsub("file:///", "", pkg_db$svn[pkg, "Repository"])
  file.path(path, pkg)
}
