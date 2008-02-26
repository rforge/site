## Various utilities

## Function definition of version ordering functions
## decompose given version number (character) in numeric major and minor part
decompose_version <- function(x){
  splitted <- as.numeric(unlist(strsplit(x, "-")))
  if(length(splitted) == 1)
     return(c(major = splitted, minor = NA))
  c(major = splitted[1], minor = splitted[2])
}

## given two version number this function returns the order of them
version_order <- function(x){
  if(!is.character(x))
    stop("A version number has to be of type character!")
  if(! length(x) == 2)
    warning("More than 2 version numbers detected: The order is calculated only from the first 2 elements in the vector!")
  x_decomposed <- decompose_version(x[1])
  y_decomposed <- decompose_version(x[2])
  if(x_decomposed["major"] != y_decomposed["major"])
    return(order(c(x_decomposed["major"],y_decomposed["major"])))
  order(c(x_decomposed["minor"],y_decomposed["minor"]))
}

## resolve dependencies helper 
resolve_dependencies <- function(pkgs,available){
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

.make_suggests_list <- function(pkgs,available){
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
  bundles <- utils:::.find_bundles(available)
  x <- lapply(x, function(x) if (length(x)) {
    for (bundle in names(bundles)) x[x %in% bundles[[bundle]]] <- bundle
         x <- x[!x %in% c("R", "NA")]
    unique(x)
  }
  else x)
  x
}

resolve_suggests <- function(pkgs,available){
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
  lock <- paste(lib,"00LOCK", sep="/")
  if(file.exists(lock))
     system(paste("rm -rf", lock))
}

## check, if packages can be installed to local library
## TODO: rotate (gzip and copy to backup location) old logs
##       to keep track of history
check_log_directory <- function(dir, path_separator, type = c("build", "check")){
  type <- match.arg(type)
  if(!check_directory(dir, fix=TRUE))
    stop(paste("There is no directory", dir, "!"))
  if(type == "build"){
    suffix = "buildlog.txt"
  }else suffix = "checklog.txt"
  system(paste("rm -f ", dir, path_separator, "*", suffix, sep = ""))
}

## Start a virtual framebuffer X server and use this for DISPLAY so that
## we can run package tcltk and friends.  We use a random PID
## as the server number so that the checks for different flavors
## get different servers.
start_virtual_X11_fb <- function(){
  ## FIXME: if /usr/bin/X11 exists -> then setting path not necessary
  Sys.setenv(PATH=paste(Sys.getenv("PATH"), ":/usr/bin/X11"), sep="")
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
