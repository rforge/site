## Upload built packages to release area
##############################

## The following steps are needed to upload packages to the build area
## (1) check if all builds are available
## (2) copy log files
## (3) delete builds which failed and add successful builds

## (1) make pkg status info based on SVN; retrieves further info from pkg db and CRAN
rf_release_packages <- function( rfc, release_dir, log_dir, backup_dir = NULL, verbose = FALSE, keep = FALSE ){

  stmp <- .rf_get_tmp(rfc)
  stopifnot( file.exists(stmp) )
  
  btgz <- grep("^SRC.build_.*?.tar.gz$", dir(stmp), value = TRUE)
  if(any(is.na(btgz)))
    return( NULL )
  if( !length(btgz) )
    return(NULL)

  check_tgz <- function(btgz){
    ## if given submission is already being processed exit and return NULL
    ##  sincethe release of R 3.0 Mac is not supported anymore for the time being (prebuild binnaries from r.resarch need OSX >= 10.6)
    out <- c( mac = TRUE, win = FALSE )
    #out["mac"] <- file.exists(file.path(stmp, gsub("SRC.", "MAC.", btgz))) && !file.exists( file.path(stmp, sprintf("%s.processing.MAC", btgz)) ) 
    out["win"] <- file.exists(file.path(stmp, gsub("SRC.", "WIN.", btgz))) && !file.exists( file.path(stmp, sprintf("%s.processing.WIN", btgz)) ) 
    out
    
    ptgz <- paste(btgz, "merging", sep = ".")
    
    if( all(out) && !file.exists(file.path(stmp, ptgz)) )
      return( ptgz )
    return( NULL )
  }

  ptgz <- lapply( btgz, check_tgz)
  nn <- !unlist(lapply(ptgz, is.null))
  if (!any(nn))
    return(NULL)

  ptgz <- unlist(ptgz[nn])[1]
  btgz <- btgz[nn][1]
  src_dir <- gsub( "SRC.", "", gsub(".tar.gz", "", btgz) )

  ## create lock file
  file.create( file.path(stmp, ptgz) )

  ## uncompress staging area
  TAR <- Sys.getenv("TAR")
  WINDOWS <- .Platform$OS.type == "windows"
  if (!nzchar(TAR)) {
    TAR <- if (WINDOWS)
      "tar --no-same-owner --force-local"
    else "internal"
  }

  ## SRC PACKAGES
  src_build_file <- file.path(stmp, btgz)
  res <- utils::untar( src_build_file, compressed = "gzip", tar = TAR, exdir = stmp)
  if (res) {
    stop("unpackaging SRC builds failed.")
  }
  ##
  sym <- load(file.path(stmp, src_dir, "PKG_STAT.rda"))
  pkg_status <- get(sym)
  pkgs <- names(pkg_status$outdated)

  build_states <- c(0, 3, 4, 5)
  status <- pkg_status$db[pkgs, "status"]
  status[ is.na(status) ] <- 5
  pkgs <- pkgs[status %in% build_states]
  
  ## copy 00install.out, build, and check logs
  lapply(pkgs, rf_copy_logs, log_dir = log_dir, build_root = file.path(stmp, src_dir), type = "Linux" )          
  ## read check results
  file <- file.path(stmp, src_dir, "RF_PKG_CHECK", "check.csv")
  results_lin <- if(file.exists(file))
    read.csv(file)
  else
    NULL
  
  ## MAC BINARIES (not  supported see above)
  #mac_build_file <- file.path( stmp, gsub("SRC.", "MAC.", btgz) )
  #res <- utils::untar( mac_build_file, compressed = "gzip", tar = TAR, exdir = stmp )
  #if (res) {
  #  stop("unpackaging MAC builds failed.")
  #}

  ## copy 00install.out, build, and check logs
  #lapply(pkgs, rf_copy_logs, log_dir = log_dir, build_root = file.path(stmp, src_dir), type = "MacOSX" )
  ## read check results
  #file <- file.path(stmp, src_dir, "RF_PKG_CHECK", "check.csv")
  #results_mac <- if(file.exists(file))
  #  read.csv(file)
  #else
  #  NULL
  results_mac <- NULL
  
  ## WIN BINARIES
  win_build_file <- file.path( stmp, gsub("SRC.", "WIN.", btgz) )
  res <- utils::untar( win_build_file, compressed = "gzip", tar = TAR, exdir = stmp )
  if (res) {
    stop("unpackaging WIN builds failed.")
  }
  
  ## copy 00install.out, build, and check logs
  lapply(pkgs, rf_copy_logs, log_dir = log_dir, build_root = file.path(stmp, src_dir), type = "Windows" )
  ## read check results
  file <- file.path(stmp, src_dir, "RF_PKG_CHECK", "check.csv")
  results_win <- if(file.exists(file))
    read.csv(file)
  else
    NULL
  
  all_results <- matrix(NA, ncol = 3, nrow = length(pkgs), dimnames = list(pkgs, c("Linux", "MacOSX", "Windows")))

  if( !is.null(results_lin) ){
    p <- as.character(results_lin[["Package"]])
    all_results[p, "Linux"] <- as.character(results_lin[["Status"]])
  }
  if( !is.null(results_mac) ){
    p <- as.character(results_mac[["Package"]])
    all_results[p, "MacOSX"] <- as.character(results_mac[["Status"]])
  }
  if( !is.null(results_win) ){
    p <- as.character(results_win[["Package"]])
    all_results[p, "Windows"] <- as.character(results_win[["Status"]])
  }
  
  ## which pkgs pass R CMD check on major platforms
  ## leave out MacOSX for the moment
  ## check os type field
  os_type <- unlist(lapply(pkg_status$outdated[pkgs], function(x) x$description["OS_type"]))
  if( length(os_type) ){
    names(os_type) <- pkgs
    unix <- which(os_type == "unix")
    if(length(unix))
      all_results[unix, "Windows"] <- "OK"
    windows <- which(os_type == "windows")
    if(length(windows))
      all_results[windows, c("Linux", "MacOSX")] <- "OK"
  }

  ## FIXME: needs to be made generic
  na.replace <- function(object, ...){
    object[is.na(object)] <- FALSE
    object
  }
  
  pkgs_ok <- apply(all_results[, c("Linux", "Windows"), drop = FALSE], 1, function(x){y <- na.replace(x)
                                                                        if(length(y))
                                                                          all(y %in% c("OK", "WARN"))
                                                                        else
                                                                          FALSE})
  ## Check binary release areas
  .rf_check_package_binary_areas( release_dir )
  
  ## remove pkgs which fail and flag as failed
  pkgs_fail <- pkgs[ !pkgs_ok ]
  .rf_remove_package_from_release( pkgs_fail, release_dir )
  lapply( pkgs_fail, function(pkg) rf_set_pkg_status( rfc, pkg, status = 3L) )
  
  ## now provide build successes to release area (after deleting old packages)
  pkgs_ok <- names(pkgs_ok)[pkgs_ok]
  .rf_remove_package_from_release( pkgs_ok, release_dir )
  .rf_release_packages( pkgs_ok, file.path(stmp, src_dir, "RF_PKG_ROOT"), release_dir )
  
  lapply( pkgs_ok, function(pkg) rf_set_pkg_status( rfc, pkg, status = 0L) )
  
  ## remove build tgz'
  #file.remove( mac_build_file )
  file.remove( win_build_file )

  if( keep ){
    stopifnot( file.info( backup_dir )$isdir )
    ## keep the latest 72 build/check attempts (covers about 12 weeks) as backup
    file.copy( src_build_file, backup_dir )
    backup_builds <- grep( "^SRC.build_.*.tar.gz$", dir(backup_dir), value = TRUE )
    to_remove <- tail(backup_builds, n = -72)
    if( length(to_remove) )
      file.remove( file.path(backup_dir, backup_builds) )
  }
  
  ## remove source builds
  file.remove( src_build_file )

  ## remove merge area
  unlink( file.path(stmp, src_dir), recursive = TRUE )
  
  ## remove lock file
  file.remove(file.path(stmp, ptgz))
  
  list( ERROR = pkgs_fail, OK = pkgs_ok )
}

rf_copy_logs <- function(pkg, log_dir, build_root, type = c("Linux", "MacOSX", "Windows")){
  type <- match.arg(type)
  if( !file.exists(file.path(log_dir, "build")) )
    dir.create( file.path(log_dir, "build") )
  if( !file.exists(file.path(log_dir, "check")) )
    dir.create( file.path(log_dir, "check") )
  if( !file.exists(file.path(log_dir, "install")) )
    dir.create( file.path(log_dir, "install") )
  ## build log
  buildlog <- file.path( build_root, "RF_LOGS", sprintf("%s-%s-%s-buildlog.txt", pkg, type, ifelse(type == "Linux", "all", "x86_64")) )
  buildout <- file.path( log_dir, "build", basename(buildlog) )
  if( file.exists(buildout) )
    file.remove( buildout )
  if( file.exists(buildlog) )
    file.copy( buildlog, buildout )
  ## check log
  checklog <- file.path( build_root, "RF_LOGS", sprintf("%s-%s-x86_64-checklog.txt", pkg, type) )
  checkout <- file.path( log_dir, "check", basename(checklog) )
  if( file.exists(checkout) )
    file.remove( checkout )
  if( file.exists(checklog) )
    file.copy( checklog, checkout )
  ## install log as obtained by R CMD check
  installog <- file.path( build_root, "RF_PKG_CHECK", "PKGS", sprintf("%s.Rcheck", pkg), sprintf("00install.out", pkg) )
  installout <- file.path( log_dir, "install", sprintf("%s-%s-install.out", pkg, type) )
  if( file.exists(installout) )
    file.remove( installout )
  if( file.exists(installog) )
    file.copy( installog, installout )
  invisible( TRUE )
}

.rf_check_package_binary_areas <- function( release_dir ){
  area_win <- contrib.url(sprintf("%s", release_dir), type = "win.binary")
  area_mac <- contrib.url(sprintf("%s", release_dir), type = "mac.binary.leopard")

  ## FIXME: we need to check symlink "latest" as well, must point to the latest minor release
  if(!file.exists(area_win)){
    dir.create(area_win)
    tools::write_PACKAGES( dir = area_win )
    file.remove( file.path(dirname( area_win ), "latest") )
    file.symlink( basename(area_win), file.path(dirname( area_win ), "latest")  )
  }
  if(!file.exists(area_mac)){
    dir.create(area_mac)
    tools::write_PACKAGES( dir = area_mac )
    file.remove( file.path(dirname( area_mac ), "latest") )
    file.symlink( basename(area_mac), file.path(dirname( area_mac ), "latest") )
  }
}

.rf_remove_package_from_release <- function(pkgs, release_dir){

  ## further configuration
  file_types <- c(Linux = ".tar.gz", MacOSX = ".tgz", Windows = ".zip")
  pkg_types <- c(Linux = "source", MacOSX = "mac.binary", Windows = "win.binary")
  ## R-Forge repository offers additional "Revision" field.
  fields <- c(tools:::.get_standard_repository_db_fields(), "Repository", "Repository/Project", "Repository/R-Forge/Revision", "Repository/R-Forge/DateTimeStamp")

  ## remove SRC packages
  pkgs_rforge_avail <- available.packages(contrib.url(sprintf("file://%s", release_dir)), filters = "duplicates")
  toremove <- which( rownames(pkgs_rforge_avail) %in% pkgs )
  files <- file.path(contrib.url(release_dir), sprintf("%s_%s.tar.gz", pkgs_rforge_avail[toremove, "Package"], pkgs_rforge_avail[toremove, "Version"]))
  lapply( files, function(file) {if(file.exists(file))
                                   file.remove(file)
                               } )
  ## write PACKAGES file
  platform <- "Linux"
  file_type <- file_types[platform]
  pkg_type <- pkg_types[platform]
  write_PACKAGES(dir = contrib.url(release_dir), fields = fields, type = pkg_type)

  ## remove WIN packages
  pkgs_rforge_avail_win <- available.packages(contrib.url(sprintf("file://%s", release_dir), type = "win.binary"), filters = "duplicates")
  toremove <- which( rownames(pkgs_rforge_avail_win) %in% pkgs )
  files <- file.path(contrib.url(release_dir, type = "win.binary"), sprintf("%s_%s.zip", pkgs_rforge_avail_win[toremove, "Package"], pkgs_rforge_avail_win[toremove, "Version"]))
  lapply( files, function(file) {if(file.exists(file))
                                   file.remove(file)
                               } )
  ## write PACKAGES file
  platform <- "Windows"
  file_type <- file_types[platform]
  pkg_type <- pkg_types[platform]
  write_PACKAGES(dir = contrib.url(release_dir, pkg_type), fields = fields, type = pkg_type)
  
  ## remove MAC packages
  #pkgs_rforge_avail_mac <- available.packages(contrib.url(sprintf("file://%s", release_dir), type = "mac.binary.leopard"), filters = "duplicates")
  #toremove <- which( rownames(pkgs_rforge_avail_mac) %in% pkgs )
  #files <- file.path(contrib.url(release_dir, type = "mac.binary.leopard"), sprintf("%s_%s.tgz", pkgs_rforge_avail_mac[toremove, "Package"], pkgs_rforge_avail_mac[toremove, "Version"]))
  #lapply( files, function(file) {if(file.exists(file))
  #                                 file.remove(file)
  #                             } )
  ## write PACKAGES file
  #platform <- "MacOSX"
  #file_type <- file_types[platform]
  #pkg_type <- pkg_types[platform]
  #write_PACKAGES(dir = contrib.url(release_dir, "mac.binary.leopard"), fields = fields, type = pkg_type)

  invisible(TRUE)
}

.rf_release_packages <- function(pkgs, path, release_dir){

  ## further configuration
  file_types <- c(Linux = ".tar.gz", MacOSX = ".tgz", Windows = ".zip")
  pkg_types <- c(Linux = "source", MacOSX = "mac.binary", Windows = "win.binary")
  ## R-Forge repository offers additional "Revision" field.
  fields <- c(tools:::.get_standard_repository_db_fields(), "Repository", "Repository/Project", "Repository/R-Forge/Revision", "Repository/R-Forge/DateTimeStamp")

  ## further configuration
  file_types <- c(Linux = ".tar.gz", MacOSX = ".tgz", Windows = ".zip")
  pkg_types <- c(Linux = "source", MacOSX = "mac.binary", Windows = "win.binary")

  ## SRC package must always be there
  pkgs_src_avail <- available.packages(contrib.url(sprintf("file://%s", path)), filters = "duplicates")
  files <- file.path(contrib.url(path), sprintf("%s_%s.tar.gz", pkgs_src_avail[pkgs, "Package"], pkgs_src_avail[pkgs, "Version"]))
  lapply( files, function(file) {if(file.exists(file))
                                   file.copy(file, contrib.url(release_dir))
                               } )
  ## write PACKAGES file
  platform <- "Linux"
  file_type <- file_types[platform]
  pkg_type <- pkg_types[platform]
  write_PACKAGES(dir = contrib.url(release_dir), fields = fields, type = pkg_type)

  ## WIN packages
  pkgs_win_avail <- available.packages(contrib.url(sprintf("file://%s", path), type = "win.binary"), filters = "duplicates")
  torelease <- which( rownames(pkgs_win_avail) %in% pkgs )
  files <- file.path(contrib.url(path, type = "win.binary"), sprintf("%s_%s.zip", pkgs_win_avail[torelease, "Package"], pkgs_win_avail[torelease, "Version"]))
  lapply( files, function(file) {if(file.exists(file))
                                   file.copy(file, contrib.url(release_dir, type = "win.binary"), copy.mode = FALSE)
                               } )
  ## write PACKAGES file
  platform <- "Windows"
  file_type <- file_types[platform]
  pkg_type <- pkg_types[platform]
  write_PACKAGES(dir = contrib.url(release_dir, pkg_type), fields = fields, type = pkg_type)


  ## MAC packages
  #pkgs_mac_avail <- available.packages(contrib.url(sprintf("file://%s", path), type = "mac.binary.leopard"), filters = "duplicates")
  #torelease <- which( rownames(pkgs_mac_avail) %in% pkgs )
  #files <- file.path(contrib.url(path, type = "mac.binary.leopard"), sprintf("%s_%s.tgz", pkgs_mac_avail[torelease, "Package"], pkgs_mac_avail[torelease, "Version"]))
  #lapply( files, function(file) {if(file.exists(file))
  #                                 file.copy(file, contrib.url(release_dir, type = "mac.binary.leopard"), copy.mode = FALSE)
  #                             } )
  ## write PACKAGES file
  #platform <- "MacOSX"
  #file_type <- file_types[platform]
  #pkg_type <- pkg_types[platform]
  #write_PACKAGES(dir = contrib.url(release_dir, "mac.binary.leopard"), fields = fields, type = pkg_type)

  invisible(TRUE)
}
