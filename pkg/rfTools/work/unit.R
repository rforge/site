require(rfTools)

## tests:
#rf <- rfTools:::.create_rf_config(db_name = "db_name",
#                                  db_user = "rf_table",
#                                  db_password = "db_pw",
#                                  svn_root = "/srv/svn", tmp_dir = "/tmp")
#testfile <- tempfile()

#rf_write_configuration( rf, file = "/home/rforge/conf/rf.conf" )
#file.remove( testfile )
rf <- rf_read_configuration( file = "/home/rforge/conf/rf.conf" )

rf
unclass(rf)

rfTools:::.check_rf_for_sanity(rf)

## update package status db (which pacakges are brand new, current, or outdated)
pkg_status_cache <- file.path( rfTools:::.rf_get_tmp(rf), "pkg_status_cache.rda" )
if( !file.exists(pkg_status_cache) ){
  pkg_status <- rfTools:::rf_pkg_status(rf)
  save(pkg_status, file = pkg_status_cache)
}else{
  load( pkg_status_cache )
}

## update package db: outdated and brandnew packages => status 1 (scheduled for build)
tobuild <- rf_prepare_build(rf, pkg_status)
## export sources and make them available for build
rf_export_and_build_pkgs( rf, pkg_status, tobuild )

## or rebuild everything
#tobuild <- rf_prepare_build(rf, pkg_status, rebuild = TRUE)

## set build offline
#rf_set_build_offline(rf)

################################################################################
## ON BUILD MACHINE
################################################################################

################################################################################
## SRC

## some parts taken from script rforge_build_packages

require( "rfTools", lib = "/home/rforge/lib/R/" )

## local library path (this is where the packages get installed to)
maj.version <- paste(R.Version()$maj, unlist(strsplit(R.Version()$min, "[.]"))[1], sep=".")
local_lib <- file.path(Sys.getenv("R_LIBS"), maj.version)

## we have to set 'R_LIBS' again otherwise package dependencies are not found
Sys.setenv("R_LIBS" = local_lib)

stmp <- "/srv/rf/staging"
## build root, where the builds/checks are done
build_root <- "/srv/rf/building"
if(!file.exists(build_root))
  dir.create(build_root)

src_dir <- rf_takeover_prepared_build(stmp, build_root)

if( is.null(src_dir) )
  q( save = "no" )

## load pkg meta info
load( file.path(build_root, src_dir, "PKG_STAT.rda") )

## setup control file
control <- rf_build_control(path_to_pkg_src  = file.path(build_root, src_dir),            ## R-Forge uncompressed pkg source
                            path_to_pkg_log  = file.path(build_root, src_dir, "RF_LOGS"), ## Log directory
                            path_to_pkg_root = file.path(build_root, src_dir, "RF_PKG_ROOT"), ## R-Forge root (contains /src ,/bin)
                            path_to_local_texmf = "/usr/local/share/texmf",## path to local texmf
                            path_to_local_library = local_lib,             ## path to local pkg library
                            path_to_check_dir = file.path(build_root, src_dir, "RF_PKG_CHECK"), ## path to check dir
                            stoplist = "/srv/R/lib/check_R_stoplist",      ## path to stoplist
                            cpu_time_limit = 600,                          ## CPU time limit
                            mail_domain_name_of_sender = system("hostname -f", intern = TRUE), ## "xmorthanc.wu.ac.at"
                            mail_relay_server = "",                        ## only necessary with sendEmail
                            mail_programme = "mail")                       ## on Windows: sendEmail

## start building ...
rf_build_packages( rf_pkg_status,
                   platform        = "Linux",
                   architecture    = "x86_64",
                   bioc_url        = "file:///srv/R/Repositories/Bioconductor/release/bioc/",
                   bioc_data       = "file:///srv/R/Repositories/Bioconductor/release/data/annotation",
                   bioc_experiment = "file:///srv/R/Repositories/Bioconductor/release/data/experiment",
                   control         = control )

## check packages
rf_check_packages( rf_pkg_status,
                   platform        = "Linux",
                   architecture    = "x86_64",
                   bioc_url        = "file:///srv/R/Repositories/Bioconductor/release/bioc/",
                   bioc_data       = "file:///srv/R/Repositories/Bioconductor/release/data/annotation",
                   bioc_experiment = "file:///srv/R/Repositories/Bioconductor/release/data/experiment",
                   control         = control )

TAR <- Sys.getenv("TAR")
WINDOWS <- .Platform$OS.type == "windows"
if (!nzchar(TAR)) {
  TAR <- if (WINDOWS)
    "tar --force-local"
  else "internal"
}

res <- utils::tar( file.path(stmp, paste("SRC", src_dir, "tar.gz", sep = ".")),
                  src_dir, compression = "gzip", compression_level = 9, tar = TAR,
                  extra_flags = sprintf("-C %s", build_root) )

## cleanup build dir
unlink(file.path(build_root, src_dir), recursive = TRUE)

## remove uncompressed src submission
file.remove( file.path(stmp, paste(src_dir, ".tar.gz.processing", sep = "")) )

TRUE


################################################################################
## MAC builder:

require( "rfTools", lib = "/Users/rforge/lib/R/" )
## local library path (this is where the packages get installed to)
maj.version <- paste(R.Version()$maj, unlist(strsplit(R.Version()$min, "[.]"))[1], sep=".")
local_lib <- file.path(Sys.getenv("R_LIBS"), maj.version)

## we have to set 'R_LIBS' again otherwise package dependencies are not found
Sys.setenv("R_LIBS" = local_lib)

stmp <- "/srv/rf/staging"
## build root, where the builds/checks are done
build_root <- "/srv/rf/building"
if(!file.exists(build_root))
  dir.create(build_root)

src_dir <- rf_takeover_prepared_build(stmp, build_root, type = "mac")

if( is.null(src_dir) )
  q( save = "no" )

## load pkg meta info
load( file.path(build_root, src_dir, "PKG_STAT.rda") )

## setup control file
control <- rf_build_control(path_to_pkg_src  = file.path(build_root, src_dir),            ## R-Forge uncompressed pkg source
                            path_to_pkg_log  = file.path(build_root, src_dir, "RF_LOGS"), ## Log directory
                            path_to_pkg_root = file.path(build_root, src_dir, "RF_PKG_ROOT"), ## R-Forge root (contains /src ,/bin)
                            path_to_local_texmf = "/usr/local/share/texmf",## path to local texmf
                            path_to_local_library = local_lib,             ## path to local pkg library
                            path_to_check_dir = file.path(build_root, src_dir, "RF_PKG_CHECK"), ## path to check dir
                            stoplist = "/srv/R/lib/check_R_stoplist",      ## path to stoplist
                            cpu_time_limit = 600,                          ## CPU time limit
                            mail_domain_name_of_sender = system("hostname", intern = TRUE), ## "xmorthanc.wu.ac.at"
                            mail_relay_server = "",                        ## only necessary with sendEmail
                            mail_programme = "mail")                       ## on Windows: sendEmail

## start building ...
rf_build_packages( rf_pkg_status,
                   platform        = "MacOSX",
                   architecture    = "x86_64",
                   bioc_url        = "http://bioconductor.statistik.tu-dortmund.de/packages/2.9/bioc",
                   bioc_data       = "http://bioconductor.statistik.tu-dortmund.de/packages/2.10/data/annotation",
                   bioc_experiment = "http://bioconductor.statistik.tu-dortmund.de/packages/2.10/data/experiment",
                   control         = control )

rf_check_packages( rf_pkg_status,
                   platform        = "MacOSX",
                   architecture    = "x86_64",
                   bioc_url        = "http://bioconductor.statistik.tu-dortmund.de/packages/2.9/bioc",
                   bioc_data       = "http://bioconductor.statistik.tu-dortmund.de/packages/2.10/data/annotation",
                   bioc_experiment = "http://bioconductor.statistik.tu-dortmund.de/packages/2.10/data/experiment",
                   control         = control )

TAR <- Sys.getenv("TAR")
WINDOWS <- .Platform$OS.type == "windows"
if (!nzchar(TAR)) {
  TAR <- if (WINDOWS)
    "tar --force-local"
  else "internal"
}

files <- file.path(src_dir, c("RF_LOGS", "RF_PKG_CHECK", "RF_PKG_ROOT"))
res <- utils::tar( file.path(stmp, paste("MAC", src_dir, "tar.gz", sep = ".")),
                  files = files, compression = "gzip", compression_level = 9, tar = TAR,
                  extra_flags = sprintf("-C %s", build_root) )

## cleanup build dir
unlink(file.path(build_root, src_dir), recursive = TRUE)




################################################################################
## WIN builder:

require( "rfTools", lib = "R:/lib/local" )
## local library path (this is where the packages get installed to)
maj.version <- paste(R.Version()$maj, unlist(strsplit(R.Version()$min, "[.]"))[1], sep=".")
local_lib <- file.path(Sys.getenv("R_LIBS"), maj.version)

## make bioc URLs
R_flavor <- switch( R.version$status,
                    "Patched" = "R-patched",
                    "Under development (unstable)" = "R-devel",
                    "R-patched" )
bioc_url <- ifelse( R_flavor == "R-devel",
                    "http://bioconductor.statistik.tu-dortmund.de/packages/2.10/bioc",
                    "http://bioconductor.statistik.tu-dortmund.de/packages/2.9/bioc")
bioc_data <- ifelse( R_flavor == "R-devel",
                    "http://bioconductor.statistik.tu-dortmund.de/packages/2.10/data/annotation",
                    "http://bioconductor.statistik.tu-dortmund.de/packages/2.9/data/annotation")
bioc_exp <- ifelse( R_flavor == "R-devel",
                    "http://bioconductor.statistik.tu-dortmund.de/packages/2.10/data/experiment",
                    "http://bioconductor.statistik.tu-dortmund.de/packages/2.9/data/experiment")

## we have to set 'R_LIBS' again otherwise package dependencies are not found
Sys.setenv("R_LIBS" = local_lib)

## STAGING AREA
stmp <- "T:"

## build root, where the builds/checks are done
build_root <- "R:/run/building"
if(!file.exists(build_root))
  dir.create(build_root)

src_dir <- rf_takeover_prepared_build(stmp, build_root, type = "win")

if( is.null(src_dir) )
  q( save = "no" )

## load pkg meta info
load( file.path(build_root, src_dir, "PKG_STAT.rda") )

## setup control file
control <- rf_build_control(path_to_pkg_src  = file.path(build_root, src_dir),            ## R-Forge uncompressed pkg source
                            path_to_pkg_log  = file.path(build_root, src_dir, "RF_LOGS"), ## Log directory
                            path_to_pkg_root = file.path(build_root, src_dir, "RF_PKG_ROOT"), ## R-Forge root (contains /src ,/bin)
                            path_to_local_texmf = "",                      ## path to local texmf
                            path_to_local_library = local_lib,             ## path to local pkg library
                            path_to_check_dir = file.path(build_root, src_dir, "RF_PKG_CHECK"), ## path to check dir
                            stoplist = "R:/lib/scripts/check_R_stoplist.txt",      ## path to stoplist
                            cpu_time_limit = 600,                          ## CPU time limit
                            mail_domain_name_of_sender = "stefan7th@r-forge.wu.ac.at", ## "xmorthanc.wu.ac.at"
                            mail_relay_server = "r-forge.wu.ac.at",                    ## only necessary with sendEmail
                            mail_programme = "sendEmail")                       ## on Windows: sendEmail

## start building ...
rf_build_packages( rf_pkg_status,
                   platform        = "Windows",
                   architecture    = "x86_64",
                   bioc_url        = bioc_url,
                   bioc_data       = bioc_data,
                   bioc_experiment = bioc_exp,
                   control         = control )

rf_check_packages( rf_pkg_status,
                   platform        = "Windows",
                   architecture    = "x86_64",
                   bioc_url        = bioc_url,
                   bioc_data       = bioc_data,
                   bioc_experiment = bioc_exp,
                   control         = control )

TAR <- Sys.getenv("TAR")
WINDOWS <- .Platform$OS.type == "windows"
if (!nzchar(TAR)) {
  TAR <- if (WINDOWS)
    "tar --force-local"
  else "internal"
}

files <- file.path(src_dir, c("RF_LOGS", "RF_PKG_CHECK", "RF_PKG_ROOT"))
res <- utils::tar( file.path(stmp, paste("MAC", src_dir, "tar.gz", sep = ".")),
                  files = files, compression = "gzip", compression_level = 9, tar = TAR,
                  extra_flags = sprintf("-C %s", build_root) )

## cleanup build dir
unlink(file.path(build_root, src_dir), recursive = TRUE)
