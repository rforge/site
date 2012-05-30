## rf Windows Toolchain: rf_build_pkgs.R
## R-Forge Windows build/check system script
## This script loads the R-Forge infrastructure package which includes
## all binary-building code
## In previous versions this file was called 'rforge_build_packages.R'
## Licence GPL-2
## Author: Stefan Theussl
## Last Change: 2012-05-30

require( "rfTools", lib = "R:/lib/local" )
## local library path (this is where the packages get installed to)
maj.version <- paste(R.Version()$maj, unlist(strsplit(R.Version()$min, "[.]"))[1], sep=".")
local_lib <- file.path(Sys.getenv("R_LIBS"), maj.version)

## environment variables from Kurt's check script
Sys.setenv(R_BROWSER="false")
Sys.setenv(R_PDFVIEWER="false")

## make bioc URLs
R_flavor <- switch( R.version$status,
                    "Patched" = "R-patched",
                    "Under development (unstable)" = "R-devel",
                    "R-patched" )
bioc_url <- ifelse( R_flavor == "R-devel",
                    "http://bioconductor.statistik.tu-dortmund.de/packages/2.11/bioc",
                    "http://bioconductor.statistik.tu-dortmund.de/packages/2.10/bioc")
bioc_data <- ifelse( R_flavor == "R-devel",
                    "http://bioconductor.statistik.tu-dortmund.de/packages/2.11/data/annotation",
                    "http://bioconductor.statistik.tu-dortmund.de/packages/2.10/data/annotation")
bioc_exp <- ifelse( R_flavor == "R-devel",
                    "http://bioconductor.statistik.tu-dortmund.de/packages/2.11/data/experiment",
                    "http://bioconductor.statistik.tu-dortmund.de/packages/2.10/data/experiment")

## we have to set 'R_LIBS' again otherwise package dependencies are not found
Sys.setenv("R_LIBS" = local_lib)

## STAGING AREA
stmp <- "T:"

## build root, where the builds/checks are done
build_root <- "R:/run/building"
if(!file.exists(build_root))
  dir.create(build_root)

system("mount \\\\cloudnc2.wu.ac.at\\srv\\nfs\\rf\\staging T:")
src_dir <- rf_takeover_prepared_build(stmp, build_root, type = "win")
src_dir

if( is.null(src_dir) ){
    system("umount -f T:")
    q( save = "no" )
}

## load pkg meta info
load( file.path(build_root, src_dir, "PKG_STAT.rda") )

## setup control file
control <- rf_build_control(path_to_pkg_src  = file.path(build_root, src_dir),            ## R-Forge uncompressed pkg source
                            path_to_pkg_log  = file.path(build_root, src_dir, "RF_LOGS"), ## Log directory
                            path_to_pkg_root = file.path(build_root, src_dir, "RF_PKG_ROOT"), ## R-Forge root (contains /src ,/bin)
                            path_to_local_texmf = "",                      ## path to local texmf
                            path_to_local_library = local_lib,             ## path to local pkg library
                            path_to_check_dir = file.path(build_root, src_dir, "RF_PKG_CHECK"), ## path to check dir
                            stoplist = file.path(system.file("stoplists", package = "rfTools"), "windows.csv"),      ## path to stoplist
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
res <- utils::tar( file.path(stmp, paste("WIN", src_dir, "tar.gz", sep = ".")),
                  files = files, compression = "gzip", compression_level = 9, tar = TAR,
                  extra_flags = sprintf("-C %s", build_root) )

## cleanup build dir
unlink(file.path(build_root, src_dir), recursive = TRUE)

## and remove uncompressed src submission
file.remove( file.path(stmp, paste("SRC.", src_dir, ".tar.gz.processing.WIN", sep = "")) )

system("umount -f T:")
TRUE
