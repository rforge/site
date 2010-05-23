## rforge_prepare_pkgs
## Author: Stefan Theussl
## The following functions search for DESCRIPTION under directory pkg of all R-Forge
## SVN repositories. It then exports all found DESCRIPTION files, builds up
## the package database and exports the package source codes. 

## NOTE: can be removed when it is included in the RForgeTools package


##############################
## Install rf DB on new system
##############################

con <- dbConnect(PgSQL(), user="postgres", password="")

## 
logfile <- "~/Rbuild_temp.log"
#logfile <- "/var/log/R/Rbuild.log"

# directory where the work is done
# Note: $pkg_dir should absolute (required for the R code)
pkg_dir <- "/home/theussl/pkgs_temp"
#pkg_dir <- "/srv/rsync/pkgs"

## svn repository
svn_dir <- "/srv/svn"
svn_url <- "svn://svn.r-forge.r-project.org/svnroot/"

## GForge plugin RPlugin -> target DB table
table <- "plugin_rforge_package_db"




rf_get_pkgs_from_db <- function( rf ){
  dbSendQuery( rf_get_db_con(rf), sprintf("SELECT pkg_name,version,rev FROM %s", table) )
}

rf_get_db_con <- function( rf ){
  stopifnot( inherits(rf, "rf") )
  rf$db_con
}

rf_create <- function( conf <- system.file( package = "rfTools" ) ){
  structure( list(db_con = dbConnect(PgSQL(),
                                     dbname="gforge",
                                     user="plugin_rforge",
                                     password="jenslf0r2")),
             class = "rf" )
}


db
.get_initial_sql_queries <- function(){
  c("CREATE TABLE "
}


pkgs_in_db <- 


dbSendQuery <- function(con, sql) {cat("SQL:\n", sql, "\n") }


##cat("--------------------------\n- Exportation of packages\n- ", date(), "\n\n");


pkgs = character()

dbSendQuery(con, "BEGIN;")
dbSendQuery(con, paste("DELETE FROM", table, ";"));






## Finish DB operations and clean up
dbSendQuery(con, "COMMIT;")
system(sprintf('rm -f "%s"/*.DESCRIPTION', pkg_dir))
#dbDisconnect(con)
#dbUnloadDriver(drv)


## START copy from perl-script
path_to_pkg_src <- pkg_dir
## build pkg database out of DESCRIPTION files
pkg_db <- tools:::.build_repository_package_db(path_to_pkg_src, unpacked = TRUE)
## packages probably not working
pkgs_defunct <- NULL
## which packages have bad DESCRIPTION files?
for(pkg in names(pkg_db)){
 pkg_DESCRIPTION <- file.path(path_to_pkg_src, pkg, "DESCRIPTION")
 if(file.exists(pkg_DESCRIPTION)){
  checked <- tryCatch(tools:::.check_package_description(pkg_DESCRIPTION), error=function(e) 1)
  if(length(checked) > 0)
    pkgs_defunct <- c(pkgs_defunct, pkg)
 }
}
## remove defunct packages from package db
## TODO: show that pkg is not working on R-forge( give hint: wrong DESCRIPTION file)
if(!is.null(pkgs_defunct))
 pkg_db <- pkg_db[!is.element(names(pkg_db), pkgs_defunct)]

## create PACKAGES in source dir
write.dcf(do.call(rbind, pkg_db), file = file.path(path_to_pkg_src, "PACKAGES"))

## END copy from perl-script

cat("\n- ", date(), " - done\n--------------------------\n\n");


## external missing

##########
## DONE ##
##########



