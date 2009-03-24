## rforge_prepare_pkgs in R

library(RPostgreSQL)
library(tools)

logfile = "/var/log/R/Rbuild.log"

# directory where the work is done
# Note: $pkg_dir should absolute (required for the R code)
pkg_dir = "/srv/rsync/pkgs"

## svn repository
svn_dir = "/srv/svn"

## GForge plugin RPlugin -> target DB table
table = "plugin_rforge_package_db"

## doesn't work, but let's pretend it does...
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname="gforge", user="plugin_rforge", password="jenslf0r2");

cat("--------------------------\n- Exportation of packages\n- ", date(), "\n\n");



if (!file.exists(pkg_dir)) create.dir(pkg_dir, recursive=TRUE)

setwd(pkg_dir)
system("rm -rf *");

pkgs = character()

dbSendQuery(con, "BEGIN;")
dbSendQuery(con, paste("DELETE FROM", table, ";"));

packages_list = readLines("http://cran.at.r-project.org/src/contrib/PACKAGES")

svnreps = list.files(svn_dir)

for (rep in svnreps) {
	cat(":", rep, ":\n")
	
	if (file.exists(paste(svn_dir, "/", rep, "/format", sep=""))) {
		descs = grep("^pkg\/([^\/]+\/)?DESCRIPTION$", 
			system(sprintf("svnlook tree --full-paths %s/%s", svn_dir, rep), intern=TRUE), 
			value=TRUE, perl=TRUE)
		for (desc in descs) {
			descname = paste(rep, ".", gsub("/", ".", desc), sep="")
			system(sprintf("svn export file://%s/%s/%s %s", 
				svn_dir, rep, desc, descname))
			## change from perl: don't use extract_desc
			full_desc = paste(readLines(descname), collapse="\n")
			vars = tryCatch(tools:::.read_description(descname), error=function(e) "ERROR")
			if (vars[1]=="ERROR") {warning("Improper Desc Format");next}
			pkg_name = vars['Package']
			
			## check for package collisions
			if (! is.na(pkgs[pkg_name])) {
				conflict_error(pkg_name, pkgs[pkg_name], rep, vars['Maintainer'])
				next
			}
			
			rt = getRevTime(file.path(svn_dir, rep), dirname(desc))
			rev = rt[[1]]; time = rt[[2]]
			check_errors <- tryCatch(tools:::.check_package_description(descname), error=function(e) "ERROR")
			if(length(check_errors) > 0) { warning("Improper Desc Format");next}
			### TODO: Resume Perl-script line #87
			
		}
	}
}





## extract the cran version of pkg from packages_list
get_cran_version <- function(pkg) {
	stop() # TODO
}

## send out email in case of naming conflict
conflict_error <- function(pkg1, pkg2, rep, email) {
	stop()
}

## get revison and time of that revision from svn 
## maybe use getRevTime2 ???
getRevTime <- function(repos, path) {
	stop()
}

dbDisconnect(con)
dbUnloadDriver(drv)
