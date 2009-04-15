## rforge_prepare_pkgs in R

#library(RPostgreSQL)
library(tools)

logfile = "/var/log/R/Rbuild.log"

# directory where the work is done
# Note: $pkg_dir should absolute (required for the R code)
pkg_dir = "/srv/rsync/pkgs"

## svn repository
svn_dir = "/srv/svn"
svn_url = "svn://svn.r-forge.r-project.org/svnroot/"

## GForge plugin RPlugin -> target DB table
table = "plugin_rforge_package_db"





## helper functions

## safe SQL parameter quoting
qq <- function(x) {
		x[is.na(x)]="" # we don't want literal 'NA's in the db
		paste(paste("'",gsub("'","''",x),"'",sep=""), collapse=", ")
}

## Convert local time to UTC
getUtcTime <- function(time) {
		# as.POSIXct is the only function that properly handles ISO8601 dates with timezones 
		# as.POSIXlt, strptime, etc. are useless here
		format(as.POSIXct(time), tz="GMT") 
}

## send out email in case of naming conflict
conflict_error <- function(pkg1, pkg2, rep, email) {
	msg = sprintf("Error: Package %s was found in %s, 
		but a package of the same name was found in %s.
		Only the one in %s will be built.", pkg1, rep, pkg2, pkg2)
	email = gsub("'", "'\\''", email); # shell escape
	system(sprintf('echo "%s" | mail -s "R-Forge: Package name conflict" -c r-forge@r-project.org \'%s\'', msg, email))
	cat("Inserting ", pkg1, " failed!\n")
	return(1)
}

## get revison and time of that revision from svn 
getRevTime <- function(path) {
	err = tryCatch(
	{
		cc = pipe(open="r", sprintf("svn info \"%s\"", path))
		x = read.dcf(cc)
		ret = list(rev=as.integer(x[1,'Revision']), 
			time=as.POSIXct(x[1,'Last Changed Date']))
		close(cc)
		FALSE
	}, error=function(e) TRUE)
	if (err) return(NA)
	else return(ret)
}

## Is the date in DESCRIPTION valid?
## The DB 
sanitizeDate <- function(date) {
	posix_date = tryCatch(as.POSIXlt(date), error=function(e) NA)
	if (is.na(posix_date) || posix_date[['year']] < 0) return("")
	return(as.character(posix_date))
}
	







## doesn't work, but let's pretend it does...
#drv <- dbDriver("PostgreSQL")
#con <- dbConnect(drv, dbname="gforge", user="plugin_rforge", password="jenslf0r2");
## dummies for SQl testing. Remove for production use



con = 1
dbSendQuery <- function(con, sql) {cat("SQL:\n", sql, "\n") }


cat("--------------------------\n- Exportation of packages\n- ", date(), "\n\n");


if (!file.exists(pkg_dir)) create.dir(pkg_dir, recursive=TRUE)

setwd(pkg_dir)
system("rm -rf *");

pkgs = character()

dbSendQuery(con, "BEGIN;")
dbSendQuery(con, paste("DELETE FROM", table, ";"));

packages_db = read.dcf(url("http://cran.at.r-project.org/src/contrib/PACKAGES"))

svnreps = list.files(svn_dir)

for (rep in svnreps) {
	cat(":", rep, ":\n")
	
	if (file.exists(paste(svn_dir, "/", rep, "/format", sep=""))) {
		## get all DESCRIPTIONs per project
		descs = grep("^pkg/([^/]+/)?DESCRIPTION$", 
			system(sprintf("svnlook tree --full-paths %s/%s", svn_dir, rep), intern=TRUE), 
			value=TRUE, perl=TRUE)

                ## encapsulate redundant parts (external repository).
		for (desc in descs) {
			## export the DESCRIPTION
			descname = paste(rep, ".", gsub("/", ".", desc), sep="")
			system(sprintf("svn export file://%s/%s/%s %s", 
				svn_dir, rep, desc, descname))
			vars = tryCatch(tools:::.read_description(descname), error=function(e) "ERROR")
			if (vars[1]=="ERROR") {warning("Improper Desc Format");next}
			### ^^^ Test this!
			
			## do checks, collect info
			pkg_name = vars['Package']
			if (! is.na(pkgs[pkg_name])) {
				conflict_error(pkg_name, pkgs[pkg_name], rep, vars['Maintainer'])
				next
			}
			rt = getRevTime(file.path(svn_url, rep))
			if (is.na(rt)) next
			
			rev = rt[[1]]; time = rt[[2]]
			check_errors <- tryCatch(tools:::.check_package_description(descname), error=function(e) "ERROR")
			if(length(check_errors) > 0) { cat("Improper Desc Format ", desc);next}
			cran = packages_db[which(packages_db[,'Package']==pkg_name),'Version'] # version on cran
			
			## build SQL and run it
			insert = paste("INSERT INTO", table, "(pkg_name, unix_group_name, version, title, description, author, license, pkg_date, last_change, rev, maintainer, cran_release) VALUES (", 
				qq(c(pkg_name, rep, vars['Version'], vars['Title'],
				vars['Description'], vars['Author'], vars['License'], sanitizeDate(vars['Date']), time, rev, vars['Maintainer'], cran)), 
				");")
			cat("Run SQL: ", insert, "\n")
			dbSendQuery(con, insert)
			
			## Export the complete package from SVN
			descdir = dirname(desc)
			system(sprintf("svn export file://%s/%s/%s %s",
				svn_dir, rep, descdir, pkg_name))
			
			## Patch DESCRIPTION with custom fields
			vars['Repository'] = 'R-Forge'
			vars['Repository/R-Forge/Project'] = rep
			vars['Repository/R-Forge/Revision'] = rev
			vars['Publication/Date'] = getUtcTime(time)
			write.dcf(matrix(vars, 1, length(vars), dimnames=list(NULL, names(vars))), file=file.path(pkg_name, "DESCRIPTION"))
		}
	}
}

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

