## rforge_prepare_pkgs
## Author: Martin Kober, Stefan Theussl
## The following functions search for DESCRIPTION under directory pkg of all R-Forge
## SVN repositories. It then exports all found DESCRIPTION files, builds up
## the package database and exports the package source codes.
## NOTE: the following functions work only with RdbiPgSQL from Bioconductor

## helper functions

## safe SQL parameter quoting
qq <- function(x) {
		x[is.na(x)]="" # we don't want literal 'NA's in the db
		paste(paste("'",gsub("'","''",x),"'",sep=""), collapse=", ")
}

## Convert local time to UTC
.UTC_time <- function(time) {
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
svn_get_publication_time_revision <- function(path) {
  err <- tryCatch({
                    cc <- pipe( open="r", sprintf("svn info \"%s\"", path) )
                    x <- read.dcf( cc )
                    ret <- list( Revision = as.integer(x[1,'Revision']), 
                      Time = as.POSIXct(x[1,'Last Changed Date']))
                    close(cc)
                    FALSE
                  }, error = identity)
  if(inherits(err, "error"))
    return(NA)
  ret
}

## Is the date in DESCRIPTION valid?
## The DB 
sanitizeDate <- function(date) {
	posix_date = tryCatch(as.POSIXlt(date), error=function(e) NA)
	if (is.na(posix_date) || posix_date[['year']] < 0) return("")
	return(as.character(posix_date))
}

get_available_packages_from_db <- function(con, table){
  out <- dbGetQuery(con, sprintf("SELECT pkg_name, version, license, rev, unix_group_name FROM %s;", table))
  out <- cbind(out, "R-Forge")
  colnames(out) <- c("Package", "Version", "License", "Repository/R-Forge/Revision", "Repository/R-Forge/Project", "Repository")
  rownames(out) <- out[["Package"]]
  as.matrix(out)
}

available_packages_in_svn <- function( path ){
  svn_reps <- list_svn_repositories( path )
  
  ## get all DESCRIPTION files per project (some of which have multiple packages)
  descs <- lapply(svn_reps, function(x) get_DESCRIPTION_from_svn(path, x))

  ## HERE I AM
  out <- lapply(descs, function(x) if(length(x[[1]])) svn_export_DESCRIPTION(path, names(x)[[1]], x[[1]]))
  
  
}


## TODO: CRAN VERSION APPEND
## load package db from CRAN
##, cran_url = "http://cran.wu.ac.at"
##  cran_pkg_db <- read.dcf(url(sprintf("%s/src/contrib/PACKAGES", cran_url)))
##    cran <- cran_pkg_db[ which(cran_pkg_db[, "Package"] == pkg_name),'Version'] # version on cran

svn_export_DESCRIPTION <- function( svn_path, repository, desc_path){
  path_to_DESCRIPTION <- file.path(svn_path, repository, desc_path)
  content <- system(sprintf("svn cat file://%s", path_to_DESCRIPTION), intern = TRUE)
  tcon <- textConnection(content, open = "r")
  vars <- tryCatch(read.dcf(file = tcon), error = identity)
  close.connection(tcon)

  if (inherits(vars, "error")) {
    warning("Improper DESCRIPTION format for %s", file.path(path_to_DESCRIPTION))
  } else {
    ## do checks, collect info
    pkg_name <- vars[, "Package"]
    publication_info <- svn_get_publication_time_revision(file.path("file://", svn_path, repository, sub("/DESCRIPTION", "", desc_path)))
    if (any(sapply(rt, is.na)))
        warning(sprintf("Could not get date, publication time from %s", path_to_DESCRIPTION))
        
    
    fields <- colnames(vars)
    out <- cbind(vars, "R-Forge", as.character(repository), as.character(publication_info[["Revision"]]), .UTC_time(publication_info[["Time"]]))
    colnames(out) <- c(fields, "Repository", "Repository/R-Forge/Project", "Repository/R-Forge/Revision", "Publication/Date")
  }
  out
}

build_SQL <- function(){
    ## build SQL and run it
    insert = paste("INSERT INTO", table, "(pkg_name, unix_group_name, version, title, description, author, license, pkg_date, last_change, rev, maintainer, cran_release) VALUES (", 
      qq(c(pkg_name, rep, vars['Version'], vars['Title'],
           vars['Description'], vars['Author'], vars['License'], sanitizeDate(vars['Date']), time, rev, vars['Maintainer'], cran)), 
      ");")
    cat("Run SQL: ", insert, "\n")
    dbSendQuery(con, insert)

}

svn_export <- function(){
    ## Export the complete package from SVN
    descdir = dirname(desc)
    system(sprintf("svn export file://%s/%s/%s %s",
                   svn_dir, rep, descdir, pkg_name))
    
    ## Patch DESCRIPTION with custom fields
    write.dcf(matrix(vars, 1, length(vars), dimnames=list(NULL, names(vars))), file=file.path(pkg_name, "DESCRIPTION"))
}


check_svn_package_list_sanity <- function(){
if (! is.na(pkgs[pkg_name])) {
      conflict_error(pkg_name, pkgs[pkg_name], rep, vars['Maintainer'])
      next
    }
## TODO: after export check all DESCRIPTIONs and remove packages with invalid DESCRIPTION
check_errors <- tryCatch(tools:::.check_package_description(descname), error=function(e) "ERROR")
if(length(check_errors) > 0) { cat("Improper Desc Format ", desc);next}

}

## NOTE: returns all accessible SVN repository under a given 'path'

list_svn_repositories <- function(path = "."){
  contents <- list.files(path)
  contents[sapply(contents, function(x) file.exists(file.path(path, x, "format")))]
}
  
get_DESCRIPTION_from_svn <- function(path = ".", repository){
  descs <- grep("^pkg/([^/]+/)?DESCRIPTION$", 
                system(sprintf("svnlook tree --full-paths %s/%s", path, repository), intern=TRUE), 
                value=TRUE, perl=TRUE)
  out <- list(descs)
  names(out) <- repository
  out
}

prepare_packages <- function(con, table, pkg_dir, svn_dir){
  if (!file.exists(pkg_dir))
    dir.create(pkg_dir, recursive=TRUE)

  current_pkgs <- get_available_packages_from_db(con, table)
  pkgs_to_export <- get_available_packages_from_svn(svn_dir)
    
  setwd(pkg_dir)
  unlink(dir(), recursive = TRUE)

  
}

