## this file includes code for handling svn repositories
## 

rf_get_svn_repos <- function( rfc )
  .get_active_reps_from_root( rfc )

rf_get_descriptions_from_svn <- function( rfc, svn_reps ){
  path_to_description <- lapply( rownames(svn_reps),
                          function(x) .find_description_in_svn(
                                         file.path(.rf_get_svn_root(rfc), x)) )
  names( path_to_description ) <- rownames( svn_reps )

  lapply( rownames(svn_reps),
          function(x) lapply(path_to_description[[ x ]],
                             function(desc, rep)
                             .svn_cat_description(file.path(
                               .rf_get_svn_root(rfc), rep), desc), x)
         )
}


## get head revison and its time stamp from svn
.svn_get_revsion_and_timestamp <- function( path ) {
  info <- tryCatch( .svn_info(path), error = identity)
  if( inherits(info, "error"))
    return( NA )
  data.frame( rev  = as.integer(info[ 1,'Revision' ]),
        time = as.POSIXct(info[ 1,'Last Changed Date' ]) )
}

.svn_info <- function( path ){
  con  <- pipe( open = "r", sprintf("svn info \"file://%s\"", path) )
  out <- tryCatch( read.dcf( con ), error = function(err) NA )
  close(con)
  out
}

.svn_cat_description <- function( repos, file ){
  con  <- pipe( open = "r", sprintf("svnlook cat %s %s", repos, file) )
  out <- tryCatch( read.dcf( con ), error = function(err) NA )
  close(con)
  out
}

.svn_tree <- function( path )
  system( sprintf("svnlook tree --full-paths %s", path), intern = TRUE )

## given the svn root directory in 'rfc' returns a list of "active"
## repositories contained in that directory
.get_active_reps_from_root <- function( rfc, verbose = FALSE ){
    svn_reps <- dir( .rf_get_svn_root(rfc) )
    ## take only those which are indeed repositories and are not hidden
    included <- sapply( svn_reps, function(x) file.exists(file.path( .rf_get_svn_root(rfc), x, "format")) )
    if( verbose )
        writeLines( sprintf("- Directory not a repository or not readable:\n-- %s.",
                            paste(svn_reps[!included], collapse = ", ")) )
    svn_reps <- svn_reps[ included ]
    ## check if meta data can be retrieved from repositories
    meta <- do.call( rbind, lapply( svn_reps, function(x)
                       .svn_get_revsion_and_timestamp(file.path(.rf_get_svn_root(rfc),x))) )
    rownames(meta) <- svn_reps
    ## check if we have no revision information in any of the packages and 
    na <- is.na( meta[,1] )
    if( verbose && any(na) )
        writeLines( sprintf("- Repository meta data cannot be retrieved:\n-- %s.",
                            paste(svn_reps[na], collapse = ", ")) )
    ## return active repositories
    meta[!na, ]
}

.find_description_in_svn <- function( path ){
   grep( "^pkg/([^/]+/)?DESCRIPTION$",
        .svn_tree( path ),
        value = TRUE, perl = TRUE )
}

## obsolete?
.svn_get_description <- function( svn_root, desc, rep, tmp_dir){
  descname <- sprintf( "%s.%s", rep, gsub("/", ".", desc) )
  system( sprintf("svn export file://%s %s",
                  file.path(svn_root, rep, desc), file.path(tmp_dir, descname)) )
}
