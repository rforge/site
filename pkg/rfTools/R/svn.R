## this file includes code for handling svn repositories
## 

rf_get_svn_repos <- function( rfc, all = FALSE, verbose = FALSE )
  .get_active_reps_from_root( rfc, verbose = verbose, all = all )

rf_get_descriptions_from_svn <- function( rfc, svn_reps ){
  path_to_description <- lapply( rownames(svn_reps),
                          function(x) .find_description_in_svn(
                                         file.path(.rf_get_svn_root(rfc), x)) )
  names( path_to_description ) <- rownames( svn_reps )

  structure( lapply( rownames(svn_reps),
                    function(x) lapply(path_to_description[[ x ]],
                                       function(desc, rep){
                                         repo_path <- file.path(.rf_get_svn_root(rfc), rep)
                                         out <- .svn_cat_description(repo_path, desc)
                                         if( !inherits(out, "error") )
                                           attr(out, "meta") <- .svn_info(file.path(repo_path, dirname(desc)))
                                         out
                                       }
                                       , x)
                    ),
            names = rownames(svn_reps) )
}


## lower level SVN functions
################################################################################

.svn_export <- function( repo, path ){
  system( sprintf("svn export %s %s", repo, path) )
}

.svn_info <- function( path ){
  #if( file.access(file.path(path, "format"), mode = 4) != 0 )
   # return( .svn_info_NA(path) )
  con  <- pipe( open = "r", sprintf("svn info \"file://%s\"", path) )
  out <- tryCatch( read.dcf( con ), error = identity )
  close(con)
  if(inherits( out, "error"))
    return( .svn_info_NA(path) )
  structure( as.character(out), names =  dimnames(out)[[2]])
}

.svn_info_NA <- function( path )
  c( Path = basename(path), URL = sprintf("file://%s", path),  "Repository Root" = sprintf("file://%s", path),  "Repository UUID" = NA, Revision = NA, "Node Kind" = NA, "Last Changed Author" = NA, "Last Changed Rev" = NA, "Last Changed Date" = NA)


.svn_cat_description <- function( repos, file ){
  con  <- pipe( open = "r", sprintf("svnlook cat %s %s", repos, file) )
  out <- tryCatch( read.dcf( con, keep.white = tools:::.keep_white_description_fields)[1L, ], error = identity )
  close(con)
  out
}

.svn_history <- function( repo, path ){
  con  <- pipe( open = "r", sprintf("svnlook history \"%s\" \"%s\"", repo, path) )
  out <- tryCatch( read.table( con, header = FALSE, skip = 2, stringsAsFactors = FALSE, sep = "" ), error = function(x) data.frame(NA, NA) )
  close(con)
  colnames(out) <- c("REVISION", "PATH")
  out
}

.svn_tree <- function( path )
  system( sprintf("svnlook tree --full-paths %s", path), intern = TRUE )

## higher level SVN functions
################################################################################

## get head revison and its time stamp from svn
.svn_get_revsion_and_timestamp <- function( path ) {
  info <- .svn_info(path)
  data.frame( rev  = as.integer(info[ "Revision" ]),
              time = as.POSIXct(info[ "Last Changed Date" ]) )
}

## for external
.svn_get_revision_and_timestamp2 <- function( path ){
  con  <- pipe( open = "r", sprintf("svn --xml info \"file://%s\"", path) )
  out <- tryCatch( readLines( con ), error = identity )
  close(con)
  tree <- XML::xmlInternalTreeParse(out)
  date <- sapply(XML::getNodeSet(tree, "/info/entry/commit/date"), XML::xmlValue)
  rev <- sapply(XML::getNodeSet(tree, "/info/entry/commit"), XML::xmlAttrs)
  XML::free(tree)
  list(date = date, rev = rev)
}	


## given the svn root directory in 'rfc' returns a list of "active"
## repositories contained in that directory
.get_active_reps_from_root <- function( rfc, verbose = FALSE, all = TRUE ){
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
    if( all )
      meta
    else
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
