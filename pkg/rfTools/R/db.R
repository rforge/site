
##############################
## rf constructor/methods
##############################

.create_rf_config <- function( db_name, db_user, db_password, svn_root, tmp_dir, db_con = NULL ){
  structure( list(db_name     = db_name,
                  db_user     = db_user,
                  db_password = db_password,
                  svn_root    = svn_root,
                  tmp_dir     = tmp_dir,
                  db_con      = db_con), class = "rf" )
}

print.rf <- function( x, ... ){
  writeLines("R-Forge service configuration.")
}

rf_standard_fields <- function()
  c("db_name", "db_user", "db_password", "svn_root", "tmp_dir")

rf_write_configuration <- function( rfc, file, fields = NULL, ... ){
  fields <- unique(rf_standard_fields(), fields)
  conf <- matrix(sapply(fields, function(x) rfc[[x]]), nrow = 1)
  colnames(conf) <- fields
  write.dcf( conf, file = file, ... )
  writeLines( sprintf("Database configuration written to '%s'", file) )
}

rf_read_configuration <- function( file, fields = NULL, ... ){
  conf <- read.dcf( file   = file,
                    fields = unique(rf_standard_fields(), fields), ... )
  .create_rf_config( db_name     = conf[, "db_name"],
                     db_user     = conf[, "db_user"],
                     db_password = conf[, "db_password"],
                     svn_root    = conf[, "svn_root"],
                     tmp_dir     = conf[, "tmp_dir"]
                     )
}

##############################
## Connect/disconnect to rf DB
##############################

rf_connect <- function( rfc ){
  stopifnot( inherits(rfc, "rf") )
  rfc$db_con <- dbConnect( PgSQL(),
                           dbname   = rfc$db_name,
                           user     = rfc$db_user,
                           password = rfc$db_password )
  rfc
}

rf_disconnect <- function( rfc ){
  stopifnot( inherits(rfc, "rf") )
  dbDisconnect( rf_get_db_con(rfc) )
  rfc$db_con <- NULL
  rfc
}

rf_get_db_con <- function( rfc ){
  stopifnot( inherits(rfc, "rf") )
  rfc$db_con
}

##############################
## rf DB sanity check
##############################

.check_rf_for_sanity <- function( rfc ){
  if( !.check_available_entries(rfc) )
    stop( sprintf("necessary entries missing in config. All of '%s' are needed",
                 paste(rf_standard_fields(), collapse = ", ")) )
  rfc <- tryCatch( rf_connect( rfc ), error = identity )
  if( inherits(rfc, "error") )
    stop( sprintf("db connection error: %s", rfc$message) )
  rf_disconnect( rfc )
  invisible( TRUE )
}

.check_available_entries <- function( rfc )
  all( rf_standard_fields() %in% names(rfc) )
  
##############################
## update rf db
##############################
rf_update_db <- function( rfc, verbose = FALSE){
  ## we need a tempory directory here
  if ( !file.exists(rfc$tmp_dir) )
    dir.create(rfc$tmp_dir, recursive=TRUE)
  if( length(dir(rfc$tmp_dir)) )
    warning( sprintf("directory '%s' not empty", rfc$tmp_dir) )
  stopifnot( file.exists(rfc$svn_root) )
  if( length(dir(rfc$svn_root)) <= 0 )
    stop( sprintf("svn root '%s' directory empty", rfc$svn_root) )

  ## directories in svn_root are potential repositories
  svn_reps <- dir(rfc$svn_root)

  ## take only those which are indeed repositories and are not hidden
  included <- sapply( svn_reps, function(x) file.exists(file.path(rfc$svn_root, x, "format")) )
  if(verbose)
    writeLines( sprintf("- Repositories not included:\n-- %s.",
                        paste(svn_reps[!included], collapse = ", ")) )
  svn_reps <- svn_reps[ included ]

  ## check if meta data can be retrieved from repositories
  included <- sapply( svn_reps, function(x)
                     !is.na(.svn_get_revsion_and_timestamp(file.path(rfc$svn_root, x))) )
  svn_reps <- svn_reps[ included ]

  ## retrieve all description files from the repositories
  if(verbose)
    writeLines( "- Retrieve DESCRIPTIONs ...")
  descriptions <- lapply( svn_reps,
                          function(x) .find_description_in_svn( file.path(rfc$svn_root, x)) )
  names( descriptions ) <- svn_reps 
  lapply( svn_reps, function(x) lapply(descriptions[[ x ]], function(desc, rep)
                                       .svn_get_description(rfc$svn_root, desc, rep, rfc$tmp_dir), x) )

  ## verify integrity of descriptions
  ## HERE I AM! ##


  package_dirs <- lapply( descriptions, dirname )

  for (rep in svnreps) {
    if(verbose)
      writeLines( sprintf("- Processing repository '%s':", rep) )


                
    
  }
  rfc <- rfTools:::rf_connect( rfc )
  pkgs_in_db <- .get_pkgs_from_table( rfc, rfc$rf_table )
  rfc <- rfTools:::rf_disconnect( rfc )
  pkgs_in_db
}

## get head revison and its time stamp from svn 
.svn_get_revsion_and_timestamp <- function( path ) {
  info <- tryCatch( .svn_info(path), error = identity)
  if( inherits(info, "error"))
    return( NA )
  list( rev  = as.integer(info[ 1,'Revision' ]),
        time = as.POSIXct(info[ 1,'Last Changed Date' ]) )
}

.svn_info <- function( path ){
  con  <- pipe( open = "r", sprintf("svn info \"file://%s\"", path) )
  x    <- read.dcf( con )
  close(con)
  x
}

.svn_tree <- function( path )
  system( sprintf("svnlook tree --full-paths %s", path), intern = TRUE )

.find_description_in_svn <- function( path ){
   grep( "^pkg/([^/]+/)?DESCRIPTION$", 
        .svn_tree( path ), 
        value = TRUE, perl = TRUE )
}

.svn_get_description <- function( svn_root, desc, rep, tmp_dir){
  descname <- sprintf( "%s.%s", rep, gsub("/", ".", desc) )
  system( sprintf("svn export file://%s %s", 
                  file.path(svn_root, rep, desc), file.path(tmp_dir, descname)) )
}


rf_show_pkgs <- function( rfc ){
  rfc <- rfTools:::rf_connect( rfc )
  pkgs_in_db <- .get_pkgs_from_table( rfc, rfc$rf_table )
  rfc <- rfTools:::rf_disconnect( rfc )
  pkgs_in_db
}

.get_pkgs_from_table <- function( rfc, table )
  dbGetQuery( rf_get_db_con(rfc),
              sprintf("SELECT pkg_name,version,revision FROM %s", table) )


##############################
## rf DB insert schema
##############################

## Sample schema (old):

##        Table "public.plugin_rforge_package_db"
##     Column      |           Type           | Modifiers
##-----------------+--------------------------+-----------
## pkg_name        | character varying(255)   | not null
## unix_group_name | character varying(30)    | not null
## version         | character varying(30)    |
## title           | text                     |
## description     | text                     |
## author          | text                     |
## license         | text                     |
## pkg_date        | date                     |
## last_change     | timestamp with time zone |
## rev             | integer                  |
## maintainer      | text                     |
## external_url    | text                     |
## cran_release    | text                     |
##Indexes:
##    "plugin_rforge_package_db_pkey" PRIMARY KEY, btree (pkg_name)
##    "plugin_rforge_package_db_unixgn" btree (unix_group_name)
##

rf_db_insert_schema <- function( rfc, config_file ){
  .check_rf_for_sanity( rfc )
  rfc <- .db_add_base_table( rfc )
  .db_add_repository_table( rfc )
}

.db_add_base_table <- function( rfc, table = "rf_package_db" ){
  rfc <- rf_connect( rfc )
  sql <- sprintf('
  CREATE TABLE %s (
    pkg_name character varying(255) NOT NULL PRIMARY KEY,
    unix_group_name character varying(30) NOT NULL,
    revision integer,
    version character varying(30),
    last_change timestamp with time zone,
    title text,
    description text,
    maintainer text,
    author text,
    license text,
    pkg_date date,
    external_url text )', table)
  dbSendQuery( rf_get_db_con(rfc), sql )
  sql <- sprintf( 'CREATE INDEX %s_unixgn ON %s (unix_group_name)', table, table )
  dbSendQuery( rf_get_db_con(rfc), sql )
  rfc <- rf_disconnect( rfc )
  rfc$rf_table <- table
  rfc
}

## information about packages also hosted on other repositories (CRAN, bioc, etc.)
.db_add_repository_table <- function( rfc, table = "rf_repository_db" ){
  rfc <- rf_connect( rfc )
  sql <- sprintf('
  CREATE TABLE %s (
    pkg_name character varying(255) NOT NULL PRIMARY KEY,
    repository integer,
    version character varying(30),
    last_change timestamp with time zone )', table)
  dbSendQuery( rf_get_db_con(rfc), sql )
  rfc <- rf_disconnect( rfc )
  rfc$rf_table_repository <- table
  rfc
}

.db_drop_table <- function( rfc, table ){
  stopifnot( !is.null(table) )
  rfc <- rf_connect( rfc )
  dbSendQuery( rf_get_db_con(rfc), sprintf("DROP TABLE %s", as.character(table)) )
  rf_disconnect( rfc )
}

##############################
## Install rf DB on new system
##############################

## has to be run as user postgres or equivalent privileged user on the target system
rf_db_install <- function( con, config_file ){
  if( missing(config_file) )
    config_file <- Sys.getenv("_RF_CONFIG_")

  if( file.exists(config_file) ){
    rfc <- rf_read_configuration( file = config_file )
    if( inherits(tryCatch(.check_rf_for_sanity( rfc ), error = identity), "error") ){
      stopifnot( any(is.null(rfc$db_name), is.null(rfc$db_user), is.null(rfc$db_password)) )
      rfc <- .create_new_rf_db( con,
                               db_name = as.character(rfc$db_name),
                               db_user = as.character(rfc$db_user),
                               db_password = as.character(rfc$db_password) )
    }
  } else {
    stopifnot( file.create(config_file) )
    rfc <- .create_new_rf_db( con )
  }
  
  rf_write_configuration( rfc, file = config_file )
  invisible( TRUE )
}

.create_new_rf_db <- function( con,
                               db_name = "rf",
                               db_user = "rforge",
                               db_password = paste(sample(c(LETTERS, letters, 0:9), 10,
                                 replace = TRUE), collapse = "") ){
  if( any(unlist(dbGetQuery(con, "SELECT datname FROM pg_database")) %in% db_name) )
    warning( sprintf("database '%s' already exists. skipping creation...", db_name) )
  else
    dbSendQuery(con, sprintf("CREATE DATABASE %s", db_name))
  if( any(unlist(dbGetQuery(con, "SELECT usename FROM pg_user")) %in% db_user) )
    warning( sprintf("user '%s' already exists. skipping creation...", db_user) )
  else {
    dbSendQuery( con, sprintf("CREATE USER %s WITH PASSWORD '%s'", db_user, db_password) )
    dbSendQuery( con, sprintf("GRANT ALL PRIVILEGES ON DATABASE %s TO %s", db_name, db_user) )
    writeLines( sprintf("NOTE: database '%s' has been created and all privileges were granted to user '%s'.", db_name, db_user) )
  }
  writeLines( sprintf("NOTE: Database '%s' initialized.", db_name) )
  rfc <- .create_rf_config( db_name, db_user, db_password )
  .check_rf_for_sanity( rfc )
  rfc
}


## Use those functions with care
rf_db_remove <- function( con, rfc, force = FALSE ){
  invisible( .delete_rf_db(con, rfc, as.logical(force)) )
}

.delete_rf_db <- function( con, rfc, force = FALSE ){
  if( !force )
  stop( "disabled function. Too dangerous." )
  dbSendQuery( con, sprintf("REVOKE ALL PRIVILEGES ON DATABASE %s FROM %s",
                            rfc$db_name,
                            rfc$db_user) )
  dbSendQuery( con, sprintf("DROP DATABASE %s", rfc$db_name) )
  dbSendQuery( con, sprintf("DROP USER %s", rfc$db_user) )
  TRUE
}

