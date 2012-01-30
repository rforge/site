## update rf package db
##############################

## updates the pkg db shown in the R packages tab
rf_update_db <- function( rfc, verbose = FALSE){
  ## we need a tempory directory here
  if ( !file.exists(.rf_get_tmp(rfc)) )
    dir.create( .rf_get_tmp(rfc), recursive=TRUE )
  if( length(dir( .rf_get_tmp(rfc))) )
    warning( sprintf("directory '%s' not empty", .rf_get_tmp(rfc)) )
  stopifnot( file.exists(.rf_get_svn_root(rfc)) )
  if( length(dir(.rf_get_svn_root(rfc))) <= 0 )
    stop( sprintf("svn root '%s' directory empty", .rf_get_svn_root(rfc)) )

  ## directories in svn_root are potential repositories
  svn_reps <- .get_active_reps_from_root( rfc, verbose )

  ## retrieve all description files from the repositories
  if( verbose )
      writeLines( "- Retrieve DESCRIPTIONs ...")
  descriptions <- rf_get_descriptions_from_svn( rfc, svn_reps )
  .check_description_for_sanity( descriptions )
  
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


rf_show_pkgs <- function( rfc ){
  rfc <- rfTools:::rf_connect( rfc )
  pkgs_in_db <- .get_pkgs_from_table( rfc, .rf_get_base_table(rfc) )
  rfc <- rfTools:::rf_disconnect( rfc )
  pkgs_in_db
}

.get_pkgs_from_table <- function( rfc, table )
  dbGetQuery( rf_get_db_con(rfc),
              sprintf("SELECT pkg_name,version,revision FROM %s", table) )

## copied from package tools and modified to support vectorized descs:
.check_desctiption_for_sanity <- function( desc ){
  db <- if (!is.na(encoding <- desc["Encoding"])) {
    if (encoding %in% c("latin1", "UTF-8")){ 
      Encoding(desc) <- encoding
      desc
    } else iconv(desc, encoding, "", sub = "byte")
  } else desc
  
  standard_package_names <- tools:::.get_standard_package_names()
  valid_package_name_regexp <- .standard_regexps()$valid_package_name
  valid_package_version_regexp <- .standard_regexps()$valid_package_version
  is_base_package <- !is.na(priority <- db["Priority"]) && 
  priority == "base"
  out <- list()
  if (any(ind <- !tools:::.is_ASCII(names(db)))) 
    out$fields_with_non_ASCII_tags <- names(db)[ind]
  ASCII_fields <- c(tools:::.get_standard_repository_db_fields(), "Encoding", 
                    "License")
  ASCII_fields <- intersect(ASCII_fields, names(db))
  if (any(ind <- !tools:::.is_ASCII(db[ASCII_fields]))) 
    out$fields_with_non_ASCII_values <- ASCII_fields[ind]
  if ("Encoding" %in% names(db)) {
    encoding <- db["Encoding"]
    if ((!Sys.getlocale("LC_CTYPE") %in% c("C", "POSIX"))) 
      db <- iconv(db, encoding, "")
  }
  else if (!all(tools:::.is_ISO_8859(db))) {
    out$missing_encoding <- TRUE
  }
  if (any(is.na(nchar(db, "c", TRUE)))) {
    db <- iconv(db, "latin1", "")
  }
  required_fields <- c("Package", "Version", "License", "Description", 
                       "Title", "Author", "Maintainer")
  if (length(i <- which(is.na(match(required_fields, names(db))) | 
                        is.na(db[required_fields])))) 
    out$missing_required_fields <- required_fields[i]
  val <- package_name <- db["Package"]
  if (!is.na(val)) {
        tmp <- character()
        if (!grepl(sprintf("^%s$", valid_package_name_regexp), 
                   val) && !grepl("^Translation-[[:alnum:].]+$", val)) 
          tmp <- c(tmp, gettext("Malformed package name"))
        if (!is_base_package) {
          if (val %in% standard_package_names$base) 
            tmp <- c(tmp, c(gettext("Invalid package name."), 
                            gettext("This is the name of a base package.")))
            else if (val %in% standard_package_names$stubs) 
              tmp <- c(tmp, c(gettext("Invalid package name."), 
                              gettext("This name was used for a base package and is remapped by library().")))
        }
        if (length(tmp)) 
          out$bad_package <- tmp
      }
  if (!is.na(val <- db["Version"]) && !is_base_package && !grepl(sprintf("^%s$", 
                                                                         valid_package_version_regexp), val)) 
    out$bad_version <- val
  if (!is.na(val <- db["Maintainer"]) && !grepl(.valid_maintainer_field_regexp, 
                                                val)) 
    out$bad_maintainer <- val
  val <- db[match(c("Depends", "Suggests", "Imports", "Enhances"), 
                  names(db), nomatch = 0L)]
  if (length(val)) {
    depends <- .strip_whitespace(unlist(strsplit(val, ",")))
    bad_dep_entry <- bad_dep_op <- bad_dep_version <- character()
    dep_regexp <- paste("^[[:space:]]*", paste("(", valid_package_name_regexp, 
                                               ")", sep = ""), "([[:space:]]*\\(([^) ]+)[[:space:]]+([^) ]+)\\))?", 
                        "[[:space:]]*$", sep = "")
    for (dep in depends) {
          if (!grepl(dep_regexp, dep)) {
            bad_dep_entry <- c(bad_dep_entry, dep)
            next
          }
          if (nzchar(sub(dep_regexp, "\\2", dep))) {
            if (!sub(dep_regexp, "\\3", dep) %in% c("<=", 
                                                    ">=", "<", ">", "==", "!=")) 
              bad_dep_op <- c(bad_dep_op, dep)
            else if (!grepl(sprintf("^%s$", valid_package_version_regexp), 
                                sub(dep_regexp, "\\4", dep))) 
                  bad_dep_version <- c(bad_dep_version, dep)
          }
        }
    if (length(c(bad_dep_entry, bad_dep_op, bad_dep_version))) 
      out$bad_depends_or_suggests_or_imports <- list(bad_dep_entry = bad_dep_entry, 
                                                     bad_dep_op = bad_dep_op, bad_dep_version = bad_dep_version)
  }
  if (!is.na(val <- db["Namespace"]) && !is.na(package_name) && 
      (val != package_name)) 
    out$bad_namespace <- val
  if (!is.na(val <- db["Priority"]) && !is.na(package_name) && 
      (tolower(val) %in% c("base", "recommended", "defunct-base")) && 
      !(package_name %in% unlist(standard_package_names))) 
    out$bad_priority <- val
  class(out) <- "check_package_description"
  out
}

