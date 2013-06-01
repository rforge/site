## Prepare package sources for build
##############################

## Three steps are needed for preparing package sources hosted in SVN for build
## (1) search SVN repositories for new or updated packages and retrieve some meta info
## (2) update package db with retrieved info and flag as 'scheduled for build'
## (3) export uncompressed sources from SVN and flag packages as 'building' in SVN

## (1) make pkg status info based on SVN; retrieves further info from pkg db and CRAN
rf_pkg_status <- function( rfc, rebuild = FALSE, verbose = FALSE ){
  stopifnot( file.exists(.rf_get_svn_root(rfc)) )
  if( length(dir(.rf_get_svn_root(rfc))) <= 0 )
    stop( sprintf("svn root '%s' directory empty", .rf_get_svn_root(rfc)) )

  ## directories in svn_root are potential repositories
  ## FIXME: cache this! only needed once a day?
  svn_reps <-  rf_get_svn_repos( rfc, verbose = verbose)

  ## retrieve all DESCRIPTION files from the repositories
  if( verbose )
      writeLines( "- Retrieve DESCRIPTIONs ...")
  descriptions <- rf_get_descriptions_from_svn( rfc, svn_reps )

  ## check all DESCRIPTION files for sanity
  test <- unlist( lapply(names(descriptions), function(repo) lapply( descriptions[[repo]], function(desc){

      if(inherits(desc, "error")) {
          ## we know that reading desc file failed
          status <- desc
      } else {
          ## FIXME: handle encoding outside of
          ## .check_description_for_sanity? Thanks to Sebastian Meyer
          ## for this hint (https://r-forge.r-project.org/tracker/?func=detail&atid=371&aid=2792&group_id=34).
          desc <- if( !is.na(encoding <- desc["Encoding"]) ) {
              if (encoding %in% c("latin1", "UTF-8")){
                  Encoding(desc) <- encoding
                  desc
              } else iconv(desc, from = encoding, to = "", sub = "byte")
          } else desc
          status <- .check_description_for_sanity(desc)
      }
      ## Authors@R handling
      fields <- tryCatch( tools:::.expand_package_description_db_R_fields(desc), error = identity )
      if( inherits(fields, "error") )
          status <- fields
      else
          if (length(fields) ){
              meta <- attr(desc, "meta")
              desc <- c(desc, fields)
              attr(desc, "meta") <- meta
          }
      list(description = desc,
           sanity_check = list(status = length(status) == 0,
           msg = status),
           repo = repo)
  })), recursive = FALSE )

  ## keep track of final results
  rf_pkg_status <- list()

  ## get CRAN package info
  ## FIXME: do we need a timeout for available.packages?
  cran_cache <- file.path( .rf_get_tmp(rfc), "cran_cache.rda" )
  if( !file.exists(cran_cache) ){
    pkgs_cran_all <- available.packages(contrib.url("http://cran.wu.ac.at"), filters = "duplicates")
    save(pkgs_cran_all, file = cran_cache)
  }
  ## only refresh cache every 6h
  refresh_time <- Sys.time() - 3600*6
  if( file.info(cran_cache)[["ctime"]] < refresh_time ){
    pkgs_cran_all <- available.packages(contrib.url("http://cran.wu.ac.at"), filters = "duplicates")
    save(pkgs_cran_all, file = cran_cache)
  }
  load( cran_cache )

  rf_pkg_status$cran <- pkgs_cran_all

  ## Malformed DESCRIPTION files (status code 3 - failed to build - later on)
  rf_pkg_status$malformed_description <- test[!unlist( lapply(test, function(x) x$sanity_check$status) )]

  ## only take those package which pass sanity checks
  pkgs_svn <- test[unlist( lapply(test, function(x) x$sanity_check$status) )]
  pkg_names <- unlist(lapply(pkgs_svn, function(x) x$description["Package"]))

  ## Conflicts: duplicate package names
  pkg_conflicts <- unique(pkg_names[duplicated(pkg_names)])
  which_conflicts <- which(pkg_names %in% pkg_conflicts)
  rf_pkg_status$conflicts <- pkgs_svn[ which_conflicts ]

  ## now proceed with unique package names
  pkgs_svn <- pkgs_svn[ -which_conflicts ]
  names(pkgs_svn) <- pkg_names[ -which_conflicts ]

  ## Retrieve current package status from db
  pkg_rforge <- rf_show_pkgs( rfc )

  ## obsolete packages which have to be removed from DB
  rf_pkg_status$obsolete <- rownames(pkg_rforge)[!rownames(pkg_rforge) %in% names(pkgs_svn)]

  ## current packages (no action required later on)
  test2 <- unlist(lapply(rownames(pkg_rforge), function(pkg){
                  x <- pkg_rforge[pkg, "rev"] == as.integer(attr(pkgs_svn[[pkg]]$description, "meta")["Last Changed Rev"])
                  if( length(x) )
                    return(x)
                  NA
                }))
  names(test2) <- rownames(pkg_rforge)

  pkgs_current <- na.omit(rownames(pkg_rforge)[ test2 ])

  ## rebuild all packages marked as 5L: offline?
  if( rebuild ){
    pkgs_current <- pkgs_current[ pkg_rforge[pkgs_current, "status" ] != 5L ]
  }

  rf_pkg_status$current <- pkgs_svn[ pkgs_current ]

  ## outdated or new packages (packages for staging area)
  rf_pkg_status$outdated <- pkgs_svn[ names(pkgs_svn)[!names(pkgs_svn)%in% pkgs_current] ]

  ## save rforge DB status
  rf_pkg_status$db <- pkg_rforge

  structure(rf_pkg_status, class = "rf_pkg_status")
}

print.rf_pkg_status <- function( x, ... ){
  writeLines("R-Forge build status")
  writeLines(sprintf("- Packages up-to-date: %s", length(x$current)))
  writeLines("- Outdated or new package(s):")
  print(names(x$outdated))
  if(length(x$malformed_description)){
    writeLines("- Repositories with packages containing malformed DESCRIPTION files:")
    print(unique(unlist(lapply(x$malformed_description, function(pkg) pkg$repo))))
  }
  if(length(x$conflicts)){
    writeLines("- Package name conflicts:")
    print(unlist(lapply(x$conflicts, function(pkg) sprintf("%s: %s", pkg$repo, pkg$description["Package"]))))
  }
  if(length(x$obsolete)){
    writeLines("- Obsolete packages:")
    print(x$obsolete)
  }
}

## (2) update pkg db with new info and flag as 'scheduled for build'
rf_prepare_build <- function(rfc, rf_pkg_status){
  ## packages which are not yet listed in DB
  brand_new <- names(rf_pkg_status$outdated)[!names(rf_pkg_status$outdated) %in% rownames(rf_pkg_status$db)]
  ## packages listed but not current
  outdated <- setdiff(names(rf_pkg_status$outdated), brand_new)
  ## packages which are marked as 'Current'
  current <- names(rf_pkg_status$current)
  ## update the CRAN version for those only
  if( length(current) ){
    updated <- rf_update_cran_info( rfc, rf_pkg_status, current )
    if(length(updated)){
      writeLines("CRAN version info update for:")
      writeLines(paste(updated, collapse = ", "))
    }
  }
  ## outdated pkgs which are not scheduled for build or building
  build_states <- c(0, 3, 4, 5)
  status <- rf_pkg_status$db[outdated, "status"]
  outdated <- outdated[status %in% build_states]
  ## character vector of package names to be built
  tobuild <- c(outdated, brand_new)
  ## add brand new packages to DB
  if( length(brand_new) ){
    rf_insert_new_pkg( rfc, rf_pkg_status, brand_new )
    writeLines("New packages on R-Forge:")
    writeLines(paste(brand_new, collapse = ", "))
  }
  ## update existing package rows and change build status in DB
  if( length(outdated) ){
    rf_update_outdated_pkg( rfc, rf_pkg_status, outdated )
    writeLines("Packages scheduled for build:")
    writeLines(paste(outdated, collapse = ", "))
  }
  tobuild
}

## (2a) update pkg db for rebuilding packages (mark as 'scheduled for build')
## rf_prepare_rebuild <- function(rfc, rf_pkg_status, offline_only = FALSE){
##   ## for rebuild we consider only packages which are marked as 'Current'
##   current <- names(rf_pkg_status$current)
##   ## update the CRAN version for those only
##   if( length(current) ){
##     updated <- rf_update_cran_info( rfc, rf_pkg_status, current )
##     if(length(updated)){
##       writeLines("CRAN version info update for:")
##       writeLines(paste(updated, collapse = ", "))
##     }
##   }
##   ## rebuild (offline) pkgs which are not scheduled for build or building
##   build_states <- if(offline_only){
##     c(5)
##   } else {
##     c(0, 3, 4)
##   }
##   status <- rf_pkg_status$db[current, "status"]
##   tobuild <- current[status %in% build_states]
##   ## add brand new packages to DB
##   ## update existing package rows and change build status in DB
##   if( length(tobuild) ){
##     rf_update_outdated_pkg( rfc, rf_pkg_status, tobuild )
##     writeLines("Packages scheduled for build:")
##     writeLines(paste(current, collapse = ", "))
##   }
##   tobuild
## }

## (3) exports package sources and make them available for build
rf_export_and_build_pkgs <- function(rfc, rf_pkg_status, pkgs){ #, rebuild = FALSE){
#  if(rebuild){
#    rf_pkg_status$outdated <- rf_pkg_status$current[pkgs]
#  }
  stmp <- .make_new_staging_area( rfc )
  status <- lapply( pkgs, function(pkg){
    desc <- rf_pkg_status$outdated[[pkg]]$description
    dest <- file.path( stmp, desc["Package"] )
    out <- .svn_export(attr(desc, "meta")["URL"], dest)
    desc["Repository"] <- "R-Forge"
    desc["Repository/R-Forge/Project"] <- rf_pkg_status$outdated[[pkg]]$repo
    desc["Repository/R-Forge/Revision"] <- attr(desc, "meta")["Last Changed Rev"]
    desc["Repository/R-Forge/DateTimeStamp"] <- format(as.POSIXct(attr(desc, "meta")["Last Changed Date"]), tz = "GMT")
    desc["Date/Publication"] <- desc["Repository/R-Forge/DateTimeStamp"]
    tools:::.write_description( desc, file.path(dest, "DESCRIPTION") )
  } )
  tools::write_PACKAGES( stmp, type = "source", unpacked = TRUE )
  ## as additional debug info save pkg status object
  save( rf_pkg_status, file = file.path(stmp, "PKG_STAT.rda") )
  TAR <- Sys.getenv("TAR")
  WINDOWS <- .Platform$OS.type == "windows"
  if (!nzchar(TAR)) {
    TAR <- if (WINDOWS)
      "tar --force-local"
    else "internal"
  }
  res <- utils::tar( file.path(.rf_get_tmp(rfc), paste(basename(stmp), "tar.gz", sep = ".")),
                     basename(stmp), compression = "gzip", compression_level = 9, tar = TAR,
                     extra_flags = sprintf("-C %s", dirname(stmp)) )
  if (res) {
    stop("packaging staging area into .tar.gz failed.")
  }
  lapply( pkgs, function(pkg) rf_set_pkg_status(rfc, pkg, status = 2L) )
  unlink( stmp, recursive = TRUE )
}

rf_update_outdated_pkg <- function( rfc, rf_pkg_status, pkgs){
  tab <- .rf_get_base_table(rfc)
  ## status set to 1L: scheduled for build
  status <- 1L

  sql <- lapply( rf_pkg_status$outdated[pkgs], function(pkg){
    desc <- pkg$description
    repo <- pkg$repo
    cran <- ""
    if( !is.null(rf_pkg_status$cran) ){
      if(desc["Package"] %in% rownames(rf_pkg_status$cran))
        cran <- rf_pkg_status$cran[desc["Package"], "Version"]
    }
    # FIXME: quotes.
    .make_SQL_update_outdated(tab, repo, desc["Package"], desc["Version"], desc["Title"], desc["Description"], desc["Author"], desc["License"], desc["Date"], substr(attr(desc, "meta")["Last Changed Date"], 1, 25), as.integer(attr(desc, "meta")["Last Changed Rev"]), desc["Maintainer"], cran, status)
  } )
  rfc <- rf_connect( rfc )
  lapply(sql, function(x) DBI::dbSendQuery(rf_get_db_con(rfc), x) )
  rfc <- rf_disconnect( rfc )
}

rf_remove_obsolete_pkg <- function( rfc, rf_pkg_status ){
  obsolete <- rf_pkg_status$obsolete
  stopifnot( is.character(obsolete) )
  if( length(obsolete) ){
    ind <- which(obsolete %in%  rownames(rfTools:::rf_show_pkgs( rfc )))
    if( length(ind) )
      rf_delete_pkg( rfc, obsolete[ind] )
    else
      warning("No packages available for removal. (Already removed?)")
  }
  invisible(TRUE)
}

rf_update_cran_info <- function( rfc, rf_pkg_status, pkgs){
  tab <- .rf_get_base_table(rfc)

  sql <- unlist( lapply( rf_pkg_status$current[pkgs], function(pkg){
    desc <- pkg$description
    pkgname <- desc["Package"]
    cran <- ""
    if( !is.null(rf_pkg_status$cran) ){
      if(pkgname %in% rownames(rf_pkg_status$cran)){
        cran <- rf_pkg_status$cran[pkgname, "Version"]
        cran_db <- rf_pkg_status$db[pkgname, "cran_release"]
        if(cran <= cran_db)
          cran <- ""
      }
    }
    if(nzchar(cran))
      .make_SQL_update_cran_info( tab, pkgname, cran )
    else
      NULL
  } ) )
  if(length(sql)){
    rfc <- rf_connect( rfc )
    lapply(sql, function(x) DBI::dbSendQuery(rf_get_db_con(rfc), x) )
    rfc <- rf_disconnect( rfc )
  }
  names(sql)
}

rf_insert_new_pkg <- function( rfc, rf_pkg_status, pkgs){
  tab <- .rf_get_base_table(rfc)
  ## status set to 1L: scheduled for build
  status <- 1L

  sql <- lapply( rf_pkg_status$outdated[pkgs], function(pkg){
    desc <- pkg$description
    repo <- pkg$repo
    cran <- ""
    if( !is.null(rf_pkg_status$cran) ){
      if(desc["Package"] %in% rownames(rf_pkg_status$cran))
        cran <- rf_pkg_status$cran[desc["Package"], "Version"]
    }
    # FIXME: quotes.
    .make_SQL_insert_new(tab, repo, desc["Package"], desc["Version"], desc["Title"], desc["Description"], desc["Author"], desc["License"], desc["Date"], substr(attr(desc, "meta")["Last Changed Date"], 1, 25), as.integer(attr(desc, "meta")["Last Changed Rev"]), desc["Maintainer"], cran, status)
  } )
  rfc <- rf_connect( rfc )
  lapply(sql, function(x) DBI::dbSendQuery(rf_get_db_con(rfc), x) )
  rfc <- rf_disconnect( rfc )
}

rf_delete_pkg <- function(rfc, pkg){
  sql <- lapply( pkg, function(pkg){
    .make_SQL_remove_pkg(.rf_get_base_table(rfc), pkg)
  } )
  rfc <- rfTools:::rf_connect( rfc )
  lapply(sql, function(x) DBI::dbSendQuery(rfTools:::rf_get_db_con(rfc), x) )
  rfc <- rfTools:::rf_disconnect( rfc )

}

.make_SQL_remove_pkg <- function(table, pkg_name){
  sprintf("DELETE FROM %s WHERE pkg_name = '%s';", table, pkg_name)
}

.make_SQL_insert_new <- function(table, unix_group_name, pkg_name, version, title, description, author, license, pkg_date, last_change, rev, maintainer, cran_release, status){
  ## FIXME: Currently we drop ' as gsub does not work for \' ...
  author <- gsub("\'","", author)
  maintainer <- gsub("\'","", maintainer)
  title <- gsub("\'","", title)
  description <- gsub("\'","", description)
  pkg_date <- tryCatch( as.Date(pkg_date, format = "%y-%m-%d"), error = function(x) NA )
  if( is.na(pkg_date) ){
    sql <- sprintf("INSERT INTO %s (pkg_name, unix_group_name, version, title, description, author, license, last_change, rev, maintainer, cran_release, status) VALUES ('%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%d')", table, pkg_name, unix_group_name, version, gsub("\'","",title), gsub("\'","",description), author, license, last_change, rev, maintainer, cran_release, status )
  } else {
    sql <- sprintf("INSERT INTO %s (pkg_name, unix_group_name, version, title, description, author, license, pkg_date, last_change, rev, maintainer, cran_release, status) VALUES ('%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%d')", table, pkg_name, unix_group_name, version, gsub("\'","",title), gsub("\'","",description), author, license, pkg_date, last_change, rev, maintainer, cran_release, status )
  }
  sql
}

.make_SQL_update_outdated <- function(table, unix_group_name, pkg_name, version, title, description, author, license, pkg_date, last_change, rev, maintainer, cran_release, status){
  ## FIXME: Currently we drop ' as gsub does not work for \' ...
  author <- gsub("\'","", author)
  maintainer <- gsub("\'","", maintainer)
  title <- gsub("\'","", title)
  description <- gsub("\'","", description)
  pkg_date <- tryCatch( as.Date(pkg_date, format = "%y-%m-%d"), error = function(x) NA )
  if( is.na(pkg_date) ){
    sql <- sprintf("UPDATE %s SET (unix_group_name, version, title, description, author, license, last_change, rev, maintainer, cran_release, status) = ('%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%d') WHERE pkg_name = '%s'", table, unix_group_name, version, title, description, author, license, last_change, rev, maintainer, cran_release, status, pkg_name )
  } else {
    sql <- sprintf("UPDATE %s SET (unix_group_name, version, title, description, author, license, pkg_date, last_change, rev, maintainer, cran_release, status) = ('%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%d') WHERE pkg_name = '%s'", table, unix_group_name, version, title, description, author, license, pkg_date, last_change, rev, maintainer, cran_release, status, pkg_name )
  }
  sql
}

.make_SQL_update_cran_info<- function( table, pkg_name, cran_release ){
  sprintf("UPDATE %s SET (cran_release) = ('%s') WHERE pkg_name = '%s'", table, cran_release, pkg_name )
}

.make_new_staging_area <- function(rfc){
  ## we need a tempory directory here
  if ( !file.exists(.rf_get_tmp(rfc)) )
    dir.create( .rf_get_tmp(rfc), recursive=TRUE )
  stage_tmp <- file.path( .rf_get_tmp(rfc), sprintf("build_%s", format(Sys.time(), format = "%Y-%m-%d-%H-%M")) )
  if ( !file.exists(stage_tmp) )
    dir.create( stage_tmp )
  if( length(dir(stage_tmp)) )
    warning( sprintf("directory '%s' not empty", stage_tmp) )
  stage_tmp
}

## Package DB helper functions

rf_set_build_offline <- function(rfc){
  rfc <- rf_connect( rfc )
  DBI::dbGetQuery( rf_get_db_con(rfc), sprintf("UPDATE %s SET status = '5';", .rf_get_base_table(rfc)) )
  rfc <- rf_disconnect( rfc )
  invisible(rfc)
}

rf_set_pkg_status <- function(rfc, pkg, status = 5L){
  rfc <- rf_connect( rfc )
  DBI::dbGetQuery( rf_get_db_con(rfc), sprintf("UPDATE %s SET status = '%d' WHERE pkg_name = '%s';", .rf_get_base_table(rfc), status, pkg) )
  rfc <- rf_disconnect( rfc )
  invisible(rfc)
}


rf_show_pkgs <- function( rfc ){
  rfc <- rf_connect( rfc )
  pkgs_in_db <- .get_pkgs_from_table( rfc, .rf_get_base_table(rfc) )
  rfc <- rf_disconnect( rfc )
  out <- pkgs_in_db[, -1]
  out[, "rev"] <- as.integer(out[, "rev"])
  out[, "status"] <- as.integer(out[, "status"])
  rownames(out) <- pkgs_in_db[, "pkg_name"]
  out
}

.get_pkgs_from_table <- function( rfc, table ){
  DBI::dbGetQuery( rf_get_db_con(rfc),
              sprintf("SELECT pkg_name,version,cran_release,rev,status FROM %s", table) )
}

## Copied from package tools and modified to support vectorized descs:
## the first part is from tools:::.read_description
## FIXME: notified KH that an upstream change would be reasonable.
.check_description_for_sanity <- function( desc ){
  db <- if (!is.na(encoding <- desc["Encoding"])) {
    if (encoding %in% c("latin1", "UTF-8")){
      Encoding(desc) <- encoding
      desc
    } else iconv(desc, from = encoding, to = "", sub = "byte")
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
            db <- iconv(db, encoding, sub = "byte")
    }
    else if (!all(tools:::.is_ISO_8859(db))) {
        out$missing_encoding <- TRUE
    }
    if (any(is.na(nchar(db, "c", TRUE)))) {
        db <- iconv(db, "latin1")
    }
    if (!is.na(aar <- db["Authors@R"]) && (is.na(db["Author"]) ||
        is.na(db["Maintainer"]))) {
        res <- tools:::.check_package_description_authors_at_R_field(aar)
        if (is.na(db["Author"]) && !is.null(s <- attr(res, "Author")))
            db["Author"] <- s
        if (is.na(db["Maintainer"]) && !is.null(s <- attr(res,
            "Maintainer")))
            db["Maintainer"] <- s
        attributes(res) <- NULL
        out <- c(out, res)
    }
    required_fields <- c("Package", "Version", "License", "Description",
        "Title", "Author", "Maintainer")
    if (length(i <- which(is.na(match(required_fields, names(db))) |
        is.na(db[required_fields]))))
        out$missing_required_fields <- required_fields[i]
    val <- package_name <- db["Package"]
    if (!is.na(val)) {
        tmp <- character()
        if (!grepl(sprintf("^(R|%s)$", valid_package_name_regexp),
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
    if (!is.na(val <- db["Maintainer"]) && !grepl(tools:::.valid_maintainer_field_regexp,
        val))
        out$bad_maintainer <- val
    val <- db[match(c("Depends", "Suggests", "Imports", "Enhances"),
        names(db), nomatch = 0L)]
    if (length(val)) {
        depends <- tools:::.strip_whitespace(unlist(strsplit(val, ",")))
        bad_dep_entry <- bad_dep_op <- bad_dep_version <- character()
        dep_regexp <- paste("^[[:space:]]*", paste("(R|", valid_package_name_regexp,
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
                else if (grepl("^[[:space:]]*R", dep)) {
                  if (!grepl(sprintf("^(r[0-9]+|%s)$", valid_package_version_regexp),
                    sub(dep_regexp, "\\4", dep)))
                    bad_dep_version <- c(bad_dep_version, dep)
                }
                else if (!grepl(sprintf("^%s$", valid_package_version_regexp),
                  sub(dep_regexp, "\\4", dep)))
                  bad_dep_version <- c(bad_dep_version, dep)
            }
        }
        if (length(c(bad_dep_entry, bad_dep_op, bad_dep_version)))
            out$bad_depends_or_suggests_or_imports <- list(bad_dep_entry = bad_dep_entry,
                bad_dep_op = bad_dep_op, bad_dep_version = bad_dep_version)
    }
    if (!is.na(val <- db["Priority"]) && !is.na(package_name) &&
        (tolower(val) %in% c("base", "recommended", "defunct-base")) &&
        !(package_name %in% unlist(standard_package_names)))
        out$bad_priority <- val
    class(out) <- "check_package_description"
    out
}


