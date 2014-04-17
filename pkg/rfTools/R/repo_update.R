rf_update_repo <- function( rfc, release_dir ){
  ## moved from release function
  stmp <- .rf_get_tmp(rfc)
  stopifnot( file.exists(stmp) )

  ## further configuration
  file_types <- c(Linux = ".tar.gz", MacOSX = ".tgz", Windows = ".zip")
  pkg_types <- c(Linux = "source", MacOSX = "mac.binary", Windows = "win.binary")

  fields <- c(tools:::.get_standard_repository_db_fields(), "Repository", "Repository/Project", "Repository/R-Forge/Revision", "Repository/R-Forge/DateTimeStamp")

  platform <- "Linux"
  file_type <- file_types[platform]
  pkg_type <- pkg_types[platform]
  write_PACKAGES(dir = contrib.url(release_dir), fields = fields, type = pkg_type)

  platform <- "Windows"
  file_type <- file_types[platform]
  pkg_type <- pkg_types[platform]
  write_PACKAGES(dir = contrib.url(release_dir, pkg_type), fields = fields, type = pkg_type)

  invisible(TRUE)
}
