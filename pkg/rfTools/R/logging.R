get_buildlog <- function(path_to_pkg_log, pkg, platform, architecture){
  paste(file.path(path_to_pkg_log, pkg), "-", platform,
        "-", architecture, "-buildlog.txt", sep = "")
}

write_prolog <- function(pkg, file, pkg_db, type = c("build", "check"), what = c("tarball", "binary"), std.out = FALSE){
  type <- match.arg(type)
  what <- match.arg(what)
  pkg_revision <- attr(pkg_db$outdated[[pkg]]$description, "meta")["Last Changed Rev"]
  ## R CMD build message
  if(type == "build")
    msg <- paste(date(), ": Building ", what, " for package ", pkg, " (SVN revision ", pkg_revision,
                 ")\n", sep = "")
  ## R CMD check message
  if(type == "check")
    msg <- paste(date(), ": Checking package ", pkg, " (SVN revision ", pkg_revision, ") ...\n", sep = "")
  cat(msg, file = file)
  ## additional information about R version not provided by R CMD build
  if(type == "build")
    cat(paste("using", R.Version()$version.string, "...\n\n"), file = file, append = TRUE)
  if(std.out)
    cat(msg)
}

write_epilog <- function(file, timing, std.out = FALSE){
  cat(paste("Run time:", round(timing, 2L), "seconds.\n"), file = file, append = TRUE)
  if(std.out)
    writeLines(paste("Done in", round(timing, 2L), "seconds."))
}

write_stoplist_notification <- function(pkg, file, type = c("build", "check"), std.out = FALSE){
  type <- match.arg(type)
  ## R CMD build message
  if(type == "build")
    msg <- paste(date(), ": Package ", pkg, " is currently on the stop list ...\n", sep = "")
  ## R CMD check message
  if(type == "check")
    msg <- paste(date(), ": Package ", pkg, " is currently on the stop list ...\n", sep = "")
  cat(msg, file = file)
  ## additional information about R version not provided by R CMD build
  if(type == "build")
    cat(paste("using", R.Version()$version.string, "...\n\n"), file = file, append = TRUE)
  if(std.out)
    cat(msg)
}
