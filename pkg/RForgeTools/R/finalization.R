provide_packages_in_contrib <- function(build_dir, contrib_dir, platform){

  file_types <- c(Linux = ".tar.gz", MacOSX = ".tgz", Windows = ".zip")
  file_type <- file_types[platform]
  pkg_types <- c(Linux = "source", MacOSX = "mac.binary", Windows = "win.binary")
  pkg_type <- pkg_types[platform]
  ## Hard coding fields (we are indexing by number, so hard-coding important here!)
  ## R-Forge repository offers additional "Revision" field.
  fields <- c(tools:::.get_standard_repository_db_fields(), "Repository", "Repository/Project", "Repository/R-Forge/Revision")
  ## Remember old working directory
  old_dir <- file_path_as_absolute(getwd())
  setwd(build_dir)

  ## first delete old packages
  system(sprintf("rm -f %s", file.path(contrib_dir, sprintf("*%s", file_type))))
  files <- dir()
  files <- files[grep(sprintf("%s$", file_type), files)]
  for(i in files){
    ## copy package to contrib directory
    file.copy(i, contrib_dir, overwrite = TRUE)
    ## delete package from build directory
    system(paste("rm -f", i))
  }
  ## remove old PACKAGES file
  setwd(contrib_dir)
  system(paste("rm -f PACKAGES*"))
  ## now find out which packages have old versions DEPRECATED! -> to be removed
  tmp <- dir()
  splitted <- strsplit(tmp, "_")
  packages <- sapply(splitted, "[", 1)
  ind <- 1L:length(packages)
  versno <- unlist(strsplit(sapply(splitted, "[", 2), file_type))
  duplicated_pkgs <- duplicated(packages)
  if(any(duplicated_pkgs)){
  ## look for duplicated packages and remove older version
    for(i in packages[duplicated_pkgs]){
      ind_package_to_remove <- ind[packages==i][version_order(versno[packages==i])[1]]
      system(paste("rm -f", tmp[ind_package_to_remove]))
    }
  }

  ## Write a new PACKAGES and PACKAGES.gz file
  write_PACKAGES(dir = contrib_dir, fields = fields, type = pkg_type)

  ## back to old directory
  setwd(old_dir)
  unique(packages)
}

notify_admins <- function(packages, donotcompile, email, platform, control, path_to_check_dir = NULL, timings = NULL, about = c("build", "check")){
  writeLines(paste("Preparing to send", about, "summary to", email, "..."))
  about <- match.arg(about)
  proc_time_minutes <- round(proc.time()["elapsed"]/60, 2L)
  attachment <- "Rnightlybuildmail.txt"
  donotcompiletxt <- paste(donotcompile, collapse = "\n")
  ## Do we inform about build or check results?
  if(about == "build"){
    pkg_txt <- paste("The binaries/sources of the following",
                     length(unique(packages)),
                     "packages are now available on R-Forge (Build time:", proc_time_minutes, "minutes):")
  } else{
    pkg_txt <- paste("The following",
                     length(unique(names(timings))),
                     "packages have been checked in", proc_time_minutes, "minutes:")
  }

  if(!is.null(timings)){
    ## max_cpu_time needed to mark packages taken too long
    ## FIXME: has to be considered in cleanup, e.g., automatically put pkg on the stop list
    max_cpu_time <- get_cpu_time_limit(control)
    sorted <- sort(timings, decreasing = TRUE)
    note <- rep("  ", length(sorted))
    too_long_symbol <- "!!"
    too_long <- which(sorted >= max_cpu_time)
    note[too_long] <- too_long_symbol
    timings_table <-  paste(formatDL(names(sorted), round(sorted, 2), "table"), note)
  }

  ## after a package check process finishes send some additional information
  add_msg <- ""
  if(!is.null(path_to_check_dir))
    if(file.exists(file.path(path_to_check_dir, "check.csv.diff")))
      add_msg <- readLines(file.path(path_to_check_dir, "check.csv.diff"))

  ## Text
  write(c(paste("R-Forge", platform, about, "log:"), " ",
          "Disk status:", " ",
          system("df -h", intern = TRUE), " ",
          "This packages have been kept back (stop list):", " ",
          donotcompiletxt, " ",
          pkg_txt, " ",
          "Timings [sec]:", " ", timings_table, " ", add_msg),
        file = attachment)
  mail_prog <- control$mail_program
  send_host <- control$mail_domain_name_of_sender
  relay_host <- control$mail_relay_server

  ## Windows (sendEmail) or Unix (mail) program to deliver reports
  writeLines("Sending ...")
  if(mail_prog == "sendEmail")
    system(paste(mail_prog, "-f", send_host, "-t", email,
                 paste("-u \"R-Forge: Nightly", about,"\" -m Summary -a"), attachment,
                 "-s", relay_host))
  if(mail_prog == "mail")
    system(paste("cat", attachment, "|", mail_prog,
                 paste("-s \"R-Forge: Nightly", about, "\""), email))
  system(paste("rm -f", attachment))
  writeLines("Done.")
}
