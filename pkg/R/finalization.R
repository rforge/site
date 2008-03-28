provide_packages_in_contrib <- function(build_dir, contrib_dir, platform){

  file_types <- c(Linux = ".tar.gz", MacOSX = ".tgz", Windows = ".zip")
  file_type <- file_types[platform]
  pkg_types <- c(Linux = "source", MacOSX = "mac.binary", Windows = "win.binary")
  pkg_type <- pkg_types[platform]
  ## Hard coding fields (we are indexing by number, so hard-coding important here!)
  fields <- tools:::.get_standard_repository_db_fields()
  ## Remember old working directory
  old_dir <- file_path_as_absolute(getwd())
  setwd(build_dir)
  files <- dir()
  files <- files[grep(file_type, files)]
  for(i in files){
    ## copy package to contrib directory
    file.copy(i, contrib_dir, overwrite = TRUE)
    ## delete package from build directory
    system(paste("rm -f", i))
  }
  ## remove old PACKAGES file
  setwd(contrib_dir)
  system(paste("rm -f PACKAGES*"))
  ## now find out which packages have old versions
  tmp <- dir()
  splitted <- strsplit(tmp, "_")
  packages <- sapply(splitted, "[", 1)
  ind <- 1:length(packages)
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
  ## Unix: wd <- getwd(); tools:::write_PACKAGES(wd,type="source")
  ## Under Windows additionally
  ##shell(paste("gzip -c", file.path(contrib_dir, "PACKAGES"), 
  ##">", file.path(contrib_dir, "PACKAGES.gz")))
  ## back to old directory
  setwd(old_dir)
  unique(packages)
}

notify_admins <- function(packages, donotcompile, email, platform, control, timings = NULL, about = c("build", "check")){
  about <- match.arg(about)
  proc_time_minutes <- round(proc.time()["elapsed"]/60, 2)
  attachment <- "Rnightlybuildmail.txt"
  donotcompiletxt <- paste(donotcompile, collapse="\n")
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
    timings <- apply(matrix(c(names(timings), timings),ncol =2), 1, function(x) {paste(c(names(x), x), collapse = " ")})
  }
  
  write(c(paste("R-Forge", platform, about, "log:"), " ",
          "Disk status:", " ",
          system("df -h", intern = TRUE), " ",
          "This packages have been kept back (stop list):", " ",
          donotcompiletxt, " ",
          pkg_txt, " ",
          unique(packages), " ",
          "Timings [sec]:", " ", timings),
        file = attachment)
  mail_prog <- control$mail_program
  send_host <- control$mail_domain_name_of_sender
  relay_host <- control$mail_relay_server
  
  if(mail_prog == "sendEmail")
    system(paste(mail_prog, "-f", send_host, "-t", email,
                 "-u \"R-Forge: Nightly build\" -m Summary -a", attachment,
                 "-s", relay_host))
  if(mail_prog == "mail")
    system(paste("cat", attachment, "|", mail_prog, 
                 "-s \"R-Forge: Nightly build\"", email))
  system(paste("rm -f", attachment))  
}
