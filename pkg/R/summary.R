## TODO: time_i.out
finalize_check_results <- function(check_results_dir, path_to_pkg_src, check_args, timings,
                                       check_results_files = c("SUMMARY", "check.csv", "time_c.out")){
  ## search check directory for check results
  ## check_results_dir <- "/srv/rsync/R-Forge.check/R-devel"
  ## path_to_pkg_src <- "/srv/R/pkgs"
  PKGS <-  file.path(check_results_dir, "PKGS")
  Rcheck <- dir(PKGS)
  Rcheck <- Rcheck[grep(".Rcheck$", Rcheck)]
  ## successfully checked
  ##pkgs <- gsub(".Rcheck$", "", Rcheck)
  
  ## provide DESCRIPTION metadata in directories with check results
  for(dir in Rcheck){
    pkg <- gsub(".Rcheck$", "", dir)
    file.copy(file.path(path_to_pkg_src, pkg, "DESCRIPTION"), file.path(PKGS, dir, "00package.dcf"))
  }
  
  ## save old check results
  suffix <- ".prev"
  check_results_files <- file.path(check_results_dir, check_results_files)
  for(file in check_results_files){
    previous_file <- paste(file, suffix, sep = "")
    if(file.exists(previous_file))
      file.remove(previous_file)
    if(file.exists(file))
      file.rename(file, previous_file)
  }

  if(!check_directory(file.path(check_results_dir, "Results"), fix=TRUE))
    stop(paste("There is no directory", file.path(check_results_dir, "Results"),"!"))
  
  today <- format(Sys.time(), "%F")
  results_archive <- file.path(check_results_dir, "Results", today)

  if(!check_directory(results_archive, fix=TRUE))
    stop(paste("There is no directory", results_archive,"!"))

  ## not yet included
  #PKGS_prev <- paste(PKGS, suffix, sep = "")
  #if(file.exists(PKGS_prev))
  #  unlink(PKGS_prev, recursive = TRUE)
  #if(file.exists(PKGS))
  #  file.rename(PKGS, PKGS_prev)
  ##test -d PKGS      && mv PKGS PKGS.prev
  ##mv Work/PKGS PKGS
  ## Move timings up.
  ##mv Work/time_c.out time_c.out
  ##mv Work/time_i.out time_i.out

  write_check_timings(file = file.path(check_results_dir, "time_c.out"), Rcheck, timings)
  
  ## Create check.csv.
  write_check_csv(file = file.path(check_results_dir, "check.csv"), Rcheck, PKGS, path_to_pkg_src, check_args)

  ## Create SUMMARY.
  write_check_SUMMARY(file = file.path(check_results_dir, "SUMMARY"), Rcheck, PKGS, path_to_pkg_src)

  for(file in check_results_files){
    file.copy(file, results_archive)
  }

  write_check_diffs(check_results_dir)  
}


## Create check.csv.
## for testing purposes: check_args <- read.csv("/srv/R/lib/check_R_stoplist", stringsAsFactors = FALSE)
write_check_csv <- function(Rcheck, check_dir, path_to_pkg_src, check_args, file = "check.csv"){
  fields <- c("Package", "Version", "Priority", "Maintainer", "Status", "Comment")
  len <- length(Rcheck)
  csv <- matrix(rep(NA, len * length(fields)), nrow = len)
  colnames(csv) <- fields
  for(i in 1:len){
    dir <- Rcheck[i]
    pkg <- gsub(".Rcheck$", "", dir)
    ## FIXME: When reading DCF files, we take the first result. E.g., BIOMOD returns the actual value plus NA. An encoding issue?
    suppressWarnings(version <- tryCatch(read.dcf(file.path(path_to_pkg_src, pkg, "DESCRIPTION"), "Version")[1], error = identity))
    if(inherits(version, "error"))
      version <- NA
    suppressWarnings(priority <- tryCatch(read.dcf(file.path(path_to_pkg_src, pkg, "DESCRIPTION"), "Priority")[1], error = identity))
    if(inherits(priority, "error"))
      priority <- NA
    suppressWarnings(maintainer <- tryCatch(read.dcf(file.path(path_to_pkg_src, pkg, "DESCRIPTION"), "Maintainer")[1], error = identity))
    if(inherits(maintainer, "error"))
      maintainer <- NA
    checklog <- readLines(file.path(check_dir, dir, "00check.log"))
    warnings <- grep("WARNING$", checklog, fixed = TRUE, useBytes = TRUE)
    errors <- grep("ERROR", checklog, fixed = TRUE, useBytes = TRUE)
    if(length(errors))
      status <- "ERROR"
    else
      if(length(warnings))
        status <- "WARN"
      else
        status <- "OK"
    
    args <- get_check_args(pkg, check_args)
    if(length(args))
      args <- sprintf("[%s]", args)
    else
      args <- ""
    csv[i, ] <- c(pkg, version, priority, maintainer, status, args)
  }
  write.csv(csv, file, row.names = FALSE)
  invisible(TRUE)
}


write_check_SUMMARY <- function(Rcheck, check_dir, path_to_pkg_src, file = "SUMMARY"){
  len <- length(Rcheck)
  SUMMARY <- character()
  for(i in 1:len){
    dir <- Rcheck[i]
    pkg <- gsub(".Rcheck$", "", dir)
    checklog <- readLines(file.path(check_dir, dir, "00check.log"))
    problems <- grep("(^\\*\tRd files|^\'*\tnon-standard|(WARNING|ERROR)$)", checklog)
    suppressWarnings(maintainer <- tryCatch(read.dcf(file.path(path_to_pkg_src, pkg, "DESCRIPTION"), "Maintainer")[1], error = identity))
    if(inherits(maintainer, "error"))
      maintainer <- NA
    if(length(problems)){
      SUMMARY <- c(SUMMARY, pkg, sprintf("Maintainer: %s", maintainer), checklog[problems])
    }
  }
  writeLines(SUMMARY, file)
  invisible(TRUE)
}

write_check_timings <- function(Rcheck, timings, file = "time_c.out"){
  len <- length(Rcheck)
  time_c <- character(len)
  for(i in 1:len){
    dir <- Rcheck[i]
    pkg <- gsub(".Rcheck$", "", dir)
    time_c[i] <- sprintf("%s: %s", pkg, timings[pkg])
  }
  writeLines(time_c, file)
}

write_check_diffs <- function(check_dir, files = "check.csv"){
  files <- file.path(check_dir, files)
  for(file in files){
    if(file.exists(sprintf("%s.prev", file))){
      system(sprintf("diff %s.prev %s > %s.diff", file, file, file))
      system(sprintf("test -s %s.diff || rm -f %s.diff", file, file))
    }
    
    if(file.exists(sprintf("%s.diff", file))){
      db <- check_results_diffs(file.path(check_dir))
      sink(sprintf("%s.diff", file))
      writeLines(c("Changes in check status (S) and/or version (V)",
                   do.call(sprintf,
                           c(list("using R %s.%s (%s-%s-%s r%s):\n"),
                             R.version[c("major", "minor", "year",
                                         "month", "day", "svn rev")]))))
      print(db)
      sink()
    }
  }
  invisible(TRUE)
}
