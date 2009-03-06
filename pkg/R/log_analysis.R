## takes a path to a gforge log repository and a time frame to consider (from, to) as arguments
## reads the logs and greps for interesting parts like downloaded packages
## TODO: provide more subsets for subsequent analysis, currently only package downloads

rforge_stats_read_logs <- function(log_rep, from = NA, to = NA, file_name = "gforge.log"){
  all_logs <- grep(file_name, dir(log_rep, recursive=TRUE), value = TRUE)
  if(is.na(from) && is.na(from)){
    logs <- file.path(log_rep, all_logs)
    dates_avail <- sort(as.Date(dirname(all_logs)))
    start <- as.Date(all_logs[1])
    end <- as.Date(all_logs[length(all_logs)])
  }
  else {
    dates_avail <- as.Date(dirname(all_logs))
    start <- as.Date(from)
    end <- as.Date(to)
    if(! ((start %in% dates_avail) && (end %in% dates_avail)) )
      stop("no log files available for this time frame.")
    logs <- file.path(log_rep, gsub("-", "/", as.character(seq(from = start, to = end, by = 1))), file_name)
  }
  download_stats <- character()
  for( log in logs ){
    content <- readLines(log)
    pkgs_src_contrib <- grep(".tar.gz", content[grep("src/contrib/", content)], value = TRUE)
    pkgs_bin_win_contrib <- grep(".zip", content[grep("bin/windows/contrib/", content)], value = TRUE)
    pkgs_bin_mac_contrib <- grep(".tgz", content[grep("bin/macosx/", content)], value = TRUE)
    download_stats <- c(download_stats,  pkgs_src_contrib, pkgs_bin_win_contrib, pkgs_bin_mac_contrib)
  }
  out <- list(from = start,
              to = end,
              n_entries = length(download_stats),
              content = download_stats,
              subset = "package download")
  class(out) <- "rforge_log"
  out
}

print.rforge_log <- function(x, ...){
  header <- sprintf("R-Forge log entries from %s to %s", as.character(x$from), as.character(x$to))
  if(is.null(x$subset)){
    writeLines(header)
    writeLines(sprintf("This subset contains %d entries", x$n_entries))
  }
  else{
    writeLines(header)
    writeLines(sprintf("This '%s' subset contains %d entries", x$subset, x$n_entries))
  }
}


tail.rforge_log <- function(x, ...){
  tail(x$content, ...)
}

head.rforge_log <- function(x, ...){
  utils:::head(x$content, ...)
}

"[.rforge_log" <- function(x, ...){
  x$content[...]
}

as.data.frame.rforge_log <- function(x, ...){
  entries <- get_entries(prepare_entries(x$content))
  tab <- as.data.frame( matrix( unlist(entries), ncol = 10, byrow= TRUE), ... )
  colnames(tab) <- c("IP", "Date", "TZ", "HTTP_Method", "Path", "HTTP_Version", "HTTP_Code", "Size", "Connected_from", "Client")
  tab
}

prepare_entries <- function(x)
  gsub("\\]", "", gsub("\\[", "", gsub("\"", "", x)))

get_entries <- function(x)
  lapply(strsplit(x, " "), function(x) {
    len <- length(x)
    browser <- NA
    if(len > 9)
      browser <- paste(x[10:len], collapse = " ")
    c(x[1:9], browser)})
