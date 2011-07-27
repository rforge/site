
## takes a path to a gforge log repository and a time frame to consider (from, to) as arguments
## reads the logs and greps for interesting parts like downloaded packages
## TODO: provide more subsets for subsequent analysis, currently only package downloads

rforge_stats_read_logs <- function(log_rep, from = NA, to = NA, subset = "package download",
                                   file_name = "gforge.log"){
  if(! (subset == "package download") )
    stop("currently only subset 'package download' is supported")
  ## <FIXME> We should check for gforge log structure first
  all_logs <- grep(file_name, dir(log_rep, recursive=TRUE), value = TRUE)
  if(is.na(from) && is.na(to)){
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
              subset = subset)
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
  if(!inherits(x, "rforge_log"))
    stop("'x' must be an object of type 'rforge_log'")
  ## first prepare input -> a list containing in each element a vector of length 10
  ## representing the data of the corresponding line in the log
  entries <- get_entries(prepare_entries(x$content))
  ## coerce to a data.frame
  tab <- as.data.frame(matrix( unlist(entries), ncol = 10, byrow= TRUE),
                       stringsAsFactors = FALSE, ... )
  ## the last element is the information about the browser. we want to split
  ## this section appropriately so that we have Client name, Version string, and further info
  client <- sub("/", " ", gsub("\\)", "", gsub("\\(", "", as.character(tab[[10]]))))
  client <- unlist(lapply(strsplit(client, " "), function(x) {
    c(x[1:2], paste(x[3:length(x)], collapse = " "))}))
  tab <- cbind(tab[, -10], as.data.frame(matrix( unlist(client), ncol = 3, byrow= TRUE),
                                         stringsAsFactors = FALSE, ... ))
  
  colnames(tab) <- c("IP", "Date", "TZ", "HTTP_Method", "Path",
                     "HTTP_Version", "HTTP_Code", "Size", "Connected_from",
                     "Client", "Client_Version", "Comment")
  
  tab[["IP"]] <- as.factor(tab[["IP"]])
  tab[["Date"]] <- as.Date(tab[["Date"]], "%d/%b/%Y:%T")
  ## <FIXME> should we remove TZ and change Date appropriately?
  tab[["TZ"]] <- as.factor(tab[["TZ"]])
  ## the relative path from the domain
  tab[["Path"]] <- as.factor(tab[["Path"]])
  tab[["HTTP_Method"]] <- as.factor(tab[["HTTP_Method"]])
  tab[["HTTP_Version"]] <- as.factor(tab[["HTTP_Version"]])
  tab[["HTTP_Code"]] <- as.factor(tab[["HTTP_Code"]])
  tab[["Connected_from"]][grep("^-$", tab[["Connected_from"]])] <- NA
  tab[["Connected_from"]] <- as.factor(tab[["Connected_from"]])
  ## some lines have a "-" entry -> coerced correctly to NA but gives warning
  suppressWarnings(tab[["Size"]] <- as.integer(tab[["Size"]]))
  ## now the clients, this can be a browser, wget, a crawler or R. "-" means NA
  tab[["Client"]][grep("-", tab[["Client"]])] <- NA
  tab[["Client"]] <- as.factor(tab[["Client"]])
  ## Client Version -> factor. <FIXME> Some Clients have non standard Version strings </FIXME>
  tab[["Client_Version"]] <- as.factor(tab[["Client_Version"]])
  ## Comment: Again watch out for "-" -> NA
  tab[["Comment"]][grep("^-$", tab[["Comment"]])] <- NA
  tab[["Comment"]] <- as.factor(tab[["Comment"]])
  ## we don't want the time zone
  tab
}

prepare_entries <- function(x)
  gsub("\\]", "", gsub("\\[", "", gsub("\"", "", x)))

get_entries <- function(x){
  out <- lapply(strsplit(x, " "), function(x) {
    len <- length(x)
    browser <- NA
    if(len > 9)
      browser <- paste(x[10:len], collapse = " ")
    c(x[1:9], browser)})
  out
}
