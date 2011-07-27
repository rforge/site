## simple R script to analyze log files
library("alfa")

#log_dir <- "/mnt/data/cran"
log_dir <- "/mnt/data/test"
suppressWarnings(stat <- tryCatch(load(file.path(log_dir, "package_download_stats.rda")),
                                  error = identity))

read_apache_log <- function( file, format = c("common", "combined"), ... ){
  format <- match.arg(format)
  check <- read.table( file = file, na.strings = "-", nrows = 1, ... )
  stopifnot( ncol(check) == (apache_log_cols( format)+1) )
  x <- read.table( file = file, na.strings = "-", stringsAsFactors = FALSE, ... )
  ## read time stamp appropriately
  dates <- make_chron_object( x )
  x <- x[,-5]
  x[,4] <- dates
  colnames(x) <- apache_log_entries(format)
  x
}


make_chron_object <- function( x )
  as.chron( gsub("\\[|\\]", "", apply(x[, c(4, 5)], 1, function(line) paste(unlist(c(line)), collapse = " "))), "%d/%b/%Y:%T")


apache_log_cols <- function( format ){
  x <- c( common = 7, combined = 9)
  x[ format ]
}

apache_log_entries <- function( format ){
  x <- c( "Client_IP", "RFC1413_identity", "Userid", "Timestamp",
         "Request", "HTTP_status", "Object_size", "Referer", "User_agent" )
  switch(format,
         "common" = x[1:7],
         "combined" = x)
}



## log formats (see also http://httpd.apache.org/docs/2.2/logs.html):
## LogFormat "%h %l %u %t \"%r\" %>s %b \"%{Referer}i\" \"%{User-Agent}i\"" combined
## LogFormat "%h %l %u %t \"%r\" %>s %b" common
## LogFormat "%{Referer}i -> %U" referer
## LogFormat "%{User-agent}i" agent

log <- read_apache_log( file.path(log_dir, dir(log_dir)[1]), format = "combined" )
log2 <- alfa:::prepare_entries(log)
log3 <- alfa:::get_entries(log2)
if(inherits(stat, "error")){
  cran_stats <- rforge_stats_read_logs(log_dir, file_name = "full.access.log")
}

## our log object
rforge_stats

## show a some log entries
rforge_stats[50]
head(rforge_stats)
tail(rforge_stats)

## coerce to data.frame
tab <- as.data.frame(rforge_stats)
names(tab)

summary(tab)
