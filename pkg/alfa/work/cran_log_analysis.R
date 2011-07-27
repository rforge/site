## simple R script to analyze log files
library("alfa")

#log_dir <- "/mnt/data/cran"
log_dir <- "/mnt/data/test"
suppressWarnings(stat <- tryCatch(load(file.path(log_dir, "package_download_stats.rda")),
                                  error = identity))

## log formats (see also http://httpd.apache.org/docs/2.2/logs.html):
## LogFormat "%h %l %u %t \"%r\" %>s %b \"%{Referer}i\" \"%{User-Agent}i\"" combined
## LogFormat "%h %l %u %t \"%r\" %>s %b" common
## LogFormat "%{Referer}i -> %U" referer
## LogFormat "%{User-agent}i" agent

log <- read_apache_log( file.path(log_dir, dir(log_dir)[1]), format = "combined" )


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
