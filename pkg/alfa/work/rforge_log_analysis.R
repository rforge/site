## simple R script to analyze log files
library("RForgeTools")

log_dir <- "/home/hadoop/Data/R-Forge_logs"
suppressWarnings(stat <- tryCatch(load(file.path(log_dir, "package_download_stats.Rda")),
                                  error = identity))

if(inherits(stat, "error")){
  rforge_stats <- rforge_stats_read_logs(log_dir)
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
