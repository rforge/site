## read subset from logs of 2008
## theussl, 2009

library("RForgeTools")

log_entries_2008 <- rforge_stats_read_logs("/home/hadoop/Data/R-Forge_logs/",
                                           from = "2008/01/01", to = "2008/12/31", 
                                           subset = "package download")
## <FIXME> we cannot provide this data as we have to remove or change IP addresses
## save(log_entries_2008, file = "log_entries_2008.Rda")

summary(log_entries_2008)

## coerce to data.frame
log_data_2008 <- as.data.frame(log_entries_2008)
## replace ip addresses with randomized addresses due to privacy reasons
set.seed(1782)
n <- length(unique(log_data_2008[["IP"]]))
new_ip <- apply(matrix(sample(letters, 3*4*n, replace = TRUE), ncol = 12), 1, function(x) {
            paste(c(paste(x[1:3], collapse = ""), paste(x[4:6], collapse = ""),
            paste(x[7:9], collapse = ""), paste(x[10:12], collapse = "")), collapse = ".")
          })

levels(log_data_2008[["IP"]]) <- new_ip

summary(log_data_2008)

## 
save(log_data_2008, file = "../data/log_data_2008.RData")
