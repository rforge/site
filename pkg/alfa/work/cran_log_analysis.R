## simple R script to analyze log files
library("alfa")
library("multicore")

ncores <- 4

## Uncompressed CRAN log files are stored in:
log_dir <- "/mnt/data/cran"

outfile <- "/mnt/data/cranlogs.rda"
files <- dir(log_dir)
mclapply( files, function(x) {cranlog <- read_access_log( file.path(log_dir, x), format = "combined" )
                              save(cranlog, file = file.path(log_dir, paste(x, "rda", sep =".")))}, mc.cores = ncores)
q( save = "no" )

## if something files see which one 
files <- dir(log_dir)
sub2 <- sub(".rda", "", files)
files <- setdiff(sub2, sub2[duplicated(sub2)])

##########################################################################
## ANALYSIS
##########################################################################

##########################################################################
## regexps
regexp_R_client <- "^R \\(.[.].+[.]."

##########################################################################
## helper functions
match_source_packages <- function( x ){
  ## match for tarballs in /src/contrib and /contrib/main
  intersect(grep("(/src/contrib)|(/contrib/main)", x[["Request"]]), grep("[.]tar[.]gz", x[["Request"]]))
}
match_lin_packages <- function( x ){
  ## only debian and ubuntu
  intersect(grep("(/bin/linux/debian)|(/bin/linux/ubunut)", x[["Request"]]), grep("r-cran", x[["Request"]]))
}
match_mac_packages <- function( x ){
  intersect(grep("(/bin/macosx/leopard/contrib)|(/bin/macosx/universal/contrib)", x[["Request"]]), grep("[.]tgz", x[["Request"]]))
}
match_win_packages <- function( x ){
  intersect(grep("(/bin/windows/contrib)", x[["Request"]]), grep("[.]zip", x[["Request"]]))
}


##########################################################################
## SRC/LINUX/MAC/WINDOWS
load("/mnt/data/cran/full.access.log.10.rda")

R_client_download_stats <- function( log ){
  GET <- log[grep("^GET", log[["Request"]]), ]
  clients <- grep( regexp_R_client, levels(log[["User_agent"]]), value = TRUE )
  GETSRC <- subset(GET[match_source_packages(GET), ], User_agent %in% clients)[, c("User_agent", "Object_size")]
  GETMAC <- subset(GET[match_mac_packages(GET), ], User_agent %in% clients)[, c("User_agent", "Object_size")]
  GETWIN <- subset(GET[match_win_packages(GET), ], User_agent %in% clients)[, c("User_agent", "Object_size")]
  req_src <- aggregate( Object_size ~ User_agent,
                        data = GETSRC, length)
  colnames(req_src) <- c(colnames(req_src)[1], "Requests")
  req_mac <- aggregate( Object_size ~ User_agent,
                        data = GETMAC, length)
  colnames(req_mac) <- c(colnames(req_mac)[1], "Requests")
  req_win <- aggregate( Object_size ~ User_agent,
                        data = GETWIN, length)
  colnames(req_win) <- c(colnames(req_win)[1], "Requests")
  tra_src <- aggregate( Object_size ~ User_agent,
                        data = GETSRC, function(x) sum(as.numeric(x)))
  tra_mac <- aggregate( Object_size ~ User_agent,
                        data = GETMAC, function(x) sum(as.numeric(x)))
  tra_win <- aggregate( Object_size ~ User_agent,
                       data = GETWIN, function(x) sum(as.numeric(x)))
  list( Requests = list(SRC = req_src,
                        MAC = req_mac,
                        WIN = req_win),
        Traffic  = list(SRC = tra_src,
                        MAC = tra_mac,
                        WIN = tra_win)
          )
}

## Linux clients extra
GET <- cranlog[grep("^GET", cranlog[["Request"]]), ]
tab_lin <- table( GET[match_lin_packages(GET), ][["User_agent"]] )
tab_lin <- tab_lin[grep("(apt)|(APT)", names(tab_lin), value = TRUE)]
tab_lin <- tab_lin[tab_lin > 0]
tab_lin

## R client downloads
downloads <- R_client_download_stats(cranlog)
downloads


## aggregate for R minor version
## Requests
req <- lapply( downloads$Requests, function(x){
  minor_version <- gsub("[.]$", "", sapply(x[["User_agent"]], substr, 4, 7))
  x[["User_agent"]] <- minor_version
  aggregate(Requests ~ User_agent, sum, data = x)
} )
## Traffic
tra <- lapply( downloads$Traffic, function(x){
  minor_version <- gsub("[.]$", "", sapply(x[["User_agent"]], substr, 4, 7))
  x[["User_agent"]] <- minor_version
  aggregate(Object_size~ User_agent, sum, data = x)
} )
## Bytes per Request
## bytes per request
bpr <- lapply( names(tra), function(x) data.frame("User_agent" = tra[[x]]$User_agent, "Ratio" = req[[x]]$Object_size/req[[x]]$Requests))
names(bpr) <- names(downloads$Traffic)
lapply( bpr, function(x){
  minor_version <- gsub("[.]$", "", sapply(x[["User_agent"]], substr, 4, 7))
  x[["User_agent"]] <- minor_version
  aggregate(Ratio ~ User_agent, mean, data = x)
} )

## package downloads for each platform
req <- lapply( downloads$Requests, function(x) sum(x[["Requests"]]) )

## download traffic for each platforms
tra <- lapply(downloads$Traffic, function(x){
  tra <- sum(x[["Object_size"]])
  class(tra) <- "object_size"
  tra})
lapply(tra, print, units = "Gb")








if( !file.exists(outfile) ){
  files <- dir(log_dir)
  cranlogs <- NULL
  for( file in files ){
    writeLines(sprintf("Reading file: %s", file))
    cranlogs <- rbind(read_access_log( file.path(log_dir, file), format = "combined" ), cranlogs)
    save(cranlogs, file = paste(outfile, "unfinished", sep = "."))
  }
  save(cranlogs, file = outfile)
}



if( !file.exists(outfile) ){
  files <- dir(log_dir)
  n <- floor( length(files)/ncores )
  rest <- length(files) - n*ncores
  cranlogs <- lapply(1:10-1, function(i) Reduce(rbind, mclapply( files[ (i*ncores+1):(i*ncores+ncores)], function(x) read_access_log( file.path(log_dir, x), format = "combined" ), mc.cores = ncores)))
    #save(cranlogs, file = paste(outfile, i, sep = "."))
  }
}




if( !file.exists(outfile) ){
  for(i in parts-1){
    files <- dir(log_dir)
    len <- floor(length(files)/parts)
    rest <- length(files) - len*parts
    cranlogs <- Reduce(rbind, mclapply( files[ (i*len+1):(i*len+len)], function(x) read_access_log( file.path(log_dir, x), format = "combined" ), mc.cores = ncores))
    save(cranlogs, file = paste(outfile, i, sep = "."))
  }
}


## log formats (see also http://httpd.apache.org/docs/2.2/logs.html):
## LogFormat "%h %l %u %t \"%r\" %>s %b \"%{Referer}i\" \"%{User-Agent}i\"" combined
## LogFormat "%h %l %u %t \"%r\" %>s %b" common
## LogFormat "%{Referer}i -> %U" referer
## LogFormat "%{User-agent}i" agent

## if(inherits(stat, "error")){
##   cran_stats <- rforge_stats_read_logs(log_dir, file_name = "full.access.log")
## }

## ## our log object
## rforge_stats

## ## show a some log entries
## rforge_stats[50]
## head(rforge_stats)
## tail(rforge_stats)

## ## coerce to data.frame
## tab <- as.data.frame(rforge_stats)
## names(tab)

## summary(tab)
