read_access_log <- function( file, format = c("common", "combined"), ... ){
  format <- match.arg(format)
  check <- read.table( file = file, na.strings = "-", nrows = 1, ... )
  stopifnot( ncol(check) == (apache_log_cols( format)+1) )
  x <- read.table( file = file, na.strings = "-", stringsAsFactors = FALSE, ... )
  ## read time stamp appropriately
  dates <- make_chron_object( x )
  x <- x[,-5]
  x[,4] <- dates
  colnames(x) <- apache_log_entries(format)
  if( format == "combined"){
    x[["User_agent"]] <- as.factor(x[["User_agent"]])
  }
  x[["HTTP_status"]] <- as.factor(x[["HTTP_status"]])
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
