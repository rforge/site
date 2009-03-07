\name{log_data_2008}
\alias{log_data_2008}
\docType{data}
\title{R-Forge Package Download Log Data}
\description{
  The \code{log_data_2008} data set is a subset of the log entries made
  by the R-Forge (GForge) system in 2008. It contains log data related
  to R package downloads which were made in 2008.
}
\usage{data(log_data_2008)}
\format{A \code{data.frame} with size 59259 rows times 12 columns. This data set
  contains the following variables: \code{"IP"}, \code{"Date"},
  \code{"TZ"}, \code{"HTTP_Method"}, \code{"Path"},
  \code{"HTTP_Version"}, \code{"HTTP_Code"}, \code{"Size"},
  \code{"Connected_from"}, \code{"Client"}, \code{"Client_Version"}
  and \code{"Comment"}.
  IP addresses (variable \code{"IP"}) have been replaced by random
  strings due to privacy reasons.
}
\source{
  The data set has been obtained from the logs of the R-Forge web server
  \url{http://R-Forge.R-project.org}.
}
\keyword{datasets}
