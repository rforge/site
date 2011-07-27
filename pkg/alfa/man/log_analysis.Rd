\name{reader}
\alias{read_access_log}

\title{Reader for Apache Log Files}

\description{
  This function reads a log file produced by an Apache web server from
  disk and returns a \code{'data.frame'}.
}
\usage{
## reader
read_access_log( file, format = c("common", "combined"), ... )
}

\arguments{
  \item{file}{a character string specifying the location of the log file
    on the local file system.}
  \item{format}{a character string specifying the log file
  format. Currently only \code{"common"} and \code{"combined"} are
  supported. See Details for more information.
}
\details{The function \code{read_access_log()} reads all entries from
  log files  produced by Apache web servers in one of two formats. The
  format \code{"common"} contains information about the client IP
  address, the RFC1413 identity, the userid determined by HTTP
  authentication, the timestamp of the request, the HTTP request, the
  HTTP status code, and the size of the object returned to the client
  (in bytes). Format \code{"combined"} additional includes information
  about the referer and the user agent.

  Each column in the resulting \code{'data.frame'} is of class
  \code{'character'} except \code{"Timestamp"} which inherits from class
  \code{'chron'} and columns \code{"HTTP_status"} and \code{"User_agent"}
  which are represented by a factor.
}
\value{
  A \code{'data.frame'}.
}

\references{
  \url{http://httpd.apache.org/docs/2.2/logs.html}.
}
\author{Stefan Theussl}

\examples{
## read log entries in format "common":
common <- read_access_log( system.file("logs/common.log", package =
"alfa") )
common
## read log entries in format "combined":
combined <- read_access_log( system.file("logs/combined.log", package =
"alfa"), format = "combined" )
combined
}
\keyword{utilities}