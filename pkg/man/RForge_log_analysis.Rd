\name{RForgeLog}
\alias{rforge_stats_read_logs}
\alias{print.rforge_log}
\alias{[.rforge_log}
\alias{head.rforge_log}
\alias{tail.rforge_log}
\alias{as.data.frame.rforge_log}

\title{R-Forge Tools for Log Analysis}

\description{
  A set of tools for handling R-Forge log files.
}
\usage{
## reader
rforge_stats_read_logs (log_rep, from = NA, to = NA, subset = "package download",
                        file_name = "gforge.log")

## methods
\method{print}{rforge_log}(x, \ldots)
\method{head}{rforge_log}(x, \ldots)
\method{tail}{rforge_log}(x, \ldots)
\method{[}{rforge_log}(x, \ldots)
\method{as.data.frame}{rforge_log}(x, \ldots)
}

\arguments{
  \item{log_rep}{the location of the log files on the local file system.}
  \item{from}{the first date to be considered for reading. Default: \code{NA}.}
  \item{to}{the last date to be considered for reading. Default: \code{NA}.}
  \item{subset}{specifies the subset, i.e., which entries are to be
extracted from the logs. Currently only \code{"package download"} is available.}
  \item{file_name}{specifies the name of the log file. Default: \code{"gforge.log"}.}
  \item{x}{an object of class \code{rforge_log}.}
  \item{...}{further arguments are passed on.}
}
\details{The function \code{rforge_stats_read_logs()} reads particular
entries from log files made by the R-Forge system or any GForge
compatible log file repository typically produced by the
Apache web server. It returns an object of class \code{rforge_log} for
which we provide infrastructure like extractor functions or print
method.

A method to the generic function \code{as.data.frame} can be used to
coerce an \code{rforge_log} object to a data.frame. 
}
\value{
  An object of class \code{rforge_log} containing a particular subset of
entries in the given log files.
}

\references{
  R-Forge (\url{http://R-Forge.R-project.org}).
}
\author{Stefan Theussl}

\examples{
## read log entries (not run):
## log_entries_2008 <- rforge_stats_read_logs("path/to/logs", from = "2008/01/01",
##                       to = "2008/12/31", subset = "package download")

## coerce to data.frame (not run):
## log_data_2008 <- as.data.frame(log_entries_2008)

## the package includes a data set containing log entries for 2008
data(log_data_2008)
dim(log_data_2008)
summary(log_data_2008)
}
\keyword{utilities}