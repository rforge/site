\name{RForgeLog}
\alias{rforge_stats_read_logs}
\alias{print.rforge_log}
\alias{[.rforge_log}
\alias{head.rforge_log}
\alias{tail.rforge_log}
\alias{as.data.frame.rforge_log}

\title{R-Forge Tools for Log Analysis}

\description{
  A set of tools for analyzing R-Forge log files.
}
\usage{
## reader
rforge_stats_read_logs (log_rep, from = NA, to = NA, file_name = "gforge.log")

## methods
\method{print}{rforge_log}(x, \ldots)
\method{head}{rforge_log}(x, \ldots)
\method{tail}{rforge_log}(x, \ldots)
\method{[}{rforge_log}(x, \ldots)
\method{as.data.frame}{rforge_log}(x, \ldots)
}
\arguments{
  \item{log_rep}{the location of the log files on the local file system.}
  \item{from}{the first date to be considered for reading. default: \code{NA}}
  \item{to}{the last date to be considered for reading. default: \code{NA}}
  \item{file_name}{specifies the name of the log file. default:  \code{"gforge.log"}}
  \item{x}{an object of class \code{rforge_log}.}
  \item{...}{further arguments are passed on.}
}
\details{No further information available yet. Work in progress ...
}
\value{
  An object of class \code{rforge_log} containing subsets of the contents of all
  log files.
}

\references{
  R-Forge (\url{http://R-Forge.R-project.org}).
}
\author{Stefan Theussl}

\examples{
## no example available yet

}
\keyword{utilities}