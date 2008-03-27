\name{RForgeTools}
\alias{build_packages}
\alias{check_packages}
\title{R-Forge Package Building and Checking Infrastructure}
\description{
  A set of tools providing the complete R-Forge build/check environment.
  Completely platform independend as one can specify on which platform to use.
}
\usage{
build_packages (email,
		platform     = c("Linux", "Windows", "MacOSX"),
                architecture = c("x86_32", "x86_64"),
                rforge_url   = "http://R-Forge.R-project.org",
                cran_url     = "http://CRAN.R-project.org",
                control      = list())
check_packages (email,
                platform           = c("Linux", "Windows", "MacOSX"),
                architecture       = c("x86_32", "x86_64"),
                rforge_url = "http://R-Forge.R-project.org",
                cran_url           = "http://CRAN.R-project.org",
                control=list())
}
\arguments{
  \item{email}        {specifies the email adress where the status report should get delivered to.}
  \item{platform}        {specifies the platform for which the packages are built/checked..}
  \item{architecture} {specifies the architecture for which the packages are built/checked.}
  \item{rforge_url}   {specifies the URL of R-Forge. default: \code{"http://R-Forge.R-project.org"}}
  \item{cran_url}     {specifies the URL of CRAN. default: \code{"http://CRAN.R-project.org"}}
  \item{control}      {a list with additional information needed by the processes}
}
\details{ No further information available yet. Work in progress ...
}
\value{
  \code{TRUE} if process was successful.
}

\references{
  R-Forge
  (\url{http://R-Forge.R-project.org}).
}
\author{Stefan Theussl}

\examples{
## no example available yet

}
\keyword{utilities}