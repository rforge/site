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
                bioc_url     = "http://bioconductor.org/packages/release/bioc",
                control      = list())
check_packages (email,
                platform     = c("Linux", "Windows", "MacOSX"),
                architecture = c("x86_32", "x86_64"),
                rforge_url   = "http://R-Forge.R-project.org",
                cran_url     = "http://CRAN.R-project.org",
                bioc_url     = "http://bioconductor.org/packages/release/bioc",
                control=list())
}
\arguments{
  \item{email}{the email adress where the status report is delivered to.}
  \item{platform}{specifies the platform on which the packages are built/checked..}
  \item{architecture}{specifies the architecture on which the packages are built/checked.}
  \item{rforge_url}{specifies the repository URL of R-Forge. Default: \code{"http://R-Forge.R-project.org"}.}
  \item{cran_url}{specifies the repository URL of CRAN. Default: \code{"http://CRAN.R-project.org"}.}
  \item{bioc_url}{specifies the repository URL of Bioconductor. Default: \code{"http://bioconductor.org/packages/release/bioc"}.}
  \item{control}{a list with additional information needed by the processes}
}
\details{ No further information available yet. Work in progress ...
}
\value{
  \code{TRUE} if build or check process was successful.
}

\references{
  R-Forge (\url{http://R-Forge.R-project.org}).
}
\author{Stefan Theussl}

\examples{
## no example available yet

}
\keyword{utilities}