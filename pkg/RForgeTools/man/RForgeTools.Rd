\name{RForgeTools}
\alias{build_packages}
\alias{check_packages}
\alias{R_Forge_control}
\title{R-Forge Package Building and Checking Infrastructure}
\description{
  A set of tools providing the complete R-Forge build/check environment.
  Works on every major platform as we use platform independend R code.
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
R_Forge_control(path_to_pkg_src, path_to_pkg_log, path_to_pkg_root,
                path_to_local_texmf, path_to_local_library,
                stoplist, mail_domain_name_of_sender, mail_relay_server,
                mail_programme = "mail", path_to_check_dir = "",
                cpu_time_limit = 600)
}
\arguments{
  \item{email}{the email adress where the status report is delivered to.}
  \item{platform}{specifies the platform on which the packages are
    built/checked.}
  \item{architecture}{specifies the architecture on which the packages
    are built/checked.}
  \item{rforge_url}{specifies the repository URL of R-Forge.
    Default: \code{"http://R-Forge.R-project.org"}.}
  \item{cran_url}{specifies the repository URL of CRAN.
    Default: \code{"http://CRAN.R-project.org"}.}
  \item{bioc_url}{specifies the repository URL of Bioconductor.
    Default: \code{"http://bioconductor.org/packages/release/bioc"}.}
  \item{control}{a list with additional information needed by the
    processes}
  \item{path_to_pkg_src}{This is where the extracted package sources can
    be found. Typically checked out once daily from the R-Forge SVN
    repositories.}
  \item{path_to_pkg_log}{Specifies the path where the log files are
    written to.}
  \item{path_to_pkg_root}{Specifies the path where the built packages
    are copied to.}
  \item{path_to_local_texmf}{Path to the local texmf tree.}
  \item{path_to_local_library}{The temporary R packages library.}
  \item{stoplist}{The path to the stoplist}
  \item{mail_domain_name_of_sender}{Typically the fully qualified domain
    name (FQDN) of the local machine.}
  \item{mail_relay_server}{The FQDN of the mail server for relaying.}
  \item{mail_programme}{The mail programme for sending status mails.
    Default: \code{"mail"}}
  \item{path_to_check_dir}{Specifies the path where the check logs are
    placed. Default: \code{""}}
  \item{cpu_time_limit}{Default: \code{600}}
}
\details{No further information available yet. Work in progress ...
}
\value{
  \code{TRUE} if build or check process was successful.
}

\references{
  R-Forge (\url{http://R-Forge.R-project.org}).
}
\author{Stefan Theussl}

\examples{
## First setup the control file

control <- R_Forge_control("/path/to/R/pkgs", 
                           "/path/to/R/logs/R-patched",
                           "/path/to/R/R-Forge",
                           "/usr/local/share/texmf",
                           "/path/to/local_lib",
                           "/path/to/stoplist",
                           system("hostname -f", intern = TRUE),
                           "mailhost.domain.org"
)

## start building (not run) ...
## build_packages(email        = "some.user@some.domain.org",
##                platform     = "Linux",
##                architecture = "x86_64",
##                control      = control)

}
\keyword{utilities}