## R-Forge building setup
## 2008-02-21
## theussl

## WINDOWS

## Windows control file
control <- list()
## configure local paths
control$path_to_pkg_src            <- "C:\\srv\\R\\pkgs"                   ## R-Forge pkg sources
control$path_to_pkg_log            <- "C:\\srv\\rsync\\R.check\\buildlogs" ## Log directory
control$path_to_pkg_root           <- "C:\\srv\\rsync\\R-Forge\\"          ## R-Forge root (contains /src ,/bin)
control$path_to_local_texmf        <- NA                                   ## path to local texmf
control$path_to_local_library      <- "C:\\srv\\R\\lib\\pkgs"              ## path to local pkg library
control$stoplist                   <- "C:\\srv\\R\\lib\\scripts\\DoNotCompile" ## path to stoplist
## mail configuration
control$mail_domain_name_of_sender <- "Rbuild@xmhera.wu-wien.ac.at" 
control$mail_relay_server <- "statmath.wu-wien.ac.at"
control$mail_programme <- "sendEmail"                

## LINUX

## Linux control object: contains all necessary information
control <- list()
## configure local paths
control$path_to_pkg_src            <- NULL                      ## R-Forge pkg sources
control$path_to_pkg_log            <- NULL                      ## Log directory
control$path_to_pkg_root           <- NULL                      ## R-Forge root (contains /src ,/bin)
control$path_to_local_texmf        <- "/srv/R/share/texmf"      ## path to local texmf
control$path_to_local_library      <- Sys.getenv("R_LIBS")      ## path to local pkg library
control$stoplist                   <- NULL                      ## path to stoplist
## mail configuration
control$mail_domain_name_of_sender <- system("hostname -f", intern = TRUE) ## "xmhera.wu-wien.ac.at" 
control$mail_relay_server <- "statmath.wu-wien.ac.at"           ## only necessary with sendEmail
control$mail_programme <- "mail"                                ## on Windows: sendEmail
