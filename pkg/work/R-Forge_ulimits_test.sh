#!/bin/sh
## R-Forge package building
## just calls R and the R-Forge infrastructure package which includes
## all the necessary functions
## theussl 2008-02
##

## before we start we set limits for all new processes
## core size (size of a core dump of a program). default 0
ulimit -c 0
## main memory limit, default unlimited -> we set it to 256MB
ulimit -m 262144
## virtual memory (swap space), default unlimited -> we set it to 256MB
ulimit -v 262144
## main memory limit, default unlimited -> we set it to 256MB
ulimit -l 262144 
## max user processes, default 266 (Linux unlimited) -> we set it to 200
ulimit -u 200
## max cpu time, default unlimited -> we set it to 10m
ulimit -t 80
## max file size, default unlimited -> we set it to 256MB
ulimit -f 524288


## Where is the package library?
export R_LIBS="/Users/theussl/lib/R"
## On MacOSX gfortran is an external package, where are the links to it
export PATH=/usr/local/bin:$PATH
## on Leopard we have to export X11 bin directory
export PATH=/usr/X11/bin:$PATH
## R build flavor
R_flavor='R-patched'
R_dir=/srv/R
## our stop_list (exported with rforge_generate_stoplist)
stoplist=$R_dir/lib/check_R_stoplist

## as long as we have no own builds
R_bin="/Library/Frameworks/R.framework/Resources/bin/R"

${R_bin} --vanilla --slave <<-EOF
library("RForgeTools", lib="~/lib/R")
## On Mac we have to set the DISPLAY variable to use TCLTK
#Sys.setenv("DISPLAY"=":10")
## IMPORTANT:
## setup control file
control <- list()
control\$path_to_pkg_src            <- "/srv/R/pkgs"             ## R-Forge pkg sourced
control\$path_to_pkg_log            <- "/srv/R/logs"             ## Log directory
control\$path_to_pkg_root           <- "/srv/R/R-Forge"          ## R-Forge root (contains /src ,/bin)
control\$path_to_local_texmf        <- ""      ## path to local texmf
control\$path_to_local_library      <- Sys.getenv("R_LIBS")      ## path to local pkg library
control\$path_to_pkg_tarballs       <- "/srv/R/R-Forge"
control\$stoplist                   <- "$stoplist"                      ## path to stoplist
## mail configuration
control\$mail_domain_name_of_sender <- system("hostname", intern = TRUE) ## "R-apple.wu-wien.ac.at" 
control\$mail_relay_server <- "statmath.wu-wien.ac.at"           ## only necessary with sendEmail
control\$mail_program <- "mail"                                ## on Windows: sendEmail

writeLines("Starting to build/check package")

R <- file.path(R.home(), "bin", "R")
pid <- RForgeTools:::start_virtual_X11_fb()
system.time(
out <- system(paste(R, "CMD", "build", file.path(control\$path_to_pkg_src, "AquaEnv")))
)
out
system.time(
out <- system(paste(R, "CMD", "build", file.path(control\$path_to_pkg_src, "phylobase")))
)
out
## dies in MacOSX as infinite warnings are printed to the display
#system.time(
#out <- system(paste(R, "CMD", "check", file.path(control\$path_to_pkg_src, "TSodbc")))
#)
#out


RForgeTools:::close_virtual_X11_fb(pid)

writeLines("Master Process didn't die yet")
EOF

echo "exiting shell script"
exit 0
