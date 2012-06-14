.libPaths("/home/rforge/lib/R")

require("rfTools")

## read connection information
conf_file <- "/home/rforge/conf/rf.conf"
rfc <- rf_read_configuration( file = conf_file )

load("/srv/rf/staging/pkg_status_cache.rda")

rf_remove_obsolete_pkg(rfc, pkg_status)


