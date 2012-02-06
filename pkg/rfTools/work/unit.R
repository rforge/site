require(rfTools)

## tests:
#rf <- rfTools:::.create_rf_config(db_name = "db_name",
#                                  db_user = "rf_table",
#                                  db_password = "db_pw",
#                                  svn_root = "/srv/svn", tmp_dir = "/tmp")
#testfile <- tempfile()

#rf_write_configuration( rf, file = "/home/rforge/conf/rf.conf" )
#file.remove( testfile )
rf <- rf_read_configuration( file = "/home/rforge/conf/rf.conf" )

rf
unclass(rf)

rfTools:::.check_rf_for_sanity(rf)

## update package status db (which pacakges are brand new, current, or outdated)
pkg_status_cache <- file.path( rfTools:::.rf_get_tmp(rf), "pkg_status_cache.rda" )
if( !file.exists(pkg_status_cache) ){
  pkg_status <- rfTools:::rf_pkg_status(rf)
  save(pkg_status, file = pkg_status_cache)
}else{
  load( pkg_status_cache )
}

## update package db: outdated and brandnew packages => status 1 (scheduled for build)
tobuild <- rf_prepare_build(rf, pkg_status)

## or rebuild everything
#tobuild <- rf_prepare_build(rf, pkg_status, rebuild = TRUE)

## set build offline
#rf_set_build_offline(rf)
