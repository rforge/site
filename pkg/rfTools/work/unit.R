library(rfTools)

## tests:
rf <- rfTools:::.create_rf_config(db_name = "db_name",
                                  db_user = "rf_table",
                                  db_password = "db_pw",
                                  svn_root = "/srv/svn", tmp_dir = "/tmp")
rf
unclass(rf)

rfTools:::.check_rf_for_sanity(rf)
testfile <- tempfile()

rf_write_configuration( rf, file = testfile )
rf_read <- rf_read_configuration( file = testfile )
stopifnot( identical(rf, rf_read) )

file.remove( testfile )

 svn_reps <- rf_get_svn_repos( rf )

