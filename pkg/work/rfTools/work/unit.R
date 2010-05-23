library(rfTools)

## tests:
rf <- .create_rf_config(db_name = "gforge", db_user = "plugin_rforge", db_password = "jenslf0r2")
rf
unclass(rf)

.check_rf_for_sanity(rf)
testfile <- tempfile()

rf_write_configuration( rf, file = testfile )
rf_read <- rf_read_configuration( file = testfile )
stopifnot( identical(rf, rf_read) )

file.remove( testfile )
