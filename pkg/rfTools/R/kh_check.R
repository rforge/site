## These functions are copied from Kurt Hornik's check.R script

check_results_diff_db <-
function(dir, files = NULL)
{
    if(is.null(files)) {
        ## Assume that both check.csv.prev and check.csv exist in dir.
        files <- file.path(dir, c("check.csv.prev", "check.csv"))
    }

    ## Assume that we know that both check.csv.prev and check.csv exist
    ## in dir.
    x <- read.csv(files[1L], colClasses = "character")
    x <- x[names(x) != "Maintainer"]
    y <- read.csv(files[2L], colClasses = "character")
    y <- y[names(y) != "Maintainer"]
    z <- merge(x, y, by = 1, all = TRUE)
    row.names(z) <- z$Package
    z
}

check_results_diffs <-
function(dir)
{
    db <- check_results_diff_db(dir)
    db <- db[, c("Version.x", "Status.x", "Version.y", "Status.y")]
    ## Show packages with one status missing (removed or added) as
    ## status change only.
    is_na_x <- is.na(db$Status.x)
    is_na_y <- is.na(db$Status.y)
    isc <- (is_na_x | is_na_y | (db$Status.x != db$Status.y))
                                        # Status change.
    ivc <- (!is_na_x & !is_na_y & (db$Version.x != db$Version.y))
                                        # Version change.
    names(db) <- c("V_Old", "S_Old", "V_New", "S_New")
    db <- cbind("S" = ifelse(isc, "*", ""),
                "V" = ifelse(ivc, "*", ""),
                db)
    db[c(which(isc & !ivc), which(isc & ivc), which(!isc & ivc)),
       c("S", "V", "S_Old", "S_New", "V_Old", "V_New")]
}
