available.packages2 <-
function (contriburl = contrib.url(getOption("repos")), method, 
    fields = NULL) 
{
    requiredFields <- tools:::.get_standard_repository_db_fields()
    if (is.null(fields)) 
        fields <- requiredFields
    else {
        stopifnot(is.character(fields))
        fields <- unique(c(requiredFields, fields))
    }
    res <- matrix(NA_character_, 0L, length(fields) + 1L, dimnames = list(NULL, 
        c(fields, "Repository")))
    for (repos in contriburl) {
        localcran <- length(grep("^file:", repos)) > 0L
        if (localcran) {
            if (substring(repos, 1L, 8L) == "file:///") {
                tmpf <- paste(substring(repos, 8L), "PACKAGES", 
                  sep = "/")
                if (.Platform$OS.type == "windows") {
                  if (length(grep("^/[A-Za-z]:", tmpf))) 
                    tmpf <- substring(tmpf, 2L)
                }
            }
            else {
                tmpf <- paste(substring(repos, 6L), "PACKAGES", 
                  sep = "/")
            }
            res0 <- read.dcf(file = tmpf)
            if (length(res0)) 
                rownames(res0) <- res0[, "Package"]
        }
        else {
            dest <- file.path(tempdir(), paste("repos_", URLencode(repos, 
                TRUE), ".rds", sep = ""))
            if (file.exists(dest)) {
                res0 <- .readRDS(dest)
            }
            else {
                tmpf <- tempfile()
                on.exit(unlink(tmpf))
                op <- options("warn")
                options(warn = -1)
                z <- try(download.file(url = paste(repos, "PACKAGES.gz", 
                  sep = "/"), destfile = tmpf, method = method, 
                  cacheOK = FALSE, quiet = TRUE, mode = "wb"), 
                  silent = TRUE)
                if (inherits(z, "try-error")) {
                  z <- try(download.file(url = paste(repos, "PACKAGES", 
                    sep = "/"), destfile = tmpf, method = method, 
                    cacheOK = FALSE, quiet = TRUE, mode = "wb"), 
                    silent = TRUE)
                }
                options(op)
                if (inherits(z, "try-error")) {
                  warning(gettextf("unable to access index for repository %s", 
                    repos), call. = FALSE, immediate. = TRUE, 
                    domain = NA)
                  next
                }
                res0 <- read.dcf(file = tmpf)
                if (length(res0)) 
                  rownames(res0) <- res0[, "Package"]
                .saveRDS(res0, dest, compress = TRUE)
                unlink(tmpf)
                on.exit()
            }
        }
        if (length(res0)) {
            missingFields <- fields[!(fields %in% colnames(res0))]
            if (length(missingFields)) {
                toadd <- matrix(NA_character_, nrow = nrow(res0), 
                  ncol = length(missingFields), dimnames = list(NULL, 
                    missingFields))
                res0 <- cbind(res0, toadd)
            }
            if ("Path" %in% colnames(res0)) {
                rp <- rep.int(repos, nrow(res0))
                path <- res0[, "Path"]
                rp[!is.na(path)] <- paste(repos, path[!is.na(path)], 
                  sep = "/")
            }
            else rp <- repos
            res0 <- cbind(res0[, fields, drop = FALSE], Repository = rp)
            res <- rbind(res, res0)
        }
    }
    if (length(res)) {
        currentR <- getRversion()
        .checkRversion <- function(x) {
            if (is.na(xx <- x["Depends"])) 
                return(TRUE)
            xx <- tools:::.split_dependencies(xx)
            zs <- xx[names(xx) == "R"]
            r <- TRUE
            for (z in zs) if (length(z) > 1L) 
                r <- r & eval(parse(text = paste("currentR", 
                  z$op, "z$version")))
            r
        }
        res <- res[apply(res, 1L, .checkRversion), , drop = FALSE]
    }
    ## removed OStype check
    res
}
