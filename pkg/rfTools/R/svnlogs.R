read_svn_verbose_xml_log <-
function(file)
{
    require("XML")
    x <- XML::xmlChildren(XML::xmlRoot(XML::xmlTreeParse(file)))
    y <- lapply(x,
                function(e) {
                    nms <- names(e)
                    author <- if("author" %in% nms)
                        XML::xmlValue(e[["author"]])
                    else
                        NA_character_
                    date <- XML::xmlValue(e[["date"]])
                    msg <- NULL
                    if("msg" %in% nms)
                        msg <- XML::xmlValue(e[["msg"]])
                    if(!length(msg))
                        msg <- ""
                    paths <- NULL
                    if("paths" %in% nms) {
                        paths <- e[["paths"]]
                        paths <-
                            cbind(do.call(rbind,
                                          XML::xmlApply(paths, xmlValue)),
                                  do.call(rbind,
                                          XML::xmlApply(paths, xmlAttrs)))
                    }
                    if(length(paths)) {
                        ## Could also contain copyfrom-path and
                        ## copyfrom-rev ...
                        rownames(paths) <- NULL
                        colnames(paths)[1L] <- "path"
                        paths <- paths[, c("path", "kind", "action"),
                                       drop = FALSE]
                    } else {
                        paths <- matrix(character(), 0L, 3L,
                                        dimnames =
                                        list(NULL,
                                             c("path", "kind", "action")))
                    }
                    list(author = author,
                         date = date,
                         msg = msg,
                         paths = paths)
                })
    z <- data.frame(revision = sapply(x, XML::xmlAttrs),
                    author = unlist(lapply(y, `[[`, "author")),
                    date =
                    as.POSIXlt(unlist(lapply(y, `[[`, "date")),
                               format = "%FT%T"),
                    msg = unlist(lapply(y, `[[`, "msg")),
                    stringsAsFactors = FALSE)
    z$paths <- lapply(y, `[[`, "paths")
    z
}
