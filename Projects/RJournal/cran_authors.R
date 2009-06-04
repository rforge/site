x <- .readRDS("packages.rds")

authors <- x[,"Author"]
y <- strsplit(authors, ",| and")
table(unlist(lapply(y, length)))
## around 54 percent of the packages have only 1 Author 
prop.table(table(unlist(lapply(y, length))))
