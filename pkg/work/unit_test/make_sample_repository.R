source_dir <- "/srv/R/pkgs"
source_pkgs <- dir(source_dir)
target_source_dir <- "pkgs"
repository_root <- "/srv/R/R-Forge"
target_repository_root <- "R-Forge"
avail_tarballs <- available.packages(contrib.url(sprintf("file://%s", repository_root), type = "source"))

pkgs <- source_pkgs[source_pkgs %in% avail_tarballs[, 1]]

## Copy subset of packages

sampled <- sample(pkgs, 10)

for(pkg in sampled){
  ## unpacked sources first
  file.copy(file.path(source_dir, pkg), target_source_dir, recursive = TRUE)
  ## then tarballs
  file.copy(file.path(contrib.url(repository_root), sprintf("%s_%s.tar.gz", pkg, avail_tarballs[pkg, "Version"])), contrib.url(target_repository_root))
}

## Write PACKAGES

## this part is taken from rforge_prepare_packages script
pkg_db <- tools:::.build_repository_package_db(target_source_dir, unpacked = TRUE, fields = c("Repository", "Repository/R-Forge/Project", "Repository/R-Forge/Revision", "Date/Publication"))
## which packages have bad DESCRIPTION files?
pkgs_defunct <- NULL
for(pkg in names(pkg_db)){
 pkg_DESCRIPTION <- file.path(target_source_dir, pkg, "DESCRIPTION")
 if(file.exists(pkg_DESCRIPTION)){
  checked <- tryCatch(tools:::.check_package_description(pkg_DESCRIPTION), error=function(e) 1)
  if(length(checked) > 0)
    pkgs_defunct <- c(pkgs_defunct, pkg)
 }
}
## remove defunct packages from package db
## TODO: show that pkg is not working on R-forge( give hint: wrong DESCRIPTION file)
if(!is.null(pkgs_defunct)){
 writeLines(paste("The following packages did not pass the check for valid DESCRIPTION:", pkgs_defunct))
 pkg_db <- pkg_db[!is.element(names(pkg_db), pkgs_defunct)]
}
write.dcf(do.call(rbind, pkg_db), file = file.path(target_source_dir, "PACKAGES"))

## taken from RForgeTools finalization.R
fields <- c(tools:::.get_standard_repository_db_fields(), "Repository", "Repository/Project", "Repository/R-Forge/Revision")
tools:::write_PACKAGES(dir = contrib.url(target_repository_root), fields = fields, type = "source")
