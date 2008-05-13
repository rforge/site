## update package repository
## OLD way:
## When using a binary distribution (Windows, Mac) we don't need to
## install every package from source, we keep a complete local CRAN
## install updated
update_package_library <- function(pkgs, path_to_pkg_src, repository_url, lib, ...){
  ## first update all installed packages if necessary
  update.packages(lib = lib, repos = repository_url, ask = FALSE)

  ## pkg list and dependency structure
  pkgs_dep <- resolve_dependency_structure(pkgs, repository_url, path_to_pkg_src)

  ## install missing packages
  pkgs_installed <- installed.packages(lib = lib)
  ## Temporarily All packages are installed
  ## TODO: install only those packages which are only available from R-Forge, the rest
  ## should be installed from CRAN or other repositories considering the install order
  pkgs_to_install <- setdiff(pkgs_dep[["ALL"]], pkgs_installed)
  if(length(pkgs_to_install) >= 2)
    install.packages(pkgs_to_install, lib = lib, contriburl = contrib.url(repository_url), ...)
}

## this function resolves the dependency structure of source pkgs and returns a list
## containing ALL packages, packages from CRAN and the install order of ALL packages
## Additionally suggested packages are included, as they are probably needed when
## building package vignettes
resolve_dependency_structure <- function(pkgs, repository_url, path_to_pkg_src){
  ## FIXME: commented stuff becomes obsolete soon -> moves to export script
  ## different file separators according to platform
  ##file_separator <- get_file_separator()
  ## build pkg database out of DESCRIPTION files
  ##pkg_db <- tools:::.build_repository_package_db(path_to_pkg_src, unpacked = TRUE)
  ## packages probably not working
  ##pkgs_defunct <- NULL
  ## which packages have bad DESCRIPTION files?
  ##for(pkg in pkgs){
  ##  pkg_DESCRIPTION <- paste(path_to_pkg_src, pkg, "DESCRIPTION", sep = file_separator)
  ##  if(file.exists(pkg_DESCRIPTION)){
  ##    checked <- tools:::.check_package_description(pkg_DESCRIPTION)
  ##    if(length(checked) > 0)
  ##      pkgs_defunct <- c(pkgs_defunct, pkg)
  ##  }
  ##}
  ## remove defunct packages from package db
  ##if(!is.null(pkgs_defunct))
  ##  pkg_db <- pkg_db[!is.element(names(pkg_db), pkgs_defunct)]
   
  ## create PACKAGES in source dir
  ##write.dcf(do.call(rbind, pkg_db), file = paste(path_to_pkg_src, "PACKAGES", sep = file_separator))
 
  ## look out for available packages
  avail_cran <- available.packages(contriburl =
                                   contrib.url(repository_url))
  avail_rforge <- available.packages(contriburl =
                                     sprintf("file:///%s", path_to_pkg_src))
  avail <- rbind(avail_rforge, avail_cran)
  ## What packages do we need from external repository
  pkgs <- pkgs[pkgs %in% rownames(avail_rforge)]
  pkgs_suggested <- resolve_suggests(pkgs, avail)
  pkgs_suggested <- pkgs_suggested[pkgs_suggested %in% rownames(avail)]
  pkgs_to_resolve_deps <- unique(c(pkgs, pkgs_suggested))
  pkgs_all <- resolve_dependencies(pkgs_to_resolve_deps, avail)
  pkgs_cran <- setdiff(pkgs_all, pkgs)
  
  DL <- utils:::.make_dependency_list(pkgs_all, avail)
  pkgs_install_order <- utils:::.find_install_order(pkgs_all, DL)

  ## return a vector with packages and install order
  list(ALL = pkgs_all, CRAN = pkgs_cran, INSTALL_ORDER = pkgs_install_order)
}
