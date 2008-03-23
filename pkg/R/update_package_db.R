## update package repository
## OLD way:
## When using a binary distribution (Windows, Mac) we don't need to
## install every package from source, we keep a complete local CRAN
## install updated
update_package_library <- function(pkgs, path_to_pkg_src, repository_url, lib, ...){
  ## first update all installed packages if necessary
  update.packages(lib = lib, repos = repository_url, ask = FALSE)
  
  ##source("${R_scripts_dir}/packages.R")
  ##source("${R_scripts_dir}/R_Forge_utils.R")
  ##dir <- file_path_as_absolute(getwd())

  pkgs_dep <- resolve_dependency_structure(pkgs, repository_url, path_to_pkg_src)
  ## install missing packages
  pkgs_installed <- installed.packages(lib = lib)
  ## Temporarily All packages are installed
  ## TODO: install only those packages which are only available from R-Forge, the rest should be installed from R-Forge considering the install order
  pkgs_to_install <- setdiff(pkgs_dep[["ALL"]], pkgs_installed)
  if(length(pkgs_to_install) >= 2)
    install.packages(pkgs_to_install, lib = lib, contriburl = contrib.url(repository_url), ...)
}


resolve_dependency_structure <- function(pkgs, repository_url, path_to_pkg_src){
  ## before creating the source repository we have to check the DESCRIPTION files for validity
  check_description_and_remove_packages_not_passed(pkgs, path_to_pkg_src)
  ## create PACKAGES from R-Forge source dirs
  write_PACKAGES(dir = path_to_pkg_src, type = "source",
                 fields = tools:::.get_standard_repository_db_fields(),
                 unpacked = TRUE) 
  ## look out for available packages
  avail_cran <- available.packages(contriburl =
                                   contrib.url(repository_url))
  avail_try<- try(avail_rforge <- available.packages(contriburl =
                                     sprintf("file:///%s", path_to_pkg_src)))
  if(inherits(avail_try, "try-error"))
    stop("Package DB cannot be properly updated as there are malformed entries
          in one of the DESCRIPTION files!")
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

check_description_and_remove_packages_not_passed <- function(pkgs, path_to_pkg_src){
  path_separator <- c(unix = "/", windows = "\\")
  path_separator <- path_separator[.Platform$OS.type]
  currentR <- getRversion()
  for(pkg in pkgs){
    pkg_DESCRIPTION <- paste(path_to_pkg_src, pkg, "DESCRIPTION", sep = path_separator)
    if(file.exists(pkg_DESCRIPTION)){
      dcf <- read.dcf(pkg_DESCRIPTION)
      R_version_try <- try(.checkRversion(dcf[1,], currentR))
      if(inherits(R_version_try, "try-error")){
        warning(paste("Package", pkg, "has malformed version number in DESCRIPTION file -> removing!"))
        system(paste("rm -rf", paste(path_to_pkg_src, pkg, sep = path_separator)))
      }
    }
  }
}

## extracted from available.packages for checking desription files as we don't know if
## some packages have valid DESCRIPTION files or not apriori. This possibly can break
## the build process if there are invalid version numbers.

.checkRversion <- function(x, currentR) {
  if (is.na(xx <- x["Depends"]))
    return(TRUE)
  xx <- tools:::.split_dependencies(xx)
  if (length(z <- xx[["R", exact = TRUE]]) > 1)
    eval(parse(text = paste("currentR", z$op, "z$version")))
  else TRUE
}
