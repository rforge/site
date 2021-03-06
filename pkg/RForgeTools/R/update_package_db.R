## update package repository
## OLD way:
## When using a binary distribution (Windows, Mac) we don't need to
## install every package from source, we keep a complete local CRAN-
## install updated
update_package_library <- function(pkgs, path_to_pkg_src, repository_url, lib, platform, ...){
  writeLines(sprintf("Updating package library %s ...", lib))
  if((platform == "Linux") | (platform == "MacOSX")){
    ## Start a virtual framebuffer X server and use this for DISPLAY so that
    ## we can run package tcltk and friends.
    pid <- start_virtual_X11_fb()
  }
  ## first update all installed packages if necessary
  ## debug  cat(sprintf("Arguments to update.packages(): lib = %s, repos = %s, ask = FALSE, checkBuilt = TRUE", lib, paste(repository_url, collapse = ", ")))
  update.packages(lib = lib, repos = repository_url, ask = FALSE, checkBuilt = TRUE, ...)
  writeLines("Done.")
  writeLines("Resolve dependency structure ...")
  ## pkg list and dependency structure
  pkgs_dep <- resolve_dependency_structure(pkgs, repository_url, path_to_pkg_src)

  ## install missing packages from standard repositories
  pkgs_installed <- installed.packages(lib = lib)
  ## Temporarily All packages are installed
  ## install those packages which are only available from R-Forge, the rest
  ## should be installed from CRAN or other repositories
  ## TODO: considering the install order
  pkgs_to_install <- setdiff(pkgs_dep[["ALL"]], rownames(pkgs_installed))
  avail_repos <- unique(available.packages(contriburl = contrib.url(repository_url))[,1])
  pkgs_to_install <- pkgs_to_install[pkgs_to_install %in% avail_repos]
  pkgs_to_install_rforge <- setdiff(pkgs_dep[["R_FORGE"]], unique(c(pkgs_to_install, rownames(pkgs_installed))))
  writeLines("Done.")
  if(length(pkgs_to_install)){
    writeLines("Install missing packages from third party repositories ...")
    install.packages(pkgs = as.character(na.omit(pkgs_to_install)), lib = lib, contriburl = contrib.url(repository_url), ...)
    writeLines("Done.")
  }
  ## FIXME: hard coded R-Forge tar.gz source dir
  if(length(pkgs_to_install_rforge)){
    writeLines("Install missing packages from R-Forge ...")
    install.packages(pkgs_to_install_rforge, lib = lib, contriburl = contrib.url("http://R-Forge.R-project.org", type = "source"), type = "source")
    writeLines("Done.")
  }
  if((platform == "Linux") | (platform == "MacOSX")){
    ## Close the virtual framebuffer X server
    close_virtual_X11_fb(pid)
  }
  writeLines("Done.")
}

## this function resolves the dependency structure of source pkgs and returns a list
## containing ALL packages, packages hosted on other repositories and the install order
## of ALL packages.
## Additionally suggested packages are included, as they are probably needed when
## building package vignettes
resolve_dependency_structure <- function(pkgs, repository_url, path_to_pkg_src){
  ## look out for available packages
  avail_repos <- available.packages(contriburl =
                                   contrib.url(repository_url))
  avail_rforge <- available.packages(contriburl = path_to_pkg_src)
  avail <- rbind(avail_rforge, avail_repos)
  ## What packages do we need from external repository
  pkgs <- pkgs[pkgs %in% rownames(avail_rforge)]
  pkgs_suggested <- resolve_suggests(pkgs, avail)
  pkgs_suggested <- pkgs_suggested[pkgs_suggested %in% rownames(avail)]
  pkgs_to_resolve_deps <- unique(c(pkgs, pkgs_suggested))
  pkgs_all <- resolve_dependencies(pkgs_to_resolve_deps, avail)
  pkgs_repos <- setdiff(pkgs_all, pkgs)
  pkgs_rforge <- setdiff(pkgs_all, rownames(avail_repos))
  ## FIXME: what to do with packages hosted on both R-Forge and other repos ...
  DL <- utils:::.make_dependency_list(pkgs_all, avail)
  pkgs_install_order <- utils:::.find_install_order(pkgs_all, DL)

  ## return a vector with packages and install order
  list(ALL = pkgs_all, REPOS = pkgs_repos, R_FORGE = pkgs_rforge, INSTALL_ORDER = pkgs_install_order)
}
