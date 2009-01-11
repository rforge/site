lib <- "/srv/R/pkgs"
pkgs <- dir("/srv/R/pkgs")
pkgs_revisions <- get_package_revision_from_sources(pkgs, lib)

pkgs_versions <- get_package_version_from_sources(pkgs, lib)
