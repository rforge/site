rforge_server_info <- function(){
  structure(list(R_default   = R.Version(),
                 R_dev       = rforge_get_R_version("devel"),
                 R_patched   = rforge_get_R_version("patched"),
                 RForgeTools = packageDescription("RForgeTools"),
                 Server      = Sys.info()), class = "rforge_server_info")
}

## FIXME: What should we do with Windows hosts
rforge_get_R_version <- function(flavor = "patched",
                                 bin = "/srv/R/bin/"){
  grep("^R version", system(file.path(bin, sprintf("R-%s --version", flavor)), intern = TRUE), value = TRUE)
}

print.rforge_server_info <- function(x, ...){
  writeLines( sprintf("RForge server information about host %s",
                     x$Server["nodename"]) )
}
