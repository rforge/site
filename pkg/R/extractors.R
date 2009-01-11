get_cpu_time_limit <- function(x){
  if(!inherits(x, "R-Forge_control"))
    stop("No R-Forge control object given")
  as.numeric(x$cpu_time_limit)
}
