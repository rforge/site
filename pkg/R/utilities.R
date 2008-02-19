## Various utilities

## Function definition of version ordering functions
## decompose given version number (character) in numeric major and minor part
decompose_version <- function(x){
  splitted <- as.numeric(unlist(strsplit(x, "-")))
  if(length(splitted)==1)
     return(c(major=splitted,minor=NA))
  c(major=splitted[1],minor=splitted[2])
}

## given two version number this function returns the order of them
version_order<-function(x){
  if(!is.character(x))
    stop("A version number has to be of type character!")
  if(!length(x)==2)
    warning("More than 2 version numbers detexted: The order is calculated only from the first 2 elements in the vector!")
  x_decomposed <- decompose_version(x[1])
  y_decomposed <- decompose_version(x[2])
  if(x_decomposed["major"] != y_decomposed["major"])
    return(order(c(x_decomposed["major"],y_decomposed["major"])))
  order(c(x_decomposed["minor"],y_decomposed["minor"]))
}
