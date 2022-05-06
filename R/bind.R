rbind_ <- function(df,...){
  if (inherits(df,"data.frame")) {
    datasets <- list(df,...)
    check_class <- lapply(datasets,function(x) inherits(x,"data.frame"))
    check_class <- prod(unlist(check_class))
    if (check_class==1L) {
      res <- do.call("rbind",datasets)
    } else stop("arguments in '...' are not of class data.frame")
    return(res)
  } else stop("argument 'df' is not of class data.frame")
}

cbind_ <- function(df,...){
  if (inherits(df,"data.frame")) {
    datasets <- list(df,...)
    check_class <- lapply(datasets,function(x) inherits(x,"data.frame"))
    check_class <- prod(unlist(check_class))
    if (check_class==1L) {
      res <- do.call("cbind",datasets)
    } else stop("arguments in '...' are not of class data.frame")
    return(res)
  } else stop("argument 'df' is not of class data.frame")
}
