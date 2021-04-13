
setnames <- function (object = name, name) {
    names(object) <- name
    return(object)
}

as_formula <- function(par){
  res <- eval(parse(text=paste0('~',par)))
  return(res)
}

is.numeric_n <- function(x,n) inherits(x,"numeric") && is.atomic(x) && length(x) == n

is.character_n <- function(x,n) inherits(x,"character") && is.atomic(x) && length(x) == n

is.dataframe_n <- function(x,n) inherits(x,"data.frame") && nrow(x) == 1L && ncol(x) == n
