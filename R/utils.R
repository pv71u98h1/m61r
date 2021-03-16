
setnames <- function (object = name, name) {
    names(object) <- name
    return(object)
}

as_formula <- function(par){
  res <- eval(parse(text=paste0('~',par)))
  return(res)
}
