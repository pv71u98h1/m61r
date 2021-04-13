filter_ <- function(df,subset=NULL){
  if (inherits(df,"data.frame")) {
    if (!(missing(subset))) {
      i = filter_i(df=df,subset)
      res <- value_(df,i,)
    } else {
      res <- df
    }
    return(res)
  } else stop("argument 'df' is not of class data.frame")
}

filter_i <- function(df,subset) {
  row <- with(df, eval(subset[[2]]))
  if (is.logical(row)) {
    row <- (row & !is.na(row))
    return(which(row))
  } else if(is.integer(row)){
    return(row)
  } else stop("'subset' must be either logical or integer")
}
