filter_ <- function(df,subset=NULL){
  if (!is.null(missing(subset))) {
      i = filter_i(df=df,subset)
      return(value_(df,i,))
  } else return(value_(df))

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
