
select_ <- function(df,variable=NULL){
  if (inherits(df,"data.frame")) {
    if (!is.null(variable)) {
        j = select_j(df=df,variable)
        return(value_(df,,j))
    } else return(value_(df))
  } else stop("argument 'df' is not of class data.frame")
}

select_j <- function(df,variable=NULL) {
   if (missing(variable)) {
        j<-TRUE
   } else {
        tmp <- as.list(seq_along(df))
        names(tmp) <- names(df)
        e <- variable[[2]]
        j <- eval(e, tmp)
   }
   return(j)
}
