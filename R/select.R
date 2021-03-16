
select_ <- function(df,variable=NULL){
  if (!is.null(variable)) {
      j = select_j(df=df,variable)
      return(value_(df,,j))
  } else return(value_(df))
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
