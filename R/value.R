
value_ <- function(df,i,j){
  if (inherits(df,"data.frame")) {
    if ((!missing(i)) & (!missing(j))) {
      res <- as.data.frame(df[i,j])
      if (is.numeric(j)) {
         names(res) <- names(df)[j]
      } else if (is.character(j)){
        names(res) <- names(df)[which(names(df) %in% j)]
      }
      return(res)
    } else if ((missing(i)) & (!missing(j))) {
       res <- as.data.frame(df[,j])
       if (is.numeric(j)) {
          names(res) <- names(df)[j]
       } else if (is.character(j)){
         names(res) <- names(df)[which(names(df) %in% j)]
       }
       return(res)
    } else if ((!missing(i)) & (missing(j))) {
      res <- as.data.frame(df[i,])
      attributes(res) <- attributes(df)
      attributes(res)$row.names <- seq(1,nrow(as.data.frame(df[i,])))
      return(res)
    } else if ((missing(i)) & (missing(j))) {
      return(df)
    }
  } else stop("argument 'df' is not of class data.frame")

}

'modify_<-' <- function(df,i,j,value){
  if (inherits(df,"data.frame")) {
    if ((!missing(i)) && (!missing(j))) {
       df[i,j] <- value
    } else if ((missing(i)) && (!missing(j))) {
       df[,j] <- value
    } else if ((!missing(i)) && (missing(j))) {
       df[i,] <- value
    } else if ((missing(i)) && (missing(j))) {
       df <- value
    } else stop("problem with i and j")
    return(df)
  } else stop("argument 'df' is not of class data.frame")


}
