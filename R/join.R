left_join_ <- function(df,df2,by=NULL,by.x=NULL,by.y=NULL){
  if (inherits(df,"data.frame") && inherits(df2,"data.frame")) {
    res <- merge(df, df2, by=by,by.x=by.x,by.y=by.y,all.x=TRUE)
    return(res)
  } else stop("either arguments 'df' or/and 'df2' are not of class data.frame")
}

right_join_ <- function(df,df2,by=NULL,by.x=NULL,by.y=NULL){
  if (inherits(df,"data.frame") && inherits(df2,"data.frame")) {
    res <-  merge(df, df2, by=by,by.x=by.x,by.y=by.y,all.y=TRUE)
    return(res)
  } else stop("either arguments 'df' or/and 'df2' are not of class data.frame")

}

inner_join_ <- function(df,df2,by=NULL,by.x=NULL,by.y=NULL){
  if (inherits(df,"data.frame") && inherits(df2,"data.frame")) {
    res <- merge(df, df2, by=by,by.x=by.x,by.y=by.y)
    return(res)
  } else stop("either arguments 'df' or/and 'df2' are not of class data.frame")
}

full_join_ <- function(df,df2,by=NULL,by.x=NULL,by.y=NULL){
  if (inherits(df,"data.frame") && inherits(df2,"data.frame")) {
    res <- merge(df, df2, by=by,by.x=by.x,by.y=by.y,all=TRUE)
    return(res)
  } else stop("either arguments 'df' or/and 'df2' are not of class data.frame")
}

semi_join_ <- function(df,df2,by=NULL,by.x=NULL,by.y=NULL){
  if (inherits(df,"data.frame") && inherits(df2,"data.frame")) {
    if (!is.null(by)){
      res <- df[df[,by] %in% df2[,by], , drop=FALSE]
    } else if (!is.null(by.x) && !is.null(by.y)){
      res <- df[df[,by.x] %in% df2[,by.y], , drop=FALSE]
    } else stop("require 'by', or 'by.x' and 'by.y' to be non-null.")
    return(res)
  } else stop("either arguments 'df' or/and 'df2' are not of class data.frame")
}

anti_join_ <- function(df,df2,by=NULL,by.x=NULL,by.y=NULL){
  if (inherits(df,"data.frame") && inherits(df2,"data.frame")) {
    if (!is.null(by)){
      res <- df[!df[,by] %in% df2[,by], , drop=FALSE]
    } else if (!is.null(by.x) && !is.null(by.y)){
      res <- df[!df[,by.x] %in% df2[,by.y], , drop=FALSE]
    } else stop("require 'by', or 'by.x' and 'by.y' to be non-null.")
    return(res)
  } else stop("either arguments 'df' or/and 'df2' are not of class data.frame")
}
