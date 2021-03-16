left_join_ <- function(df,df2,by=NULL,by.x=NULL,by.y=NULL){
  res <- merge(df, df2, by=by,by.x=by.x,by.y=by.y,all.x=TRUE)
  return(res)
}

right_join_ <- function(df,df2,by=NULL,by.x=NULL,by.y=NULL){
  res <-  merge(df, df2, by=by,by.x=by.x,by.y=by.y,all.y=TRUE)
  return(res)
}

inner_join_ <- function(df,df2,by=NULL,by.x=NULL,by.y=NULL){
  res <- merge(df, df2, by=by,by.x=by.x,by.y=by.y)
  return(res)
}

full_join_ <- function(df,df2,by=NULL,by.x=NULL,by.y=NULL){
  res <- merge(df, df2, by=by,by.x=by.x,by.y=by.y,all=TRUE)
  return(res)
}

semi_join_ <- function(df,df2,by=NULL,by.x=NULL,by.y=NULL){
  if (!is.null(by)){
    res <- df[df[,by] %in% df2[,by], , drop=FALSE]
  } else if (!is.null(by.x) && !is.null(by.y)){
    res <- df[df[,by.x] %in% df2[,by.y], , drop=FALSE]
  } else stop("require 'by', or 'by.x' and 'by.y' to be non-null.")
  return(res)
}

anti_join_ <- function(df,df2,by=NULL,by.x=NULL,by.y=NULL){
  if (!is.null(by)){
    res <- df[!df[,by] %in% df2[,by], , drop=FALSE]
  } else if (!is.null(by.x) && !is.null(by.y)){
    res <- df[!df[,by.x] %in% df2[,by.y], , drop=FALSE]
  } else stop("require 'by', or 'by.x' and 'by.y' to be non-null.")
  return(res)
}
