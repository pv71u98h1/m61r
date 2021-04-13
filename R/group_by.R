group_by_ <- function(df,group=NULL){
  if (inherits(df,"data.frame")) {
    if (!is.null(group)){
      j <- group_col_(df,group)
      split_v <- lapply(j,function(x){df[[x]]})
      names(split_v) <- names(df)[j]
      res <- split(df,split_v,drop=TRUE)
      return(res)
    } else {
      return(df)
    }
  } else stop("argument 'df' is not of class data.frame")
}

group_col_ <- function(df,group){
  tmp <- as.list(seq_along(df))
  names(tmp) <- names(df)
  e <- group[[2]]
  j <- eval(e, tmp)
  return(j)
}

group_df_ <- function(df,group){
  j <- group_col_(df,group)
  split_v <- lapply(j,function(x){df[[x]]})
  names(split_v) <- names(df)[j]
  res <- unique(do.call("cbind.data.frame",split_v))
  row.names(res)<- NULL
  return(res)
}
