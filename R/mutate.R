mutate_ <- function(df,...){
  expr <- list(...)
  res <- list()
  for (i in 1:(length(expr))) {
    res[[i]] <- expression_(df=df,group=NULL,expr[i][[1]])
    if (dim(res[[i]])[1] < dim(df)[1]) {
      res[[i]] <- rep(res[[i]],length.out=dim(df)[1])
    }
  }
  res <- as.data.frame(do.call("cbind",res))
  names(res) <- names(expr)[1:length(expr)]
  row.names(res)<- NULL
  res <- cbind(df,res)
  return(res)
}

transmutate_ <- function(df,...){
  expr <- list(...)
  res <- list()
  for (i in 1:(length(expr))) {
    res[[i]] <- expression_(df=df,group=NULL,expr[i][[1]])
    if (dim(res[[i]])[1] < dim(df)[1]) {
      res[[i]] <- rep(res[[i]],length.out=dim(df)[1])
    }
  }
  res <- as.data.frame(do.call("cbind",res))
  names(res) <- names(expr)[1:length(expr)]
  return(res)
}
