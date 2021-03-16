summarise_ <- function(df,group=NULL,...){

  expr <- list(...)

  if (!is.null(group)){
    group_df_tmp <- group_df_(df,group)

    res <- list()
    for (i in 1:(length(expr))) {
      res[[i]] <- expression_(df=df,group=group,expr[i][[1]])
    }
    res <- do.call("cbind",res)
    names(res) <- names(expr)[1:length(expr)]
    res <- cbind(group_df_tmp,res)
    row.names(res)<- NULL

  }  else {
    res <- list()
    for (i in 1:(length(expr))) {
      res[[i]] <- expression_(df=df,group=NULL,expr[i][[1]])
    }
    res <- as.data.frame(do.call("cbind",res))
    names(res) <- names(expr)[1:length(expr)]
    row.names(res)<- NULL
  }

  return(res)

}
