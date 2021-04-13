mutate_ <- function(df,...){
  if (inherits(df,"data.frame")) {
    expr <- list(...)
    res <- list()
    for (i in 1:(length(expr))) {
      tmp <- expression_(df=df,group=NULL,expr[i][[1]])

      if (is.numeric_n(tmp$fun_expr,nrow(df))) {

         res[[i]] <-tmp$fun_expr

      } else if (is.character_n(tmp$fun_expr,nrow(df))) {

         res[[i]] <-tmp$fun_expr

      } else if (is.dataframe_n(tmp$fun_expr,nrow(df))) {

         res[[i]] <-tmp$fun_expr

      } else {
         warning(paste0("expression ",as.character(expr[i][[1]][2])," got a result with 'nrow' different from 'df'. Mutate_ returns 'NA'" ))
         res[[i]] <- rep(NA,nrow(df))
      }
    }
    res <- as.data.frame(do.call("cbind",res))
    names(res) <- names(expr)[1:length(expr)]
    row.names(res)<- NULL
    res <- cbind(df,res)
    return(res)
  } else stop("argument 'df' is not of class data.frame")

}

transmutate_ <- function(df,...){
  if (inherits(df,"data.frame")) {
    expr <- list(...)
    res <- list()
    for (i in 1:(length(expr))) {
      tmp <- expression_(df=df,group=NULL,expr[i][[1]])

      if (is.numeric_n(tmp$fun_expr,nrow(df))) {

         res[[i]] <-tmp$fun_expr

      } else if (is.character_n(tmp$fun_expr,nrow(df))) {

         res[[i]] <-tmp$fun_expr

      } else if (is.dataframe_n(tmp$fun_expr,nrow(df))) {

         res[[i]] <-tmp$fun_expr

      } else {
         warning(paste0("expression ",as.character(expr[i][[1]][2])," got a result with 'nrow' different from 'df'. Mutate_ returns 'NA'" ))
         res[[i]] <- rep(NA,nrow(df))
      }
    }
    res <- as.data.frame(do.call("cbind",res))
    names(res) <- names(expr)[1:length(expr)]
    return(res)
  } else stop("argument 'df' is not of class data.frame")

}
