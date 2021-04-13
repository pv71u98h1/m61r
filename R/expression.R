expression_ <- function(df,group=NULL,fun_expr) {
  if (inherits(df,"data.frame")) {
    if (missing(fun_expr)) {
         res <- df
    } else {
      if (is.null(group)){
        e <- fun_expr[[2]]
        res <- with(df, eval(e))
        res <- list(fun_expr=res)
      } else {
       e <- fun_expr[[2]]
       df_tmp <- group_by_(df,group)
       res <- lapply(df_tmp,function(x){with(x, eval(e))})
       res <- list(fun_expr=res)
       row.names(res)<- NULL
      }
    }
    return(res)
  } else stop("argument 'df' is not of class data.frame")

}
