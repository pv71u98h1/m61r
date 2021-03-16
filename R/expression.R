expression_ <- function(df,group=NULL,fun_expr) {
   if (missing(fun_expr)) {
        res <- df
   } else {
     if (is.null(group)){
       e <- fun_expr[[2]]
       res <- with(df, eval(e))
       res <- data.frame(fun_expr=res)
     } else {
      e <- fun_expr[[2]]
      df_tmp <- group_by_(df,group)
      res <- lapply(df_tmp,function(x){with(x, eval(e))})
      res <- do.call("rbind",res)
      res <- data.frame(fun_expr=res)
      row.names(res)<- NULL
     }
   }
   return(res)
}
