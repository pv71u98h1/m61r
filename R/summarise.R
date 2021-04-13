summarise_ <- function(df,group=NULL,...){
  if (inherits(df,"data.frame")) {
    expr <- list(...)

    if (!is.null(group)){
      group_df_tmp <- group_df_(df,group)

      res <- list()
      for (i in 1:(length(expr))) {
         tmp <- expression_(df=df,group=group,expr[i][[1]])

         if (prod(unlist(lapply(tmp$fun_expr,function(x)is.numeric_n(x,1L))))==1L) {

            res[[i]] <-do.call("rbind",tmp$fun_expr)

         } else if (prod(unlist(lapply(tmp$fun_expr,function(x)is.character_n(x,1L))))==1L) {

            res[[i]] <-do.call("rbind",tmp$fun_expr)

         } else if (prod(unlist(lapply(tmp$fun_expr,function(x)is.dataframe_n(x,1L))))==1L){

            tmp2 <- lapply(tmp$fun_expr,function(x){names(x)<-"zzz";x})
            res[[i]] <-do.call("rbind",tmp2)

         } else {
            warning(paste0("expression ",as.character(expr[i][[1]][2])," got a result with a dimension higher than [1,1]. Summarise_ returns 'NA'" ))
            res[[i]] <- data.frame(rep(NA,nrow(group_df_tmp)))
         }
      }
      res <- as.data.frame(do.call("cbind",res))
      names(res) <- names(expr)[1:length(expr)]
      res <- cbind(group_df_tmp,res)
      row.names(res)<- NULL

    }  else {
      res <- list()
      for (i in 1:(length(expr))) {
         tmp <- expression_(df=df,group=NULL,expr[i][[1]])

         if (is.numeric_n(tmp$fun_expr,1L)) {

            res[[i]] <-as.numeric(tmp$fun_expr)

         } else if (is.character_n(tmp$fun_expr,1L)) {

            res[[i]] <-tmp$fun_expr

         } else if (is.dataframe_n(tmp$fun_expr,1L)){

            res[[i]] <-tmp$fun_expr[1,1]

         } else {
            warning(paste0("expression ",as.character(expr[i][[1]][2])," got a result with a dimension higher than [1,1]. Summarise_ returns 'NA'" ))
            res[[i]] <- NA
         }
      }
      res <- as.data.frame(do.call("cbind",res))
      names(res) <- names(expr)[1:length(expr)]
      row.names(res)<- NULL
    }

    return(res)

  } else stop("argument 'df' is not of class data.frame")


}
