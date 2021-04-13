gather_ <- function(df,new_col_name = "parameters",new_col_values = "values",pivot){
  if (inherits(df,"data.frame")) {
    pivot_ <- names(df) %in% pivot

    if (is.logical(pivot_)) {
      df_gather <- df[,!pivot_]
    } else stop("pivot does not reflect any columns of df")

    df_core <- df[,pivot_]

    target_gather <- setdiff(names(df),pivot)

    res <- lapply(target_gather,function(x){
      as.data.frame(list(setnames(list(rep(x,length(df_gather[,x])),df_gather[,x]), c(new_col_name,new_col_values))))
    })
    res <- do.call("rbind",res)

    core <- lapply(target_gather,function(x){
      df_core
    })
    core <- do.call("rbind",core)
    res <- cbind(core,res)

    return(res)

  } else stop("argument 'df' is not of class data.frame")
}

spread_ <- function(df,col_name,col_values,pivot){
  if (inherits(df,"data.frame")) {
    pivot_ <- names(df) %in% pivot
    exp <- eval(parse(text=paste0('~',col_name)))
    # exp <- as_formula(col_name)

    tmp <- group_by_(df,exp)

    # build a simple id
    key <- basename(tempfile("id_"))
    tmp <- lapply(tmp,function(x){
      res <- cbind(do.call("paste",c(x[pivot_],sep="_")),x)
      names(res) <- c(key,names(x))
      return(res)
    })

    # index of both core and spread
    index1_ <- c(key,names(df)) %in% c(key,pivot)
    index2_ <- !(c(key,names(df)) %in% pivot)

    ## build df_core
    df_core <- lapply(tmp,function(x){
      x[,index1_]
    })

    df_core <- do.call("rbind",df_core)
    row.names(df_core) <- NULL
    df_core <- unique(df_core)

    ## build df_spread
    df_spread <- tmp[[1]][,index2_]
    names(df_spread)[which(names(df_spread)==col_values)] <- as.character(unique(df_spread[[col_name]]))
    df_spread <- df_spread[,-which(names(df_spread)==col_name)]

    co_df_spread <- setdiff(names(tmp),names(tmp)[1])

    for (i in 1: length(co_df_spread)) {

      df_spread_b <- tmp[[co_df_spread[i]]][,index2_]
      names(df_spread_b)[which(names(df_spread_b)==col_values)] <- as.character(unique(df_spread_b[[col_name]]))
      df_spread_b <- df_spread_b[,-which(names(df_spread_b)==col_name)]

      # df_spread <- full_join_(df_spread,df_spread_b,by.x=key,by.y=key)
      df_spread <- merge(df_spread,df_spread_b,by.x=key,by.y=key,all=TRUE)
    }

    # join core and spread
    #res <- full_join_(df_core,df_spread,by.x=key,by.y=key)
    res <- merge(df_core,df_spread,by.x=key,by.y=key,all=TRUE)
    pivot_key <- c(key,names(df)) %in% key
    return(res[,-which(pivot_key)])

  } else stop("argument 'df' is not of class data.frame")

}
