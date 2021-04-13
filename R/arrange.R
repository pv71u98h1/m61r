arrange_ <- function(df,...){
  if (inherits(df,"data.frame")) {
    expr <- list(...)
    form1 <- paste0(all.vars(expr[[1]]),collapse=",")
    form <- eval(parse(text=paste0('~data.frame(tmp=order(',form1,'))'),keep.source=FALSE))
    i <- expression_(df=df,group=NULL,form)$fun_expr$tmp
    res <- value_(df,i,)
    row.names(res) <- NULL
    attributes(res) <- attributes(df)
    return(res)
  } else stop("argument 'df' is not of class data.frame")
}

desange_ <- function(df,...){
  if (inherits(df,"data.frame")) {
    expr <- list(...)
    form1 <- paste0(all.vars(expr[[1]]),collapse=",")
    form <- eval(parse(text=paste0('~data.frame(tmp=order(',form1,',decreasing = TRUE))'),keep.source=FALSE))
    i <- expression_(df=df,group=NULL,form)$fun_expr$tmp
    res <- value_(df,i,)
    row.names(res) <- NULL
    attributes(res) <- attributes(df)
    return(res)
  } else stop("argument 'df' is not of class data.frame")
}
