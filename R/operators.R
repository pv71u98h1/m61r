`[.m61r` <- function(x,i,j,...){
  if (missing(i) & missing(j)){
     get("values",x)()
  } else if (missing(i) & !missing(j)){
     get("values",x)(,j)
  } else if (!missing(i) & missing(j)){
     get("values",x)(i,)
  } else if (!missing(i) & !missing(j)){
     get("values",x)(i,j)
  }
}

`[<-.m61r` <- function(x,i,j,value){
  get("modify",x)(i,j,value)
  x
}

print.m61r <- function(x,...){
  res <- get("values",x)()
  print(res)
}

names.m61r <- function(x,...){
  get("process",x)(FUN=names,...)
}

dim.m61r <- function(x,...){
  get("process",x)(FUN=dim,...)
}

as.data.frame.m61r <- function(x,...){
  get("values",x)()
}

rbind.m61r <- function(x, ...) {
  datasets <- lapply(list(x, ...), function(obj) {
    if (inherits(obj, "m61r")) return(obj[]) 
    stop("All arguments in '...' must be of class m61r")
  })  
  res_df <- do.call(rbind_, datasets)  
  return(m61r::m61r(res_df))
}

cbind.m61r <- function(x, ...) {
  datasets <- lapply(list(x, ...), function(obj) {
    if (inherits(obj, "m61r")) return(obj[])
    stop("All arguments in '...' must be of class m61r")
  })  
  res_df <- do.call(cbind_, datasets)
  return(m61r::m61r(res_df))
}



left_join <- function(x, y, ...) {
  UseMethod("left_join")
}

left_join.m61r <- function(x,y,by=NULL,by.x=NULL,by.y=NULL){
  datasets <- list(x,y)
  check_class <- lapply(datasets,function(x) inherits(x,"m61r"))
  check_class <- prod(unlist(check_class))
  if (check_class==1L) {
    res <- left_join_(x[],y[],by=NULL,by.x=NULL,by.y=NULL)
  } else stop("arguments in '...' are not of class m61r")
  return(m61r::m61r(res))
}

right_join <- function(x, y, ...) {
  UseMethod("right_join")
}

right_join.m61r <- function(x,y,by=NULL,by.x=NULL,by.y=NULL){
  datasets <- list(x,y)
  check_class <- lapply(datasets,function(x) inherits(x,"m61r"))
  check_class <- prod(unlist(check_class))
  if (check_class==1L) {
    res <- right_join_(x[],y[],by=by,by.x=by.x,by.y=by.y)
  } else stop("arguments in '...' are not of class m61r")
  return(m61r::m61r(res))
}

inner_join <- function(x, y, ...) {
  UseMethod("inner_join")
}

inner_join.m61r <- function(x,y,by=NULL,by.x=NULL,by.y=NULL){
  datasets <- list(x,y)
  check_class <- lapply(datasets,function(x) inherits(x,"m61r"))
  check_class <- prod(unlist(check_class))
  if (check_class==1L) {
    res <- inner_join_(x[],y[],by=by,by.x=by.x,by.y=by.y)

  } else stop("arguments in '...' are not of class m61r")
  return(m61r::m61r(res))
}

full_join <- function(x, y, ...) {
  UseMethod("full_join")
}

full_join.m61r <- function(x,y,by=NULL,by.x=NULL,by.y=NULL){
  datasets <- list(x,y)
  check_class <- lapply(datasets,function(x) inherits(x,"m61r"))
  check_class <- prod(unlist(check_class))
  if (check_class==1L) {
    res <- full_join_(x[],y[],by=by,by.x=by.x,by.y=by.y)
  } else stop("arguments in '...' are not of class m61r")
  return(m61r::m61r(res))
}

semi_join <- function(x, y, ...) {
  UseMethod("semi_join")
}

semi_join.m61r <- function(x,y,by=NULL,by.x=NULL,by.y=NULL){
  datasets <- list(x,y)
  check_class <- lapply(datasets,function(x) inherits(x,"m61r"))
  check_class <- prod(unlist(check_class))
  if (check_class==1L) {
    res <- semi_join_(x[],y[],by=by,by.x=by.x,by.y=by.y)
  } else stop("arguments in '...' are not of class m61r")
  return(m61r::m61r(res))
}

anti_join <- function(x, y, ...) {
  UseMethod("anti_join")
}

anti_join.m61r <- function(x,y,by=NULL,by.x=NULL,by.y=NULL){
  datasets <- list(x,y)
  check_class <- lapply(datasets,function(x) inherits(x,"m61r"))
  check_class <- prod(unlist(check_class))
  if (check_class==1L) {
    res <- anti_join_(x[],y[],by=by,by.x=by.x,by.y=by.y)
  } else stop("arguments in '...' are not of class m61r")
  return(m61r::m61r(res))
}

