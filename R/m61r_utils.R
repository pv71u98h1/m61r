
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
