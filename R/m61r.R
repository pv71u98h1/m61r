m61r <- function(df=NULL){

  # private
  df_ <- df
  group_ <- NULL
  result_ <- df

  # public
  object <- local({


    ##########
    # filter #
    ##########
    filter <- function(subset) {
      result_ <<- filter_(df=result_,subset=subset)
      invisible()
    }

    ##########
    # select #
    ##########
    select <- function(variable) {
      result_ <<- select_(df=result_,variable=variable)
      invisible()
    }

    ############
    # group_by #
    ############
    group_by <- function(group){
      group_ <<- group
      invisible()
    }

    ######################
    # mutate/transmutate #
    ######################

    # mutate
    mutate <- function(...){
      on.exit(group_ <<- NULL,add=TRUE)
      result_ <<- mutate_(result_,...)
      invisible()
    }

    # transmutate
    transmutate <- function(...){
      on.exit(group_ <<- NULL,add=TRUE)
      result_ <<- transmutate_(result_,...)
      invisible()
    }

    #############
    # summarise #
    #############
    summarise <- function(...){
      on.exit(group_ <<- NULL,add=TRUE)
      result_ <<- summarise_(result_,group_,...)
      invisible()

    }

    ###################
    # arrange/descent #
    ###################

    # arrange
    arrange <- function(...){
      on.exit(group_ <<- NULL,add=TRUE)
      result_ <<- arrange_(result_,...)
      invisible()
    }

    # desange
    desange <- function(...){
      on.exit(group_ <<- NULL,add=TRUE)
      result_ <<- desange_(result_,...)
      invisible()
    }

    ########
    # join #
    ########

    # left_join
    left_join <- function(y,by=NULL,by.x=NULL,by.y=NULL){
      on.exit(group_ <<- NULL,add=TRUE)
      result_ <<- left_join_(result_, y, by=by,by.x=by.x,by.y=by.y)
      invisible()
    }

    # right_join
    right_join <- function(y,by=NULL,by.x=NULL,by.y=NULL){
      on.exit(group_ <<- NULL,add=TRUE)
      result_ <<- right_join_(result_, y, by=by,by.x=by.x,by.y=by.y)
      invisible()
    }

    # inner_join
    inner_join <- function(y,by=NULL,by.x=NULL,by.y=NULL){
      on.exit(group_ <<- NULL,add=TRUE)
      result_ <<- inner_join_(result_, y, by=by,by.x=by.x,by.y=by.y)
      invisible()
    }

    # full_join
    full_join <- function(y,by=NULL,by.x=NULL,by.y=NULL){
      on.exit(group_ <<- NULL,add=TRUE)
      result_ <<- full_join_(result_, y, by=by,by.x=by.x,by.y=by.y)
      invisible()
    }

    # semi_join
    semi_join <- function(y,by=NULL,by.x=NULL,by.y=NULL){
      on.exit(group_ <<- NULL,add=TRUE)
      result_ <<- semi_join_(result_, y, by=by,by.x=by.x,by.y=by.y)
      invisible()
    }

    # anti_join
    anti_join <- function(y,by=NULL,by.x=NULL,by.y=NULL){
      on.exit(group_ <<- NULL,add=TRUE)
      result_ <<- anti_join_(result_, y, by=by,by.x=by.x,by.y=by.y)
      invisible()
    }

    ###########
    # reshape #
    ###########
    gather <- function(new_col_name = "parameters",new_col_values = "values",pivot=NULL){
      on.exit(group_ <<- NULL,add=TRUE)
      result_ <<- gather_(result_, new_col_name = new_col_name,new_col_values = new_col_values,pivot=pivot)
      invisible()
    }

    spread <- function(col_name,col_values,pivot=NULL){
      on.exit(group_ <<- NULL,add=TRUE)
      result_ <<- spread_(result_, col_name=col_name,col_values=col_values,pivot=pivot)
      invisible()
    }

    ##########
    # values #
    ##########

    # values
    values <- function(i,j) {
      on.exit(group_ <<- NULL,add=TRUE)
      on.exit(result_ <<- df_,add=TRUE)

      if (missing(i) & missing(j)){
         return(value_(result_,,))
      } else if (missing(i) & !missing(j)){
         return(value_(result_,,j))
      } else if (!missing(i) & missing(j)){
         return(value_(result_,i,))
      } else if (!missing(i) & !missing(j)){
         return(value_(result_,i,j))
      }

	  }

    # modify
    modify <- function(i,j,value) {
      on.exit(group_ <<- NULL,add=TRUE)
      modify_(df=result_,i,j) <- value
      result_ <<- result_
      invisible()
    }

    ###############
    # CLONING     #
    ###############
    clone <- function(){
      res <- m61r()
      tmp <- mget(ls(parent.env(object)),parent.env(object))
      lapply(names(tmp)[which(!names(tmp)%in%"object")],function(x){assign(x,tmp[[x]],parent.env(res))})
      return(res)
    }

    ################
    # PROCESS      #
    ################

    process <- function(FUN,...){
      on.exit(group_ <<- NULL,add=TRUE)
      on.exit(result_ <<- df_,add=TRUE)
      return(FUN(result_,...))
    }


    environment()
    })
    lockEnvironment(object, TRUE)
    structure(object, class=c("m61r", class(object)))
}
