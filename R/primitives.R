
###########
# ARRANGE #
###########

#' @export
arrange_ <- function(df,...){
  if (inherits(df,"data.frame")) {
    expr <- list(...)
    sort_cols_names <- all.vars(expr[[1]])
    sort_list <- df[, sort_cols_names, drop = FALSE]

    i <- do.call(order, sort_list)
    
    res <- df[i, , drop=FALSE] # Tri rapide par indexation
    row.names(res) <- NULL
    
    return(res)
  } else stop("argument 'df' is not of class data.frame")
}

#' @export
desange_ <- function(df,...){
  if (inherits(df,"data.frame")) {
    expr <- list(...)
    sort_cols_names <- all.vars(expr[[1]])
    sort_list <- df[, sort_cols_names, drop = FALSE]

    i <- do.call(order, c(sort_list, decreasing = TRUE))

    res <- df[i, , drop=FALSE]
    row.names(res) <- NULL
    
    return(res)
  } else stop("argument 'df' is not of class data.frame")
}

########
# BIND #
########

rbind_ <- function(df, ...) {
  datasets <- list(df, ...)
  check_class <- vapply(datasets, inherits, logical(1), what = "data.frame")
  if (all(check_class)) {
    res <- do.call(rbind.data.frame, datasets)
    row.names(res) <- NULL 
    return(res)
  } else {
    stop("All arguments must be of class 'data.frame'.")
  }
}

cbind_ <- function(df, ...) {
  datasets <- list(df, ...)
  check_class <- vapply(datasets, inherits, logical(1), what = "data.frame")
  if (all(check_class)) {
    res <- do.call(cbind.data.frame, datasets)
    return(res)
  } else {
    stop("All arguments must be of class 'data.frame'.")
  }
}



##########
# FILTER #
##########

filter_i <- function(df, subset) {
  row <- with(df, eval(subset[[2]]))
  
  if (is.logical(row)) {
    row[is.na(row)] <- FALSE 
    return(row)
    
  } else if (is.integer(row) || is.numeric(row)) { 
    return(as.integer(row))
    
  } else {
    stop("'subset' must be either logical or integer/numeric vector of indices.")
  }
}

#' @export
filter_ <- function(df,subset=NULL){
  if (inherits(df,"data.frame")) {
    if (!(missing(subset))) {
      i <- filter_i(df=df,subset)
      res <- df[i, , drop=FALSE] 
    } else {
      res <- df
    }
    row.names(res) <- NULL
    return(res)
  } else stop("argument 'df' is not of class data.frame")
}


########
# JOIN #
########

#' @export
left_join_ <- function(df,df2,by=NULL,by.x=NULL,by.y=NULL){
  if (inherits(df,"data.frame") && inherits(df2,"data.frame")) {
    res <- merge(df, df2, by=by,by.x=by.x,by.y=by.y,all.x=TRUE, sort=FALSE)
    return(res)
  } else stop("either arguments 'df' or/and 'df2' are not of class data.frame")
}

#' @export
right_join_ <- function(df,df2,by=NULL,by.x=NULL,by.y=NULL){
  if (inherits(df,"data.frame") && inherits(df2,"data.frame")) {
    res <-  merge(df, df2, by=by,by.x=by.x,by.y=by.y,all.y=TRUE, sort=FALSE)
    return(res)
  } else stop("either arguments 'df' or/and 'df2' are not of class data.frame")

}

#' @export
inner_join_ <- function(df,df2,by=NULL,by.x=NULL,by.y=NULL){
  if (inherits(df,"data.frame") && inherits(df2,"data.frame")) {
    res <- merge(df, df2, by=by,by.x=by.x,by.y=by.y, sort=FALSE)
    return(res)
  } else stop("either arguments 'df' or/and 'df2' are not of class data.frame")
}

#' @export
full_join_ <- function(df,df2,by=NULL,by.x=NULL,by.y=NULL){
  if (inherits(df,"data.frame") && inherits(df2,"data.frame")) {
    res <- merge(df, df2, by=by,by.x=by.x,by.y=by.y,all=TRUE, sort=FALSE)
    return(res)
  } else stop("either arguments 'df' or/and 'df2' are not of class data.frame")
}

#' @export
semi_join_ <- function(df,df2,by=NULL,by.x=NULL,by.y=NULL){
  if (inherits(df,"data.frame") && inherits(df2,"data.frame")) {

    if (!is.null(by)){
      cols_x <- by
      cols_y <- by
    } else if (!is.null(by.x) && !is.null(by.y)){
      cols_x <- by.x
      cols_y <- by.y
    } else stop("require 'by', or 'by.x' and 'by.y' to be non-null.")
    key_x <- create_key(df, cols_x)
    key_y <- create_key(df2, cols_y)
    match_condition <- key_x %in% key_y
    res <- df[match_condition, , drop=FALSE]
    return(res)
  } else stop("either arguments 'df' or/and 'df2' are not of class data.frame")
}

#' @export
anti_join_ <- function(df,df2,by=NULL,by.x=NULL,by.y=NULL){
  if (inherits(df,"data.frame") && inherits(df2,"data.frame")) {
    if (!is.null(by)){
      cols_x <- by; cols_y <- by
    } else if (!is.null(by.x) && !is.null(by.y)){
      cols_x <- by.x; cols_y <- by.y
    } else stop("require 'by', or 'by.x' and 'by.y' to be non-null.")
    key_x <- create_key(df, cols_x)
    key_y <- create_key(df2, cols_y)
    res <- df[!(key_x %in% key_y), , drop=FALSE] 
    return(res)
  } else stop("either arguments 'df' or/and 'df2' are not of class data.frame")
  
}


##########
# MUTATE #
##########
mutate_runner <- function(df, expr, return_full_df) {
  if (!inherits(df, "data.frame")) stop("argument 'df' is not of class data.frame")
  
  processed_cols <- lapply(seq_along(expr), function(i) {
    e <- expr[[i]]
    nm <- names(expr)[i]
    tmp <- expression_(df = df, group_info = NULL, fun_expr = e)
    val <- tmp$fun_expr
    
    if (is.data.frame(val)) {
      if (!is.null(nm) && nm != "") names(val) <- paste(nm, names(val), sep = ".")
      return(val)
    }
    
    if (is.list(val) && !is.data.frame(val) && length(val) == nrow(df)) {
      res_df <- data.frame(I(val))
      names(res_df) <- nm
      return(res_df)
    }
    
    res_df <- data.frame(val, stringsAsFactors = FALSE)
    if (nrow(res_df) != nrow(df) && nrow(res_df) == 1) {
      res_df <- res_df[rep(1, nrow(df)), , drop = FALSE]
    }
    if (nm != "" && ncol(res_df) == 1) names(res_df) <- nm
    return(res_df)
  })

  new_data <- do.call(cbind.data.frame, processed_cols)
  
  if (return_full_df) {
    update_cols <- intersect(names(df), names(new_data))
    keep_cols <- setdiff(names(df), update_cols)
    
    if (length(update_cols) > 0) {
      res <- df
      res[update_cols] <- new_data[update_cols]
      
      brand_new_cols <- setdiff(names(new_data), update_cols)
      if (length(brand_new_cols) > 0) {
        res <- cbind.data.frame(res, new_data[brand_new_cols])
      }
    } else {
      res <- cbind.data.frame(df, new_data)
    }
    
    row.names(res) <- NULL
    return(res)
  } else {
    row.names(new_data) <- NULL
    return(new_data)
  }
}

#' @export
mutate_ <- function(df,...) { mutate_runner(df, list(...), TRUE) }

#' @export
transmutate_ <- function(df,...) { mutate_runner(df, list(...), FALSE) }


#################
# GATHER/SPREAD #
#################
#' @export
gather_ <- function(df, new_col_name = "parameters", new_col_values = "values", pivot){
  if (inherits(df,"data.frame")) {
    pivot_cols <- intersect(names(df), pivot)
    gather_cols <- setdiff(names(df), pivot_cols)

    df_gather <- df[, gather_cols, drop = FALSE]
    df_core <- df[, pivot_cols, drop = FALSE]
    
    N_rows <- nrow(df); N_gather <- ncol(df_gather)
    
    keys <- rep(gather_cols, each = N_rows)
    values <- unlist(df_gather, use.names = FALSE)
    core_repeated <- df_core[rep(seq_len(N_rows), times = N_gather), , drop = FALSE]
    row.names(core_repeated) <- NULL 

    res <- cbind.data.frame(
      core_repeated,
      setNames(list(keys, values), c(new_col_name, new_col_values))
    )
    
    return(res)
  } else stop("argument 'df' is not of class data.frame")
}

#' @export
spread_ <- function(df, col_name, col_values, pivot){
  if (inherits(df,"data.frame")) {
    
    if (length(pivot) > 1) {
        df$id_row_tmp <- do.call(interaction, c(df[, pivot, drop = FALSE], drop = TRUE))
    } else {
        df$id_row_tmp <- df[[pivot[1]]]
    }
    
    res <- reshape(
        data = df,
        v.names = col_values, 
        idvar = "id_row_tmp",     
        timevar = col_name,   
        direction = "wide"
    )

    new_names <- sub(paste0("^", col_values, "\\."), "", names(res))
    names(res) <- new_names
    
    res$id_row_tmp <- NULL 
    
    return(res)
  } else stop("argument 'df' is not of class data.frame")
}

##########
# SELECT #
##########
#' @export
select_ <- function(df, variable=NULL){
  if (inherits(df,"data.frame")) {
    
    if (!is.null(variable)) {
        j <- select_j(df=df, variable)
        
        res <- df[, j, drop = FALSE]

        new_names <- names(variable)

        if (!is.null(new_names) && length(new_names) == ncol(res)) {
            names(res) <- new_names
        }
        
        return(res)
    } else {
        return(df)
    }
  } else stop("argument 'df' is not of class data.frame")
}

select_j <- function(df,variable=NULL) {
   if (missing(variable)) {
        j<-TRUE
   } else {
        tmp <- as.list(seq_along(df))
        names(tmp) <- names(df)
        e <- variable[[2]]
        j <- eval(e, tmp)
   }
   return(j)
}

#############
# SUMMARISE #
#############
#' @export
summarise_ <- function(df, group_info = NULL, ...) {
  if (!inherits(df, "data.frame")) {
    stop("argument 'df' is not of class data.frame")
  }

  expr <- list(...)
  if (length(expr) == 0) {
    return(if(is.null(group_info)) df[0, ] else group_info$keys)
  }
  
  processed_list <- lapply(seq_along(expr), function(i) {
    e <- expr[[i]]
    res_name <- names(expr)[i]
    
    tmp <- expression_(df = df, group_info = group_info, fun_expr = e)
    val <- tmp$fun_expr 
    
    if (!is.null(group_info)) {
      if (is.list(val) && length(val) > 0 && (is.list(val[[1]]) || !is.null(names(val[[1]])))) {
        group_dfs <- lapply(val, as.data.frame, stringsAsFactors = FALSE)
        res_mat <- do.call(rbind, group_dfs)
        
        if (!is.null(res_name) && res_name != "") {
          names(res_mat) <- paste(res_name, names(res_mat), sep = ".")
        }
        return(res_mat)
      }

      res_vec <- unlist(val)
      res_df <- data.frame(res_vec, stringsAsFactors = FALSE)
      names(res_df) <- if(is.null(res_name) || res_name == "") paste0("V", i) else res_name
      return(res_df)

    } else {
      if (is.list(val) && !is.data.frame(val)) {
        res_df <- as.data.frame(val, stringsAsFactors = FALSE)
        if (!is.null(res_name) && res_name != "") {
            names(res_df) <- if(ncol(res_df) > 1) paste(res_name, names(res_df), sep = ".") else res_name
        }
        return(res_df)
      }
      
      res_df <- data.frame(val, stringsAsFactors = FALSE)
      if (!is.null(res_name) && res_name != "") names(res_df) <- res_name
      return(res_df)
    }
  })
  
  summarized_values <- do.call(cbind.data.frame, processed_list)
  row.names(summarized_values) <- NULL

  if (!is.null(group_info)) {
    res <- cbind.data.frame(group_info$keys, summarized_values)
  } else {
    res <- summarized_values
  }
  
  return(res)
}

#########
# VALUE #
#########
#' @export
value_ <- function(df, i, j){
  if (inherits(df, "data.frame")) {
    
    i_val <- if (!missing(i)) i else seq_len(nrow(df))
    j_val <- if (!missing(j)) j else seq_len(ncol(df))

    if (!missing(i) || !missing(j)) {
      res <- df[i_val, j_val, drop = FALSE]

      if (is.numeric(j_val)) {
        names(res) <- names(df)[j_val]
      }
      
      return(res)
    } else {
      return(df)
    }
  } else stop("argument 'df' is not of class data.frame")
}

#' @export
`modify_<-` <- function(df, i, j, value) {
  if (inherits(df, "data.frame")) {
    
    i_val <- if (!missing(i)) i else seq_len(nrow(df))
    j_val <- if (!missing(j)) j else seq_len(ncol(df))

    df[i_val, j_val] <- value
    
    return(df)
  } else stop("argument 'df' is not of class data.frame")
}

