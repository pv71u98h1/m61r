#' @export
expression_ <- function(df, group_info = NULL, fun_expr, envir = parent.frame()) {
  if (!inherits(df, "data.frame")) stop("df must be a data.frame")
  
  e <- fun_expr[[2]]
  
  if (is.null(group_info)) {
    eval_env <- list2env(as.list(df), parent = envir)
    eval_env$.SD <- function() df
    res <- eval(e, eval_env)
    return(list(fun_expr = res))
  } else {
    res <- lapply(group_info$indices, function(i) {
      group_view <- df[i, , drop = FALSE]
      eval_env <- list2env(as.list(group_view), parent = envir)
      eval_env$.SD <- function() group_view
      eval(e, eval_env)
    })
    return(list(fun_expr = res))
  }
}


#########
# GROUP #
#########
#' @export
get_group_indices_ <- function(df, group = NULL) {
  if (!inherits(df, "data.frame")) stop("argument 'df' is not of class data.frame")
  if (is.null(group)) return(NULL)

  group_vars <- all.vars(group)
  
  if (length(group_vars) > 0) {
    group_list <- as.list(df[, group_vars, drop = FALSE])
  } else {
    group_list <- list(with(df, eval(group[[2]])))
  }

  group_factor <- do.call(interaction, c(group_list, drop = TRUE))

  group_indices <- split(seq_len(nrow(df)), f = group_factor)  
  
  first_indices <- vapply(group_indices, `[`, 1L, FUN.VALUE = integer(1))
  group_keys <- df[first_indices, group_vars, drop = FALSE]
  row.names(group_keys) <- NULL

  return(list(
    group_cols = group_vars,
    indices    = group_indices,
    keys       = group_keys 
  ))
}

group_col_ <- function(df,group){
  tmp <- as.list(seq_along(df))
  names(tmp) <- names(df)
  e <- group[[2]]
  j <- eval(e, tmp)
  return(j)
}

#############
# CASE WHEN #
#############

#' @export
case_when <- function(...) {
  args <- list(...)
  n_args <- length(args)
  
  if (n_args < 1) return(NULL)  
  default_val <- args[[n_args]]

  if (n_args == 1) return(default_val)
  
  n_rows <- length(args[[1]])
  res <- rep(default_val, length.out = n_rows)
  
  for (i in seq(n_args - 2, 1, by = -2)) {
    condition <- args[[i]]
    value <- args[[i+1]]
    res[condition] <- value
  }
  
  return(res)
}

#' @export
create_key <- function(df, cols) {
    if (length(cols) == 1) {
        return(df[[cols]]) 
    }
    df_factor <- as.data.frame(lapply(df[, cols, drop = FALSE], as.factor))
    return(do.call(interaction, c(df_factor, drop = TRUE))) 
}

#' @export
.select_cols <- function(df, selection) {
  if (is.function(selection)) {
    return(names(df)[vapply(df, selection, logical(1))])
  } else if (is.character(selection)) {
    return(selection)
  }
  stop("Selection must be a predicate function or a character vector of names.")
}

#' @export
across <- function(cols, FUN, ...) {
  df_subset <- get(".SD", envir = parent.frame())()
  
  if (is.function(cols)) {
    target_cols <- names(df_subset)[vapply(df_subset, cols, logical(1))]
  } else {
    target_cols <- names(df_subset[ , cols, drop = FALSE])
  }

  res <- lapply(df_subset[, target_cols, drop = FALSE], FUN, ...)
  return(res)
}

setnames <- function (object = name, name) {
    names(object) <- name
    return(object)
}

as_formula <- function(par){
  res <- eval(parse(text=paste0('~',par)))
  return(res)
}

is.numeric_n <- function(x,n) is.numeric(x) && length(x) == n

is.character_n <- function(x,n) inherits(x,"character") && is.atomic(x) && length(x) == n

is.dataframe_n <- function(x,n) inherits(x,"data.frame") && nrow(x) == 1L && ncol(x) == n
