#' @export
join_asof_ <- function(df, df2, by.x, by.y, direction = "backward") {
  if (!inherits(df, "data.frame") || !inherits(df2, "data.frame")) 
    stop("Arguments must be data.frames")
  
  df2 <- df2[order(df2[[by.y]]), , drop = FALSE]
  
  # findInterval is a vectorized C-based function
  # Returns indices i such that y[i] <= x < y[i+1]
  indices <- findInterval(df[[by.x]], df2[[by.y]])
  
  if (direction == "forward") {
    indices <- ifelse(df[[by.x]] == df2[[by.y]][indices], indices, indices + 1)
  }
  valid_mask <- indices > 0 & indices <= nrow(df2)
  
  res_df2 <- df2[indices, , drop = FALSE]
  res_df2[!valid_mask, ] <- NA 
  
  res <- cbind.data.frame(df, res_df2)
  row.names(res) <- NULL
  return(res)
}

#' @export
cut_time <- function(var, breaks_str) {
  call_expr <- substitute(cut(var, breaks = breaks_str), list(var = var, breaks_str = breaks_str))
  return(call_expr)
}

explode_ <- function(df, column) {
  if (!is.list(df[[column]])) stop("The column must be a list.")
    
  lens <- lengths(df[[column]])
  idx <- rep(seq_len(nrow(df)), lens)
  
  res <- df[idx, , drop = FALSE]
  
  res[[column]] <- do.call(c, df[[column]])
  
  row.names(res) <- NULL
  return(res)
}