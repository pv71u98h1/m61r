#' @export
read_csv <- function(file, header = TRUE, sep = ",", stringsAsFactors = FALSE, ...) {
  res <- read.table(file = file, header = header, sep = sep, 
                    stringsAsFactors = stringsAsFactors, ...)
  row.names(res) <- NULL
  return(m61r::m61r(res))
}
