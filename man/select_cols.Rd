\name{.select_cols}
\alias{.select_cols}
\encoding{UTF-8}
\title{Internal column selection helper}
\description{
  The \code{.select_cols} function is a low-level helper used to resolve column selections. It identifies column names based on character vectors, numeric indices, or predicate functions applied to the data.
}
\usage{
# .select_cols(df, cols)
}
\arguments{
  \item{df}{A data frame or the data subset \code{.SD()} against which predicates are evaluated.}
  \item{cols}{A character vector of names, a numeric vector of indices, or a predicate function (e.g., \code{is.numeric}).}
}
\details{
  This helper is primarily used internally by functions like \code{across()} to handle scoped selections. When a predicate function is provided, it is applied to each column of \code{df} via \code{vapply} to return a logical vector for subsetting.
}
\value{
  A character vector of the selected column names.
}
\examples{
# The .select_cols helper allows for clean, functional 
# column selection based on predicates.

tmp <- m61r(mtcars)
tmp$mutate(all_num = ~lapply(.SD()[, .select_cols(.SD(), is.numeric)], 
                             function(x) x * 10))
tmp$head(3)
}
\keyword{m61r}
\keyword{internal}