\name{across}
\alias{across}
\encoding{UTF-8}
\title{Apply a function across multiple columns}
\description{
  The \code{across} function allows you to apply the same transformation or aggregation to multiple columns simultaneously. It is designed to be used within \code{summarise_}, \code{mutate_}, or \code{transmutate_} methods.
}
\usage{
# across(cols, FUN, ...)
}
\arguments{
  \item{cols}{A character vector of column names, a numeric vector of column indices, or a predicate function (e.g., \code{is.numeric}) to select columns.}
  \item{FUN}{A function to be applied to each of the selected columns.}
  \item{...}{Additional arguments passed to the function \code{FUN}.}
}
\details{
  This function provides a concise way to perform operations on multiple columns at once. It internally accesses the data subset (\code{.SD}) of the current group or data frame. If \code{cols} is a function, it acts as a filter to select all columns for which the function returns \code{TRUE}.
}
\value{
  A list where each element represents the result of \code{FUN} applied to a selected column. When used within \code{summarise_}, this list is automatically flattened into separate columns.
}
\examples{
# Usage within an m61r pipeline for aggregation
p <- m61r(mtcars)
p$summarise(
  avg = ~across(c("mpg", "disp", "hp"), mean)
)
p[]

# Usage with a predicate function to select numeric columns
p <- m61r(iris)
p$summarise(
  stats = ~across(is.numeric, sd, na.rm = TRUE)
)
p[]
}
\keyword{m61r}
\keyword{transformation}