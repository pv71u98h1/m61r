\name{case_when}
\alias{case_when}
\encoding{UTF-8}
\title{Logic within a dataframe}
\description{
  The \code{case_when} function provides a vectorised approach to multiple \code{if_else} conditions in a readable and efficient way. It evaluates conditions sequentially and assigns values as soon as a condition is satisfied.
}
\usage{
# case_when(...)
}
\arguments{
  \item{...}{A sequence of condition/value pairs, ending with a default value (\code{default}). 
  Format: \code{condition1, value1, condition2, value2, ..., default}.}
}
\details{
  This function is optimized for use inside \code{mutate_} or \code{transmutate_} methods.   
  The final argument acts as the fallback value (the "otherwise" branch) if all preceding conditions evaluate to \code{FALSE}.
}
\value{
  An atomic vector of the same length as the input conditions. The output type (e.g., character, numeric) is determined by the types of the values provided.
}
\examples{
# Independent usage
x <- 1:10
res <- case_when(
  x <= 3, "Small",
  x <= 7, "Medium",
  "Large" # Default/Otherwise value
)

# Usage with an m61r pipeline
tmp <- m61r(mtcars)
tmp$mutate(
  efficiency_cat = ~case_when(
    mpg > 25, "Economical",
    mpg > 15, "Standard",
    "High Consumption"
  )
)
tmp
}
\keyword{m61r}
\keyword{logic}
