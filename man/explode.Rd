\name{explode}
\alias{explode}
\encoding{UTF-8}
\title{Flattening a List-column }
\description{
  The \code{explode} method flattens a list-column, creating a new row for every element in the list while duplicating the values of all other columns.
}
\usage{
# Within an m61r object
# tmp$explode(column)
}
\arguments{
  \item{column}{A character string specifying the name of the list-column to be flattened.}
}
\details{
  This operation is particularly useful after creating temporal sequences or ranges using \code{Map()} or \code{seq()}. It transforms "nested" data into a "long" format suitable for standard aggregations. 
  
  Technically, it uses \code{rep()} to replicate row indices based on the length of each list element, ensuring maximum performance for large data frames.
}
\value{
  The function updates the internal data frame of the \code{m61r} object invisibly.
}
\examples{

df <- data.frame(
  id = 1:2,
  tags = I(list(c("A", "B"), c("C", "D", "E")))
)

tmp <- m61r(df)

# This will result in 2 rows for id 1 and 3 rows for id 2
tmp$explode("tags")

tmp

# Time-Series Example
df_time <- data.frame(
  id = 1,
  start = as.POSIXct("2025-01-01 08:00"),
  end   = as.POSIXct("2025-01-01 13:00")
)

tmp <- m61r(df_time)
# Create a sequence of hours
tmp$mutate(hour_slot = ~Map(function(s, e) seq(s, e, by = "hour"), start, end))
# Explode to get one row per hour
tmp$explode("hour_slot")
tmp
}
\keyword{m61r}
\keyword{manipulation}










