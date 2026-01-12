\name{join_asof}
\alias{join_asof_}
\encoding{UTF-8}
\title{Join Two Data Frames Based on Nearest Key}
\description{
  Performs an "As-Of" join, matching rows from two data frames where the keys are close but not necessarily equal. This is the primary tool for time-series synchronization, mimicking `Polars` \code{join_asof}.
}
\usage{
# Primitive function
# join_asof_(x, y, by_x, by_y, direction = "backward")

# Within an m61r pipeline
# p$join_asof(y, by_x, by_y, direction = "backward")
}
\arguments{
  \item{x, result_}{The left data frame (primary timeline).}
  \item{y}{The right data frame (reference timeline). Must be sorted by the join key.}
  \item{by_x}{The column name in the left data frame used for joining.}
  \item{by_y}{The column name in the right data frame used for joining.}
  \item{direction}{Direction of the search: \code{"backward"} (default) finds the nearest value \eqn{\le} key; \code{"forward"} finds the nearest value \eqn{\ge} key.}
}
\details{
  The "As-Of" join is fundamentally different from a standard join. It does not look for exact matches but finds the closest record in a reference table. 
  
  \itemize{
    \item \strong{Backward}: Matches the observation in \code{y} that is most recent relative to the time in \code{x} (where \code{y_time <= x_time}).
    \item \strong{Forward}: Matches the next upcoming observation in \code{y} (where \code{y_time >= x_time}).
  }
  
  For maximum speed, \code{m61r} utilizes the \code{findInterval} function, which performs a binary search in C, ensuring that even with millions of rows, the join remains nearly instantaneous.
}
\value{
  A data frame (or updates the \code{m61r} object) containing all columns from \code{x} and the matched columns from \code{y}.
}
\examples{

quotes <- data.frame(
  time = as.POSIXct("2025-01-01 10:00") + c(0, 10, 20),
  price = c(100.1, 100.5, 100.3)
)

trades <- data.frame(
  time = as.POSIXct("2025-01-01 10:00:05"),
  volume = 50
)

# This matches the trade at 10:00:05 with the price at 10:00:00 (100.1)
p <- m61r(trades)
p$join_asof(quotes, by_x = "time", by_y = "time", direction = "backward")

print(p)
}
\keyword{m61r}
\keyword{temporal}

