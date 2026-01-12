\name{cut_time}
\alias{cut_time}
\encoding{UTF-8}
\title{Binning Date and Time Columns}
\description{
  \code{cut_time} is a helper function designed to generate expressions for binning \code{POSIXct} or \code{Date} columns into specific time intervals. It is primarily intended for use within \code{mutate} calls to create grouping variables for time-series analysis.
}
\usage{
# cut_time(var, breaks_str)
}
\arguments{
  \item{var}{The symbol of the date or time column (e.g., \code{timestamp}).}
  \item{breaks_str}{A character string specifying the time interval (e.g., \code{"hour"}, \code{"day"}, \code{"week"}, \code{"month"}). This is passed directly to the \code{breaks} argument of Base R \code{cut} function.}
}
\details{
  This function uses \code{substitute} to create a symbolic call to \code{base::cut}. When used inside an \code{m61r} pipeline, it allows for high-performance temporal bucketing.
}
\value{
  A language object (call) representing the binning operation, which is evaluated within the context of the data frame.
}
\examples{

df_time <- data.frame(
  timestamp = seq(as.POSIXct("2025-01-01"), by = "15 mins", length.out = 100),
  value = rnorm(100)
)

tmp <- m61r(df_time)


tmp$mutate(day_bin = ~eval(cut_time(timestamp, "day")))

tmp$group_by(~day_bin)
tmp$summarise(daily_avg = ~mean(value))

tmp$head()
}
\keyword{m61r}
\keyword{temporal}
