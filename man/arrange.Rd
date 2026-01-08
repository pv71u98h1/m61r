\name{arrange}
\alias{arrange}
\alias{arrange_}
\alias{desange_}
\title{Arrange your data.frames}
\description{Re-arrange your data.frame in ascending or descending order given one or several columns.}
\usage{
# arrange_(df, ...)
# desange_(df, ...)
}
\arguments{
\item{df}{A data.frame.}
\item{...}{A formula used for arranging the data.frame (e.g., \code{~c(col1, col2)}).}
}
\value{
The functions return an object of the same type as \code{df}.
Properties:
\itemize{
  \item Columns are not modified.
  \item Output rows are in the order specified by the formula.
  \item Data frame attributes are preserved.
}
}
\examples{
tmp <- arrange_(CO2, ~c(conc))
head(tmp)

co2 <- m61r(df = CO2)
co2$arrange(~c(conc))
co2$head()
}
\keyword{m61r}