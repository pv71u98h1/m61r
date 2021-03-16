\name{arrange}
\alias{arrange_}
\alias{desange_}
\title{Arrange your data.frames}
\description{
  Re-arrange your data.frame in ascending or descending order and given one or several columns.
}
\usage{
arrange_(df, ...)

desange_(df, ...)
}
\arguments{
\item{df}{data.frame}
\item{...}{formula used for arranging the data.frame}
}
\examples{

tmp <- arrange_(CO2,~c(conc))
head(tmp)

tmp <- arrange_(CO2,~c(Treatment,conc,uptake))
head(tmp)

tmp <- desange_(CO2,~c(Treatment,conc,uptake))
head(tmp)

}
\keyword{m61r}
