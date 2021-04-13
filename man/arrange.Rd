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
\value{
The functions return an object of the same type as \code{df}.
The output has the following properties:
Properties:
\itemize{
\item Columns are not modified.
\item Output get rows in the order specified by \item{...}.
\item Data frame attributes are preserved.
}
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
