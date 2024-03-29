\name{filter}
\alias{filter_}
\encoding{UTF-8}
\title{filter a data.frame}
\description{
  Filter rows of a data.frame with conditions.
}
\usage{
filter_(df, subset = NULL)
}
\arguments{
\item{df}{data.frame}

\item{subset}{formula that describes the conditions}
}
\value{
The function returns an object of the same type as \code{df}.
Properties:
\itemize{
\item Columns are not modified.
\item Only rows following the condtion determined by \item{subset} appear.
\item Data frame attributes are preserved.
}
}
\examples{

tmp <- filter_(CO2,~Plant=="Qn1")
head(tmp)

tmp <- filter_(CO2,~Type=="Quebec")
head(tmp)

}
\keyword{m61r}
