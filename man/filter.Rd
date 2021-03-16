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
\examples{

tmp <- filter_(CO2,~Plant=="Qn1")
head(tmp)

tmp <- filter_(CO2,~Type=="Quebec")
head(tmp)

}
\keyword{m61r}
