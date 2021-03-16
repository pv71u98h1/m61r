\name{summarise}
\alias{summarise_}
\encoding{UTF-8}
\title{Summarise formula on groups }
\description{
  Summarise of formulas on a data.frame.
}
\usage{
summarise_(df, group = NULL, ...)
}
\arguments{
\item{df}{data.frame}

\item{group}{formula that describes the group}

\item{...}{formulas to be generated}
}

\examples{

  summarise_(CO2,mean=~mean(uptake),sd=~sd(uptake))

  summarise_(CO2, group=~c(Type,Treatment),mean=~mean(uptake),sd=~sd(uptake))

}
\keyword{m61r}
