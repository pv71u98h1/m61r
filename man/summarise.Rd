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
\value{
\code{summarise_()} returns a data frame.
If argument \code{group} is not \code{NULL}, output get its first columns called according to the names present in argument \code{group}.
The following columns are called according to the name of each argument present in \code{...}.
Each row corresponds to processed expressions determined in \code{...} for each group determined in \code{group}, or over the whole data frame if \code{group} is \code{NULL}.
}
\examples{

  summarise_(CO2,a=~mean(uptake),b=~sd(uptake))

  summarise_(CO2, group=~c(Type,Treatment),a=~mean(uptake),b=~sd(uptake))

}
\keyword{m61r}
