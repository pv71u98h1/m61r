\name{select}
\alias{select_}
\encoding{UTF-8}
\title{select columns of a data.frame}
\description{
  Select columns of a data.frame.
}
\usage{
select_(df, variable = NULL)
}
\arguments{
\item{df}{data.frame}

\item{variable}{formula that describes the selection}
}
\value{
\code{select_()} returns a data frame.
Properties:
\itemize{
\item Only columns following the condtion determined by \item{variable} appear.
\item Rows are not modified.
}
}
\examples{

  tmp <- select_(CO2,~Type)
  head(tmp)

  tmp <- select_(CO2,~c(Plant,Type))
  head(tmp)

  tmp <- select_(CO2,~-Type)
  head(tmp)

  tmp <- select_(CO2,variable=~-(Plant:Treatment))
  head(tmp)
}
\keyword{m61r}
