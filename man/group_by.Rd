\name{group_by}
\alias{group_by_}
\encoding{UTF-8}
\title{group_by a data.frame by chosen columns}
\description{
  Group a data.frame by chosen columns
}
\usage{
group_by_(df, group = NULL)
}
\arguments{
\item{df}{data.frame}

\item{group}{formula that describes the group}
}
\details{
  The output of this function is a list.
}
\examples{
group_by_(CO2,~c(Type,Treatment))
}
\keyword{m61r}
