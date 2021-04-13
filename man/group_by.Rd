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
\value{
The function returns a list.
Each element of the list is a subset of data frame \code{df}. Subset is determined by variables given in \code{group}.
Each data frame get the following properties:
\itemize{
\item Columns are not modified.
\item Only rows corresponding to the subset.
\item Data frame attributes are preserved.
}
}
\examples{
tmp <- group_by_(CO2,~c(Type,Treatment))

tmp[[1]]
}
\keyword{m61r}
