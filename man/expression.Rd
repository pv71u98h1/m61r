\name{expression}
\alias{expression_}
\encoding{UTF-8}
\title{Formula to be run on a data.frame given a group}
\description{
  Evaluate a formula on the data.frame.
}
\usage{
expression_(df, group=NULL, fun_expr)
}
\arguments{
\item{df}{data.frame}

\item{group}{formula that describes the group}

\item{fun_expr}{formula that describes the expression to be run on the data.frame}
}
\value{
The function returns a list.
Each element of the list get the result of processed expressions determined in \code{...} on the whole data frame \code{df} if \code{group} is kept \code{NULL}, or for each group determined in \code{group} otherwise.
The class of each element is intrinsic to the output of the expression determined in argument \code{...}.
}
\examples{
expression_(CO2,fun_expr=~mean(conc))

expression_(CO2,fun_expr=~conc/uptake)

# with group
expression_(CO2,group=~Type,fun_expr=~mean(uptake))

expression_(CO2,group=~Type,fun_expr=~lm(uptake~conc))

}
\keyword{m61r}
