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
\examples{
expression_(CO2,fun_expr=~mean(conc))

expression_(CO2,fun_expr=~conc/uptake)

# with group
expression_(CO2,group=~Type,~mean(uptake))

expression_(CO2,group=~c(Type,Treatment),~lm(uptake ~ conc))

}
\keyword{m61r}
