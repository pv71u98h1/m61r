\name{mutate}
\alias{mutate_}
\alias{transmutate_}
\encoding{UTF-8}
\title{Mutate and transmutate a data.frame}
\description{
  Mutate and transmutate a data.frame.
}
\usage{
mutate_(df, ...)

transmutate_(df, ...)
}
\arguments{
\item{df}{data.frame}

\item{...}{formula used for mutating/transmutating the data.frame}
}
\examples{
tmp <- mutate_(CO2,z=~conc/uptake)
head(tmp)

tmp <- mutate_(CO2,mean=~mean(uptake))
head(tmp)

tmp <- mutate_(CO2,z1=~uptake/conc,y=~conc/100)
head(tmp)

tmp <- transmutate_(CO2,z2=~uptake/conc,y2=~conc/100)
head(tmp)
}
\keyword{m61r}
