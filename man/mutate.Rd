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
\value{
The functions return a data frame.
The output has the following properties:
\itemize{
\item
For function \code{mutate_()}, output includes all \code{df} columns.
In addition, new columns are created according to argument \code{...} and placed after the others.
\item
For function \code{transmutate_()}, output includes only columns created according to argument \code{...} and placed after the others.
}
}
\examples{
tmp <- mutate_(CO2,z=~conc/uptake)
head(tmp)

# Return an warning: expression mean(uptake) get a result with 'nrow' different from 'df'
# tmp <- mutate_(CO2,mean=~mean(uptake))

tmp <- mutate_(CO2,z1=~uptake/conc,y=~conc/100)
head(tmp)

tmp <- transmutate_(CO2,z2=~uptake/conc,y2=~conc/100)
head(tmp)
}
\keyword{m61r}
