\name{mutate}
\alias{mutate_}
\alias{transmutate_}
\encoding{UTF-8}
\title{Transformative selections of a data.frame}
\description{
  Transformative selections of a data.frame.
}
\details{
\code{mutate_} and \code{transmutate_} execute expressions non-grouped. If the \code{m61r} object is in a grouped state (via \code{$group_by()}), that grouping state is ignored by the primitive functions, ensuring Base R speed for vectorised operations.
}
\usage{
# mutate_(df, ...)
# transmutate_(df, ...)
}
\arguments{
\item{df}{data.frame}

\item{...}{formula used for transformative selections the data.frame}
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

# with m61r class
co2 <- m61r(df=CO2)

co2$mutate(z=~conc/uptake)
co2

# not allowed
#co2$mutate(mean=~mean(uptake))
#co2
#Warning message:
#In FUN(X[[i]], ...) : Expression mean has incompatible length.

co2$mutate(z1=~uptake/conc,y=~conc/100)
co2

co2$transmutate(z2=~uptake/conc,y2=~conc/100)
co2

}
\keyword{m61r}
