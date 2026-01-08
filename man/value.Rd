\name{value}
\alias{value_}
\alias{modify_<-}
\encoding{UTF-8}
\title{get or assign a value to a data.frame}
\description{
Get or assign a value to a data.frame
}
\usage{
# value_(df, i, j)
# 'modify_<-'(df,i,j,value)
}
\arguments{
\item{df}{data.frame}

\item{i}{row}

\item{j}{column}

\item{value}{value to be assigned}
}
\value{
The functions \code{value_} and \code{'modify_<-'} return a data frame.
Properties:
\itemize{
\item Only rows determined by \item{i} appear. If \item{i} is missing, no row is filtered.
\item Only columns determined by \item{j} appear. If \item{j} is missing, no column is filtered.
}
Besides,
\itemize{
\item For function \code{value_}:
If argument \code{i} is non-missing and argument \code{j} is missing, the function returns an object of the same type as \code{df}.
If both arguments \code{i} and \code{j} are missing, the function returns an object of the same type as \code{df}.
\item For function \code{'modify_<-'}:
The function returns an object of the same type as \code{df}.
}
}
\examples{

tmp <- value_(CO2,1,2)
attributes(tmp) # data frame

tmp <- value_(CO2,1:2,2)
attributes(tmp) # data frame

tmp <- value_(CO2,1:2,2:4)
attributes(tmp) # data frame

tmp <- value_(CO2,,2)
attributes(tmp) # data frame

tmp <- value_(CO2,2)
attributes(tmp) # same as CO2

tmp <- value_(CO2)
attributes(tmp) # same as CO2

df3 <- data.frame(id = 1:4,
                      age = c(40,50,60,50),
                      dose.a1 = c(1,2,1,2),
                      dose.a2 = c(2,1,2,1),
                      dose.a14 = c(3,3,3,3))


'modify_<-'(df3,1,2,6)

'modify_<-'(df3,1:3,2:4,data.frame(c(20,10,90),c(9,3,4),c(0,0,0)))

}
\keyword{m61r}
