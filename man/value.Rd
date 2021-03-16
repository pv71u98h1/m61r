\name{value}
\alias{value_}
\alias{modify_<-}
\encoding{UTF-8}
\title{get or assign a value to a m61r object}
\description{
Get or assign a value to a m61r object
}
\usage{
value_(df, i, j)

'modify_<-'(df,i,j,value)
}
\arguments{
\item{df}{data.frame}

\item{i}{row}

\item{j}{column}

\item{value}{value to be assigned}
}
\examples{

df3 <- data.frame(id = 1:4,
                      age = c(40,50,60,50),
                      dose.a1 = c(1,2,1,2),
                      dose.a2 = c(2,1,2,1),
                      dose.a14 = c(3,3,3,3))

value_(df3,1,2)
'modify_<-'(df3,1,2,6)

}
\keyword{m61r}
