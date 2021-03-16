\name{reshape}
\alias{gather_}
\alias{spread_}
\encoding{UTF-8}
\title{Reshape a data.frame}
\description{
Reshape a data.frame.
}
\usage{
gather_(df, new_col_name = "parameters", new_col_values = "values",
  pivot = NULL)

spread_(df, col_name, col_values, pivot = NULL)

}
\arguments{
\item{df}{data.frame}

\item{new_col_name}{name of the new column 'parameters'}

\item{new_col_values}{name of the new columns 'values'}

\item{col_name}{name of the column 'parameters'}

\item{col_values}{name of the new columns 'values'}

\item{pivot}{name of the columns used as pivot}

}
\examples{

df3 <- data.frame(id = 1:4,
                    age = c(40,50,60,50),
                    dose.a1 = c(1,2,1,2),
                    dose.a2 = c(2,1,2,1),
                    dose.a14 = c(3,3,3,3))

gather_(df3,pivot = c("id","age"))

df4 <- gather_(df3,pivot = c("id","age"))
df5 <- rbind(df4,
    data.frame(id=5, age=20,parameters="dose.a14",values=8),
    data.frame(id=6, age=10,parameters="dose.a1",values=5))

spread_(df5,col_name="parameters",col_values="values",pivot=c("id","age"))

}
\keyword{m61r}
