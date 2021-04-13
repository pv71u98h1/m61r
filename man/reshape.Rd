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
  pivot)

spread_(df, col_name, col_values, pivot)

}
\arguments{
\item{df}{data.frame}

\item{new_col_name}{name of the new column 'parameters'}

\item{new_col_values}{name of the new columns 'values'}

\item{col_name}{name of the column 'parameters'}

\item{col_values}{name of the new columns 'values'}

\item{pivot}{name of the columns used as pivot}

}
\details{
A data frame is said 'wide' if several of its columns describe connected information of the same record.
A data frame is said ‘long’ if two of its columns provide information about records, with one describing their name and the second their value.
Functions \code{gather_()} and \code{spread_()} enable to reshape a data frames from a ‘wide’ format to a 'long' format, and vice-versa.
}
\value{
The functions return a data frame.
\itemize{
\item Output from function \code{gather_()} get 'pivot' columns determined by argument \code{pivot}, and 'long' columns named according to arguments \code{new_col_name} and \code{new_col_values}.
\item Output from function \code{spread_()} get 'pivot' columns determined by argument \code{pivot}, and 'wide' columns named according to values in column determined by argument \code{col_name}.
For 'wide' columns, each row corresponds to values present in column determined by argument \code{col_values}.
}
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
