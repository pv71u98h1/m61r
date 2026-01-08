\name{join}
\alias{left_join_}
\alias{anti_join_}
\alias{full_join_}
\alias{inner_join_}
\alias{right_join_}
\alias{semi_join_}
\encoding{UTF-8}
\title{Join two data.frames}
\description{
  Join two data.frames.
}
\usage{
# left_join_(df, df2, by = NULL, by.x = NULL, by.y = NULL)
# anti_join_(df, df2, by = NULL, by.x = NULL, by.y = NULL)
# full_join_(df, df2, by = NULL, by.x = NULL, by.y = NULL)
# inner_join_(df, df2, by = NULL, by.x = NULL, by.y = NULL)
# right_join_(df, df2, by = NULL, by.x = NULL, by.y = NULL)
# semi_join_(df, df2, by = NULL, by.x = NULL, by.y = NULL)

}
\arguments{
\item{df}{data.frame}

\item{df2}{data.frame}

\item{by}{column names of the pivot of both data.frame 1 and data.frame 2 if they are identical. Otherwise, better to use by.x and by.y}

\item{by.x}{column names of the pivot of data.frame 1}

\item{by.y}{column names of the pivot of data.frame 2}

}
\value{
The functions return a data frame.
The output has the following properties:
\itemize{
\item
For functions \code{left_join()}, \code{inner_join()}, \code{full_join()}, and \code{right_join()}, output includes all \code{df1} columns and all \code{df2} columns. For columns with
identical names in \code{df1} and \code{df2}, a suffix '.x' and '.y' is added.
For \code{left_join()}, all \code{df1} rows with matching rows of \code{df2}
For \code{inner_join()}, a subset of \code{df1} rows matching rows of \code{df2}.
For \code{full_join()}, all \code{df1} rows, with all \code{df2} rows.
For \code{right_join()}, all \code{df2} rows with matching rows of \code{df1}.
\item
For functions \code{semi_join()} and \code{anti_join()}, output include columns of \code{df1} only.
For \code{semi_join()}, all \code{df1} rows with a match in \code{df2}.
For \code{anti_join()}, a subset of \code{df1} rows not matching rows of \code{df2}.
}
}
\examples{

books <- data.frame(
             name = I(c("Tukey", "Venables", "Tierney","Ripley",
                   "Ripley", "McNeil", "R Core")),
             title = c("Exploratory Data Analysis",
                   "Modern Applied Statistics ...",
                   "LISP-STAT",
                   "Spatial Statistics", "Stochastic Simulation",
                   "Interactive Data Analysis",
                   "An Introduction to R"),
              other.author = c(NA, "Ripley", NA, NA, NA, NA,"Venables & Smith"))

authors <- data.frame(
               surname = I(c("Tukey", "Venables", "Tierney", "Ripley", "McNeil","Asimov")),
               nationality = c("US", "Australia", "US", "UK", "Australia","US"),
               deceased = c("yes", rep("no", 4),"yes"))

tmp <- left_join_(books,authors, by.x = "name", by.y = "surname")
head(tmp)

tmp <- inner_join_(books,authors, by.x = "name", by.y = "surname")
head(tmp)

tmp <- full_join_(books,authors, by.x = "name", by.y = "surname")
head(tmp)

tmp <- right_join_(books,authors, by.x = "name", by.y = "surname")
head(tmp)

tmp <- semi_join_(books,authors, by.x = "name", by.y = "surname")
head(tmp)

tmp <- anti_join_(books,authors, by.x = "name", by.y = "surname")
head(tmp)


  # with m61r class
  
  ## inner join
  tmp <- m61r(df=authors)

  tmp$inner_join(books, by.x = "surname", by.y = "name")
  tmp

  ## left join
  tmp$left_join(books, by.x = "surname", by.y = "name")
  tmp

  ## right join
  tmp$right_join(books, by.x = "surname", by.y = "name")
  tmp

  ## full join
  tmp$full_join(books, by.x = "surname", by.y = "name")
  tmp

  ## semi join
  tmp$semi_join(books, by.x = "surname", by.y = "name")
  tmp

  ## anti join #1
  tmp$anti_join(books, by.x = "surname", by.y = "name")
  tmp

  ## anti join #2
  tmp2 <- m61r(df=books)
  tmp2$anti_join(authors, by.x = "name", by.y = "surname")
  tmp2

  ## with two m61r objects
  tmp1 <- m61r(books)
  tmp2 <- m61r(authors)
  tmp3 <- anti_join(tmp1,tmp2, by.x = "name", by.y = "surname")
  tmp3

}
\keyword{m61r}
