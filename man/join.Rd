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
left_join_(df, df2, by = NULL, by.x = NULL, by.y = NULL)

anti_join_(df, df2, by = NULL, by.x = NULL, by.y = NULL)

full_join_(df, df2, by = NULL, by.x = NULL, by.y = NULL)

inner_join_(df, df2, by = NULL, by.x = NULL, by.y = NULL)

right_join_(df, df2, by = NULL, by.x = NULL, by.y = NULL)

semi_join_(df, df2, by = NULL, by.x = NULL, by.y = NULL)

}
\arguments{
\item{df}{data.frame}

\item{df2}{data.frame}

\item{by}{column names of the pivot of both data.frame 1 and data.frame 2 if they are identical. Otherwise, better to use by.x and by.y}

\item{by.x}{column names of the pivot of data.frame 1}

\item{by.y}{column names of the pivot of data.frame 2}
}
\examples{

authors <- data.frame(
               surname = I(c("Tukey", "Venables", "Tierney", "Ripley", "McNeil")),
               nationality = c("US", "Australia", "US", "UK", "Australia"),
               deceased = c("yes", rep("no", 4)))

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

tmp <- inner_join_(authors,books, by.x = "surname", by.y = "name")
head(tmp)

tmp <- left_join_(authors,books, by.x = "surname", by.y = "name")
head(tmp)

tmp <- right_join_(authors,books, by.x = "surname", by.y = "name")
head(tmp)

tmp <- full_join_(authors,books, by.x = "surname", by.y = "name")
head(tmp)

tmp <- semi_join_(authors,books, by.x = "surname", by.y = "name")
head(tmp)

tmp <- anti_join_(authors,books, by.x = "surname", by.y = "name")
head(tmp)

tmp <- anti_join_(books,authors, by.x = "name", by.y = "surname")
head(tmp)

}
\keyword{m61r}
