\name{m61r}
\alias{m61r}
\alias{[.m61r}
\alias{[<-.m61r}
\alias{print.m61r}
\alias{names.m61r}
\alias{dim.m61r}
\alias{as.data.frame.m61r}
\encoding{UTF-8}
\title{Create m61r object}
\description{
Create a m61r object that enables to run a sequence of operations on a data.frame.
}
\usage{
m61r(df = NULL)

\method{[}{m61r}(x, i, j, ...)

\method{[}{m61r}(x, i, j) <- value

\method{print}{m61r}(x, ...)

\method{names}{m61r}(x, ...)

\method{dim}{m61r}(x, ...)

\method{as.data.frame}{m61r}(x, ...)

}
\arguments{
\item{df}{data.frame}

\item{x}{object of class \code{m61r}}

\item{i}{row}

\item{j}{column}

\item{...}{further arguments passed to or from other methods}

\item{value}{value to be assigned}

}
\value{
The function \code{m61r} returns an object of type \code{m61r}.

Argument \code{df} get stored internally to the object \code{m61r}.
One manipulates the internal \code{data.frame} by using internal functions similar to the ones implemented in package \code{m61r} for \code{data.frames} as \code{arrange}, \code{desange}, \code{filter}, \code{join} and its relatives, \code{mutate} and \code{transmutate}, \code{gather} and \code{spread}, \code{select}, \code{groupe_by}, \code{summarise}, \code{values} and \code{modify}.
The result of the last action is stored internally to the object \code{m61r} until the internal function \code{values} get called.
It is thus possible to create a readable sequence of actions on a \code{data.frame}.

In addition,
\itemize{
\item \code{[.m61r} returns a subset of the internal \code{data.frame} embedded to the object \code{m61r}.
\item \code{[<-.m61r} assigns \code{value} to the internal \code{data.frame} embedded to the object \code{m61r}.
\item \code{print.m61r} prints the internal \code{data.frame} embedded to the object \code{m61r}.
\item \code{names.m61r} provides the names of the column of the internal \code{data.frame} embedded to the object \code{m61r}.
\item \code{dim.m61r} provides the dimensions of the internal \code{data.frame} embedded to the object \code{m61r}.
\item \code{as.data.frame.m61r} extracts the internal \code{data.frame} embedded to the object \code{m61r}.
}

Finally, it is possible to clone a \code{m61r} object into a new one by using the internal function \code{clone}.

}
\examples{

  # init
  co2 <- m61r(df=CO2)

  # filter
  co2$filter(~Plant=="Qn1")
  co2

  co2$filter(~Type=="Quebec")
  co2

  # select
  co2$select(~Type)
  co2

  co2$select(~c(Plant,Type))
  co2

  co2$select(~-Type)
  co2

  co2$select(variable=~-(Plant:Treatment))
  co2

  # mutate/transmutate
  co2$mutate(z=~conc/uptake)
  co2

  co2$mutate(mean=~mean(uptake))
  co2

  co2$mutate(z1=~uptake/conc,y=~conc/100)
  co2

  co2$transmutate(z2=~uptake/conc,y2=~conc/100)
  co2

  # summarise
  co2$summarise(mean=~mean(uptake),sd=~sd(uptake))
  co2

  co2$group_by(~c(Type,Treatment))
  co2$summarise(mean=~mean(uptake),sd=~sd(uptake))
  co2

  # arrange/dessange
  co2$arrange(~c(conc))
  co2

  co2$arrange(~c(Treatment,conc,uptake))
  co2

  co2$desange(~c(Treatment,conc,uptake))
  co2

  # join
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

  # Reshape

  ## gather
  df3 <- data.frame(id = 1:4,
                    age = c(40,50,60,50),
                    dose.a1 = c(1,2,1,2),
                    dose.a2 = c(2,1,2,1),
                    dose.a14 = c(3,3,3,3))

  df4 <- m61r::m61r(df3)
  df4$gather(pivot = c("id","age"))
  df4

  ## spread
  df3 <- data.frame(id = 1:4,
                    age = c(40,50,60,50),
                    dose.a1 = c(1,2,1,2),
                    dose.a2 = c(2,1,2,1),
                    dose.a14 = c(3,3,3,3))

  df4 <- m61r::gather_(df3,pivot = c("id","age"))
  df4 <- rbind(df4,
    data.frame(id=5, age=20,parameters="dose.a14",values=8),
    data.frame(id=6, age=10,parameters="dose.a1",values=5))

  tmp <- m61r::m61r(df4)
  tmp$spread(col_name="parameters",col_values="values",pivot=c("id","age"))
  tmp


  # equivalence
  co2           # is not equivalent to co2[]
  co2[]         # is equivalent to co2$values()
  co2[1,]       # is equivalent to co2$values(1,)
  co2[,2:3]     # is equivalent to co2$values(,2:3)
  co2[1:10,1:3] # is equivalent to co2$values(1:10,2:3)
  co2[1,"Plant"]# is equivalent to co2$values(1,"Plant")

  # modification on m61r object only stay for one step
  co2[1,"conc"] <- 100
  co2[1,] # temporary result
  co2[1,] # back to normal

  # WARNING:
  # Keep the brackets to manipulate the intern data.frame
  co2[] <- co2[-1,]
  co2[1:3,] # temporary result
  co2[1:3,] # back to normal

  # ... OR you will destroy co2, and only keep the data.frame
  # co2 <- co2[-1,]
  # class(co2) # data.frame

  # descriptive manipulation
  names(co2)
  dim(co2)
  str(co2)

  ## cloning
  # The following will only create a second variable that point on
  # the same object (!= cloning)
  foo <- co2
  str(co2)
  str(foo)

  # Instead, cloning into a new environemnt
  foo <- co2$clone()
  str(co2)
  str(foo)
}
\keyword{m61r}
