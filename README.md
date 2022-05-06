# m61r

![](http://cranlogs.r-pkg.org/badges/last-month/m61r)

Minimal. No dependencies.

Data manipulation in one package and in base R.

'dplyr' and 'tidyr'-like in one place.

'pipe' effect replaced by object 'm61r'.

Nothing else than base R to build the package.

R-package 'm61r' is at the core of [m61r-cli](https://github.com/pv71u98h1/m61r-cli), data manipulation in bash command line.

## Installation from CRAN

```R
install.packages("m61r")
```

## Installation from github in base R

```R
setwd("~")

download.file("https://github.com/pv71u98h1/m61r/archive/0.0.3.zip",destfile="m61r-0.0.3.zip")
unzip("m61r-0.0.3.zip")

# install
install.packages(file.path("~","m61r-0.0.3"), repos=NULL, type='source')

# build vignettes
vign <- list.files(file.path("~","m61r-0.0.3","vignettes"))
dir.create(file.path("~","m61r-0.0.3","inst","doc"),recursive=TRUE)
lapply(vign,function(x){
   tools::buildVignette(file   = file.path("~","m61r-0.0.3","vignettes",x),
                 dir    = file.path("~","m61r-0.0.3","inst","doc"))
})

# install the vignettes
install.packages(file.path("~","m61r-0.0.3"), repos=NULL, type='source')

# clean
unlink(file.path("~","m61r-0.0.3"),recursive=TRUE)
file.remove(file.path("~","m61r-0.0.3.zip"))
```

## Usage

### Example 1: pipeline with 1 step cache
```R
library(m61r)

co2 <- m61r(CO2)
co2$filter(~Plant %in% c("Qn1","Qc3"))
co2$mutate(z1=~uptake/conc,y=~conc/100)
co2$group_by(~c(Type,Treatment))
co2$summarise(foo=~mean(z1),bar=~sd(y))
co2 # print results

head(co2) # back to normal
```

### Example 2: filter, select, mutate, summarise, group_by, join and reshape

```R

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

  ## with two m61r objects
  tmp1 <- m61r(books)
  tmp2 <- m61r(authors)
  tmp3 <- anti_join(tmp1,tmp2, by.x = "name", by.y = "surname")
  tmp3

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

```
### Example 3: manipulation of a m61r object

```R

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

```
