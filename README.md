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

### Example 2: get only a data.frame as result
```R
co2 <- m61r(CO2)
co2$filter(~Plant %in% c("Qn1","Qc3"))
co2$transmutate(z1=~uptake/conc,y=~conc/100)
tmp <- co2[] # get only the data.frame and not the whole m61r object

head(tmp)

class(tmp)
```

### Example 3: manipulation of a m61r object
```R
co2 <- m61r(CO2)
head(co2)
names(co2)
dim(co2)
co2[1,]
head(co2[,2:3])
co2[1:10,1:3]
co2[1,"Plant"]
str(co2)

co2[1,"conc"] <- 100
co2[1,] # w/temporary change
co2[1,] # back to normal

# WARNING: Keep the brackets to manipulate the intern data.frame
co2[] <- co2[-1,]
co2[1:3,] # temporary result
co2[1:3,] # back to normal

# ... OR you will destroy co2, and only keep the data.frame
# co2 <- co2[-1,]
# class(co2) # data.frame

# cloning
foo <- co2 # This will only create
           # a second variable that points
           # on the same object (i.e not cloning)
str(co2)
str(foo)

# Instead, cloning into a new environment
foo <- co2$clone()
str(co2)
str(foo)

```
