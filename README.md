# m61r

![](http://cranlogs.r-pkg.org/badges/last-month/m61r)

Minimal. No dependencies.

Data manipulation in one package and in base R.

'dplyr' and 'tidyr'-like in one place.

'pipe' effect replace by object 'm61r'.

Nothing else than base R to build the package.

## Installation from CRAN

```R
install.packages("m61r")
```

## Installation from github in base R

```R
setwd("~")

download.file("https://github.com/pv71u98h1/m61r/archive/0.0.2.zip",destfile="m61r-0.0.2.zip")
unzip("m61r-0.0.2.zip")

# install
install.packages(file.path("~","m61r-0.0.2"), repos=NULL, type='source')

# build vignettes
vign <- list.files(file.path("~","m61r-0.0.2","vignettes"))
dir.create(file.path("~","m61r-0.0.2","inst","doc"),recursive=TRUE)
lapply(vign,function(x){
   tools::buildVignette(file   = file.path("~","m61r-0.0.2","vignettes",x),
                 dir    = file.path("~","m61r-0.0.2","inst","doc"))
})

# install the vignettes
install.packages(file.path("~","m61r-0.0.2"), repos=NULL, type='source')

# clean
unlink(file.path("~","m61r-0.0.2"),recursive=TRUE)
file.remove(file.path("~","m61r-0.0.2.zip"))
```

## Usage

### example 1: filter, mutate, group_by, ...

```R
library(m61r)

co2 <- m61r(CO2)
co2$filter(~Plant %in% c("Qn1","Qc3"))
co2$mutate(z1=~uptake/conc,y=~conc/100)
co2$group_by(~c(Type,Treatment))
co2$summarise(foo=~mean(uptake),bar=~sd(uptake))
co2 # get results

co2 # back to normal
```

### example 2: gather and spread
```R
library(m61r)

## gather
df3 <- data.frame(id = 1:4,
                  age = c(40,50,60,50),
                  dose.a1 = c(1,2,1,2),
                  dose.a2 = c(2,1,2,1),
                  dose.a14 = c(3,3,3,3))

res <- m61r::m61r(df3)
res$gather(pivot = c("id","age"))
res
res # back to normal

## spread
res$gather(pivot = c("id","age"))

df4 <- rbind(res[],
  data.frame(id=5, age=20,parameters="dose.a14",values=8),
  data.frame(id=6, age=10,parameters="dose.a1",values=5))

tmp <- m61r::m61r(df4)
tmp$spread(col_name="parameters",col_values="values",pivot=c("id","age"))
tmp
```
