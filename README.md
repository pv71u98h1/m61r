# m61r

Minimal. No dependencies.

Data manipulation in one package and in base r.

'dplyr' and 'tidyr'-like in one place.

Nothing else than base r to build the package.

## Installation from github in base r

```R
setwd("~")

download.file("https://github.com/pv71u98h1/m61r/archive/0.0.1.zip",destfile="m61r-0.0.1.zip")
unzip("m61r-0.0.1.zip")

# install
install.packages(file.path("~","m61r-0.0.1"), repos=NULL, type='source')

# build vignettes
vign <- list.files(file.path("~","m61r-0.0.1","vignettes"))
dir.create(file.path("~","m61r-0.0.1","inst","doc"),recursive=TRUE)
lapply(vign,function(x){
   tools::buildVignette(file   = file.path("~","m61r-0.0.1","vignettes",x),
                 dir    = file.path("~","m61r-0.0.1","inst","doc"))
})

# install the vignettes
install.packages(file.path("~","m61r-0.0.1"), repos=NULL, type='source')

# clean
unlink(file.path("~","m61r-0.0.1"),recursive=TRUE)
file.remove(file.path("~","m61r-0.0.1.zip"))
```

## Usage

```R
library(m61r)

co2 <- m61r(CO2)
co2$filter(~Plant %in% c("Qn1","Qc3"))
co2$mutate(z1=~uptake/conc,y=~conc/100)
co2$group_by(~c(Type,Treatment))
co2$summarise(foo=~mean(uptake),bar=~sd(uptake))
co2 # get results

head(co2) # back to normal


```
