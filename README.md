# m61r: The Pure Base R Data Engine 🌌

[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://cran.r-project.org/package=m61r)
[![Base R](https://img.shields.io/badge/R-Base%20Only-green.svg)](https://www.r-project.org/)

`m61r` provides dependency-free tabular and time-series manipulation for R. It is designed for the Base R purist who demands modern syntax and high performance.


## Why m61r?

- **Zero Dependencies**: No `dplyr`, no `tidyverse`. It runs on any vanilla R installation.

- **Unified Logic**: Treat tabular and time series as one.

- **High Performance**: Optimized Base R verbs for tabular accounting.

- **Fluent API**: A clean, object-oriented API inspired by the best modern data tools

- **Transparent**: Written in 100% Base R


## Quick Start

1. Tabular Manipulation

```R

library(m61r)

# Initialize with the built-in CO2 dataset

co2 <- m61r(CO2)

co2$filter(~uptake > 30)$
    select(~c(Plant, Type, uptake))$
    group_by(~Type)$
    summarise(mean_uptake = ~mean(uptake))

print(co2[])

```

2. Temporal Data Manipulation

```R
ts_data <- data.frame(
  time = as.POSIXct("2025-01-01 00:00:00") + runif(50, 0, 86400),
  consumption = rnorm(50, 500, 100)
)

p_agg <- m61r(ts_data)

p_agg$mutate(hour_bin = ~format(time, "%Y-%m-%d %H:00"))
p_agg$group_by(~hour_bin)
p_agg$summarise(
  n_obs = ~length(consumption),
  avg_load = ~mean(consumption)
)
p_agg$head(5)
```

# Benchmarks

1. filter, mutate, head

```R
library(microbenchmark)
library(m61r)

set.seed(42)
n <- 1e6
df_large <- data.frame(
  id = 1:n,
  group = sample(letters, n, replace = TRUE),
  val = rnorm(n)
)

p <- m61r(df_large)

results <- microbenchmark(
  # Approche Base R classique
  base_r = {
    tmp <- df_large[df_large$val > 0, ]
    tmp$new_val <- tmp$val * 2
    head(tmp)
  },
  
  # Approche m61r
  m61r_pipe = {    
    p$filter(~val > 0)
    p$mutate(new_val = ~val * 2)
    p$head()
  },
  times = 50 
)

print(results)
```

**Unit: milliseconds**

| `expr` | `min` | `lq` | `mean` | `median` | `uq` | `max` | `neval` |
| --- | --- | --- | --- | --- | --- | --- | --- |
| base_r | 37.79 | 42.08 | 49.75 | 44.39 | 59.48 | 92.38 | 50 |
| **m61r_pipe** | 20.11 | 21.94 | 27.10 | **23.78** | 26.19 | 55.45 | 50 |


2. group_by, summarise
```R
results_group <- microbenchmark(
  # Base R avec aggregate
  base_aggregate = {
    aggregate(val ~ group, data = df_large, FUN = mean)
  },
  
  # m61r
  m61r_grouped = {
    p$group_by(~group)
    p$summarise(avg = ~mean(val))
    p[]
  },
  times = 20
)

print(results_group)
```

**Unit: milliseconds**

| `expr` | `min` | `lq` | `mean` | `median` | `uq` | `max` | `neval` |
| --- | --- | --- | --- | --- | --- | --- | --- |
| base_aggregate | 329.80 | 350.84 | 361.70 | 354.05 | 372.74 | 442.04 | 20 |
| **m61r_grouped** | 146.57 | 147.70 | 154.79 | **149.88** | 165.77 | 172.25 | 20 |


# Installation


## Installation from CRAN

```R
install.packages("m61r")
```

## Installation from GITHUB in Base R

```R
setwd("~")

download.file("https://github.com/pv71u98h1/m61r/archive/0.1..zip",destfile="m61r-0.1.0.zip")
unzip("m61r-0.1.0.zip")

# install
install.packages("m61r", repos=NULL, type='source')

unlink(file.path("~","m61r-0.1.0"),recursive=TRUE)
file.remove(file.path("~","m61r-0.1.0.zip"))
```

