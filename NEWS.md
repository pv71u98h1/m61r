# m61r 0.1.0

### Major New Features

* **Enhanced "Pipeline" Interface**: The `m61r` object now allows for a more fluid chaining of "pipe-like" operations.
* **Support for Scoped Operations**:
    * Introduced `.SD()` and `.select_cols()` for advanced column manipulation within the object's environment.
    * Added the `across()` function, allowing a function to be applied to multiple columns simultaneously within `mutate()` and `summarise()`.    
* **Temporal Orchestration**:
    * Added the `$explode()` method to `explode` list-columns, typically used after creating temporal sequences.
    * Added `$join_asof()` for proximity-based temporal joins (backward/forward), essential for asynchronous time-series.
* **Conditional Logic**: Implemented `case_when_` to allow complex vectorised syntax within the `m61r` object.
* **I/O**: Implemented `write.csv` to allow exporting data.frame from the `m61r` object.

### API Improvements

* **summarise_**: Complete code refactoring of name management. Named expressions (e.g., `avg = ~mean(x)`) and list expressions (e.g., `~across(...)`) now correctly handle suffixes and prevent `NA` column names.
* **mutate_**: Optimized internal evaluation for better memory management on large data frames.
* **Joins**: Improved stability of join functions (`inner`, `left`, `right`, `full`, `semi`, `anti`) across mixed column types.

### Documentation

* Created two new detailed vignettes:
    1. `m61r_advanced`: Complex manipulation and functional programming.
    2. `m61r_temporal`: Specific management of time-series data.

