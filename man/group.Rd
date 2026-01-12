\name{get_group_indices_}
\alias{get_group_indices_}
\encoding{UTF-8}
\title{Determine Grouping Structure for a data.frame}
\description{
\code{get_group_indices_} calculates the necessary indices and keys for efficient grouped operations (like \code{summarise_}). This mechanism uses Base R \code{\link[base]{interaction}} for group factor calculation.
}
\usage{
# get_group_indices_(df, group = NULL)
}
\arguments{
\item{df}{data.frame}
\item{group}{A formula (\code{~<expression>}) that describes the grouping columns. Column names can be listed in a vector (e.g., \code{~c(colA, colB)}). Base R functions may be nested to process columns (e.g., for date-time components).}
}
\value{
\code{get_group_indices_} returns a list containing: \code{group_cols} (names), \code{indices} (a list of row indices per group, for fast subsetting), and \code{keys} (a data frame of unique group combinations). 
}
\examples{

g_info <- get_group_indices_(CO2, ~c(Type, Treatment))
summarise_(CO2, group_info = g_info, mean = ~mean(uptake))


# Grouping with a Base R function: Group by the 'year' of a column 'Date'
df_date <- data.frame(
  Date = seq(as.Date("2020-01-01"), by = "month", length.out = 12),
  Value = 1:12
)

# Usage within the m61r pipeline:
df_date_m61r <- m61r(df_date)
df_date_m61r$group_by(~format(Date, "\%Y"))
df_date_m61r$summarise(mean_val = ~mean(Value))
df_date_m61r
}
\keyword{m61r}
\keyword{grouping}
