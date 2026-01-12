\name{summarise}
\alias{summarise_}
\encoding{UTF-8}
\title{Summarise Formula on Groups }
\description{
  Summarise of formulas on a data.frame.
}
\details{
\code{summarise_} is the aggregation function. It expects the grouping information from \code{get_group_indices_}. When a formula expression (e.g., \code{~mean(uptake)}) is run, it is executed for each group subset, relying on Base R \code{\link[base]{lapply}} over the pre-calculated group indices for performance. \strong{All expressions within \code{summarise_} must return an atomic vector of length 1 for each group.}
}
\usage{
# summarise_(df, group_info = NULL, ...)
}
\arguments{
\item{df}{data.frame}

\item{group_info}{formula that describes the group}

\item{...}{formulas to be generated}
}
\value{
\code{summarise_()} returns a data frame.
If argument \code{group_info} is not \code{NULL}, output get its first columns called according to the names present in argument \code{group_info}.
The following columns are called according to the name of each argument present in \code{...}.
Each row corresponds to processed expressions determined in \code{...} for each group determined in \code{group_info}, or over the whole data frame if \code{group_info} is \code{NULL}.
}
\examples{

  summarise_(CO2,a=~mean(uptake),b=~sd(uptake))

  g_info <- get_group_indices_(CO2, ~c(Type, Treatment))
  tmp <- summarise_(CO2, group_info=g_info,mean=~mean(uptake),sd=~sd(uptake))
  tmp

  # with m61r class
  co2 <- m61r(df=CO2)

  # summarise
  co2$summarise(mean=~mean(uptake),sd=~sd(uptake))
  co2
  
  co2 = m61r(CO2)
  co2$group_by(~cbind(Type,Treatment))
  co2$summarise(mean=~mean(uptake),sd=~sd(uptake))
  co2
}
\keyword{m61r}

