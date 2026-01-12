\name{io_csv}
\alias{read_csv}
\alias{write_csv}
\encoding{UTF-8}
\title{CSV Input and Output Utilities}
\description{
  High-performance wrappers for reading and writing CSV files. These functions utilize Base R \code{read.table} and \code{write.table} engines while ensuring the resulting data frames are optimized for \code{m61r} pipelines.
}
\usage{
# read_csv(file, header = TRUE, sep = ",", stringsAsFactors = FALSE, ...)

# Within an m61r pipeline
# p$write_csv(file, sep = ",", row.names = FALSE, quote = FALSE, ...)
}
\arguments{
  \item{file}{A character string specifying the file path.}
  \item{header}{Logical; does the file contain a header row?}
  \item{sep}{The field separator character.}
  \item{stringsAsFactors}{Logical; should character vectors be converted to factors?}
  \item{row.names}{Logical; should row names be written to the file?}
  \item{quote}{Logical; should character strings be quoted?}
  \item{...}{Additional arguments passed to the underlying \code{read.table} or \code{write.table} functions.}
}
\details{
  \code{read_csv} is an optimized loader that automatically strips row names after reading, ensuring a clean index for subsequent \code{m61r} operations.
  
  \code{write_csv} is designed to be used as a terminal step in an \code{m61r} pipeline. It accesses the internal \code{result_} buffer of the object and exports it to the specified file path.
}
\value{
  \code{read_csv} returns a \code{data.frame}. 
  \code{write_csv} returns \code{invisible()} and is used for its side effect of file creation.
}
\examples{

# df <- read_csv("data.csv")
# p <- m61r(df)

p <- m61r(mtcars)
p$filter(~mpg > 20)
p$mutate(hp_per_cyl = ~hp / cyl)

# Export results
# p$write_csv("filtered_mtcars.csv")
}
\keyword{m61r}
\keyword{IO}