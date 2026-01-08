\name{create_key}
\alias{create_key}
\encoding{UTF-8}
\title{Generate Optimized Join Keys}
\description{
  An internal utility used to create a unique identifier vector from one or multiple columns. This is the foundational engine for filtering joins (\code{semi_join_} and \code{anti_join_}).
}
\usage{
# create_key(df, cols)
}
\arguments{
  \item{df}{A data.frame.}
  \item{cols}{A character vector of column names used to build the key.}
}
\details{
  For a single column, \code{create_key} simply extracts the vector. For multiple columns, it leverages \code{base::interaction} on factors. 
  
  By converting columns to factors before calling \code{interaction(..., drop = TRUE)}, the function ensures that only existing combinations are computed, significantly reducing memory overhead and increasing computation speed compared to string concatenation. This mimics the "hashing" phase of modern join engines within the constraints of Pure Base R.
}
\value{
  A vector (either atomic or a factor) representing the combined unique keys for each row of the data frame.
}
\examples{
df <- data.frame(id = c("A", "B", "A"), val = 1:3)
key_single <- create_key(df, "id")

df_comp <- data.frame(
  year = c(2020, 2020, 2021),
  month = c(1, 2, 1),
  data = 10:12
)
# Returns a factor representing unique year-month combinations
key_multi <- create_key(df_comp, c("year", "month"))

# key_x <- create_key(df1, by)
# key_y <- create_key(df2, by)
# match_idx <- key_x \%in\% key_y
}
\keyword{internal}
\keyword{manipulation}

