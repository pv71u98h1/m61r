\name{m61r-package}
\alias{m61r-package}
\docType{package}
\title{High-Performance Data Manipulation with Pure Base R}
\description{
The \code{m61r} package provides a suite of optimized functions for tabular data manipulation, 
The design prioritizes computational speed and a clean, readable data-processing grammar.
The package got highly inspired by modern data manipulation packages, while exclusively built upon the Base R environment. 
}
\details{
The core of \code{m61r} is the Base R Data Manipulation Grammar, implemented through two layers:

\itemize{
\item \strong{Primitives (e.g., \code{filter_}, \code{arrange_}):} These are the raw, optimized functions operating directly on Base R \code{data.frame} objects. They are designed for maximum computational efficiency.
\item \strong{\code{m61r} Object (\code{m61r()} constructor):} This S3 class provides a pipeline interface, allowing a sequence of operations (e.g., \code{$filter()}, \code{$mutate()}) to be chained cleanly. 
}

\strong{The Base R Formula Domain-Specific Language:}
All manipulation functions (like \code{filter}, \code{mutate}, \code{group_by}, \code{select}, \code{summarise}) utilise a formula syntax (\code{~<expression>}). This ensures Non-Standard Evaluation can access column names directly within the context of the data frame.

\strong{Date and Time Handling:}
For complex grouping or filtering on date-time columns (\code{Date} or \code{POSIXct}), users must employ standard Base R functions within the formula expression to extract components.
For instance, to group by the year of a column named \code{DateColumn}, one must use Base R \code{format} function: \code{~format(DateColumn, "\%Y")}

For detailed documentation, see the individual function reference pages.
}
\keyword{package}
\keyword{data manipulation}
\keyword{Base R}