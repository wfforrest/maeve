% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/truncate_numeric.R
\name{truncate_numeric}
\alias{truncate_numeric}
\title{Truncate values in a numeric vector, logically or numerically.}
\usage{
truncate_numeric(x, lower = -Inf, upper = Inf, type = c("logical", "numeric"))
}
\arguments{
\item{x}{numeric vector to be truncated}

\item{lower}{numeric lower end or allowable range.}

\item{upper}{numeric upper end or allowable range.}

\item{type}{character indicating whether to return a "logical" or "numeric" vector.}
}
\value{
a vector the same length as the input vector, with either logical or numeric entries.
}
\description{
Takes a numeric vector and returns a vector of equal length, either a logical or a numeric with values truncated within provided ranges.
}
\examples{
   dat <- data.frame( x = 0:10, y = rnorm( 11 ), letter = letters[1:11] )
   truncate_numeric( dat$x, 3, 8 ) # logical vector by default
   subset( dat, truncate_numeric( dat$x, 3, 8 ) ) # subset a data frame.
   #
   truncate_numeric( dat$x, 3, 8, 'numeric' ) # numeric vector


}
\seealso{
\code{\link{data.frame}}
}
\author{
Bill Forrest \email{forrest@gene.com}
}
\keyword{truncate}
