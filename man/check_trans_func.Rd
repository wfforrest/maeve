% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_trans_func.R
\name{check_trans_func}
\alias{check_trans_func}
\title{Check the transformation and inverse transformation functions.}
\usage{
check_trans_func(
  trans_func_char = c("log", "Identity", "sqrt", "cube_root"),
  inv_func_char = c("exp", "Identity", "square", "cube"),
  test_func_x = 1:10,
  TOLERANCE = 1e-06
)
}
\arguments{
\item{trans_func_char}{character with transformation name.}

\item{inv_func_char}{character with inverse name.}

\item{test_func_x}{numeric values at which to check inverse.}

\item{TOLERANCE}{numeric tolerance for agreement of inverted values.}
}
\value{
a list with the transformation and inverse transformation functions.
}
\description{
Check the transformation and inverse transformation functions.
}
\examples{
 out <- maeve:::check_trans_func( 'log', 'exp' )
 
 

}
\seealso{
\code{\link{character}}
}
\author{
Bill Forrest \email{forrest@gene.com}
}
\keyword{character}
