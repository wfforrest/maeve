% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_basis_matrix.R
\name{make_basis_matrix}
\alias{make_basis_matrix}
\title{Make a matrix for a linear (mixed) model with known basis functions.}
\usage{
make_basis_matrix(
  x,
  basis = c("tent", "poly"),
  break_points = NULL,
  add_column_names = TRUE,
  format_digits = 3,
  degree = 2,
  include_intercept = FALSE,
  use_poly_coefs = FALSE
)
}
\arguments{
\item{x}{numeric vector of values to use in forming the basis.}

\item{basis}{character string specifying which kind of basis to generate.}

\item{break_points}{numeric vector with the x-locations at which to place break points.}

\item{add_column_names}{logical whether to add column names to the matrix.}

\item{format_digits}{how many digits to retain in column names?  Used only if add_column_names is TRUE}

\item{degree}{numeric degree of polynomial basis, if that is selected.  Passed straight to stats::poly().}

\item{include_intercept}{logical whether to append an intercept term to the matrix for a polynomial basis.}

\item{use_poly_coefs}{logical whether to use "attr( maeve_options('poly_object'), 'coef')" to scale poly basis matrix.}
}
\value{
a matrix
}
\description{
Takes a vector of x-values and a basis type ("tent" and "poly" are current options) 
and returns a design matrix for fitting a piecewise linear mixed effects model with 
one group.  Basis created uses the so-called "tent-functions".  See, e.g., 
Simon Wood (2017) GAM 2nd edition text, section 4.2, page 164-166.
}
\examples{
  ### Need to put in an actual example here.
  simple_tent_basis <-
    maeve:::make_basis_matrix( x = seq( 0, 1, length = 11 ),
                               basis = 'tent',
                               break_points = c( 0, 0.5, 1 )
                              )
  ##
  simple_poly_basis <-
    maeve:::make_basis_matrix( x = seq( 0, 1, length = 11 ),
                               basis = 'poly',
                               degree = 3
                              )
  

}
\seealso{
\code{\link{matrix}}
}
\author{
Bill Forrest \email{forrest@gene.com}
}
\keyword{matrix}
