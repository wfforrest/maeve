% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_basis_vec_error.R
\name{check_basis_vec_error}
\alias{check_basis_vec_error}
\title{Check model fitting for a basis_vec-count-related error and recommend a new number.}
\usage{
check_basis_vec_error(
  fitted_model_list,
  Min_Basis_Vec_n = NA,
  Basis_Vec_n = NA,
  Max_Basis_Vec_n = NA,
  step_up = 1,
  step_down = 1,
  too_low_message = "Error: grouping factors must have > 1 sampled level\\n",
  too_high_message = paste0("Error in ",
    "smooth.construct.tp.smooth.spec(object, dk$data, dk$knots): ",
    "A term has fewer unique covariate combinations ",
    "than specified maximum degrees of freedom\\n"),
  stop_for_other_error = TRUE,
  other_error_message = paste("Unknown error in fitted_model_list object.",
    "Hands-on investigation required.\\n")
)
}
\arguments{
\item{fitted_model_list}{any R object with a class() attribute.  Anything for which class( fitted_model_list ) != "try-error" will be returned unchanged.}

\item{Min_Basis_Vec_n}{numeric minimum number of basis_vecs required for gamm4() fit.  Should match the value sent to maeve::model_study()}

\item{Basis_Vec_n}{numeric target number of basis_vecs required for gamm4() fit.  Should match the value sent to maeve::model_study()}

\item{Max_Basis_Vec_n}{numeric maximum number of basis_vecs required for gamm4() fit.  Should match the value sent to maeve::model_study()}

\item{step_up}{numeric number by which to increase the target basis_vec count if the number of basis_vecs appears to be too low.}

\item{step_down}{numeric number by which to decrease the target basis_vec count if the number of basis_vecs appears to be too high.}

\item{too_low_message}{character expected error message if the number of basis_vecs is too low.}

\item{too_high_message}{character expected error message if the number of basis_vecs is too high.}

\item{stop_for_other_error}{logical should there be a stop if the input is a "try-error", but not with either of the expected error messages?}

\item{other_error_message}{character error message returned in the list if the function does not stop and returns a list from an alternate try-error.}
}
\value{
A list with current & suggested numbers of spline basis_vecs.
}
\description{
This is a function to check the output of maeve:::models_in_list() called from
maeve::model_study().  A relatively common error in the automatic
calling is to have an untenably low or high number of spline basis_vecs designated
in the formula to gamm4() within maeve:::models_in_list().  This function attempts
to auto-detect which error is being made (too few basis_vecs or too many?) and 
suggest an augmented count to facilitate a successful model fit.
}
\examples{
  test_check_basis_vec_error <-
    maeve:::check_basis_vec_error(
                             try( log( 'cat' ), silent = TRUE ),
                             stop_for_other_error = FALSE
                             )

}
\references{
\url{www.r-project.org}
}
\author{
Bill Forrest <forrest@gene.com>

Bill Forrest \email{forrest@gene.com}
}
\keyword{list}
