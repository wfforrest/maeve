% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/determine_EOS_CR.R
\name{determine_EOS_CR}
\alias{determine_EOS_CR}
\title{Determine whether each identifier has a complete response (CR) at the end of study (EOS).}
\usage{
determine_EOS_CR(YY, min_val = 0)
}
\arguments{
\item{YY}{data.frame with times and original-scale tumor burdens for each of several identifiers.}

\item{min_val}{numeric threshold below which a tumor size is classified as a Complete Response (CR) if it is at End of Study (EOS).}
}
\value{
A numeric sum total of Complete Response cases recorded at End of Study in the group.
}
\description{
Determine whether each identifier has a complete response (CR) at the end of study (EOS).
}
\examples{
 cat('Currently no working example for unexported function determine_EOS_CR().')

}
\references{
\url{www.r-project.org}
}
\author{
Bill Forrest <forrest@gene.com>

Bill Forrest \email{forrest@gene.com}
}
\keyword{data.frame}
