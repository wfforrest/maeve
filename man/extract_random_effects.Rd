% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_random_effects.R
\name{extract_random_effects}
\alias{extract_random_effects}
\title{Extract random intercept and slope effects from lmerMod and / or gam models.}
\usage{
extract_random_effects(
  md4_lmer,
  md4_gamm4,
  md4_lmer_pwl,
  md4_lmer_poly,
  levels_ID = NULL
)
}
\arguments{
\item{md4_lmer}{lmerMod object returned from a maeve:: call to lme4::lmer()}

\item{md4_gamm4}{list with 'mer' component returned from gamm4::gamm4()}

\item{md4_lmer_pwl}{lmerMod object returned from a maeve by piecewise linear mixed model regression.}

\item{md4_lmer_poly}{lmerMod object returned from a maeve by polynomial basis mixed model regression.}

\item{levels_ID}{optional character vector with the ordered levels of subject identifiers.  If not provided, the function attempts to get these levels from the models.}
}
\value{
a data.frame with intercept & slope random effects.
}
\description{
Extract random intercept and slope effects from lmerMod and / or gam models.
}
\examples{
 cat( 'No working example for unexported function extract_random_effect().' )

}
\seealso{
\code{\link{data.frame}}
}
\author{
Bill Forrest \email{forrest@gene.com}
}
\keyword{character}
