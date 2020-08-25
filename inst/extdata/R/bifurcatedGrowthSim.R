#' A data.frame with longitudinal data simulated from two different linear trends to exemplify bifurcated response patterns.
#'
#' Data frame with 7 columns and 1000 rows with a simulated study of four groups with ten animals each.  Each animal has a log-linear response pattern drawn from one of two patterns, but the pattern frequency differs by group.  This illustrates a simple within-group bifurcation of response patterns.  See maeve/inst/scripts/simulate_maeve_test_data.R for details.
#'
#' The columns are as follows:
#' \itemize{
#'   \item  study_id           factor five-digit DIVOS study identification number
#'   \item  group_name         factor dose group label
#'   \item  cohort_id          factor cohort identifier
#'   \item  animalID           factor animal identifier
#'   \item  DAY_OF_STUDY       numeric  day of study
#'   \item  BODY_WEIGHT        numeric  body weight in grams
#'   \item  TUMOR_VOLUME       numeric  tumor volume calculated as ( 0.5 ) * ( TUMOR_LENGTH_X ) * ( TUMOR_LENGTH_Y )^2
#'}
#'
#' @author Bill Forrest \email{forrest@@gene.com}
#'
#' @references \url{www.r-project.org}
#'
#' @docType data
#' @keywords datasets
#' @name bifurcatedGrowthSim
#' @usage data( bifurcatedGrowthSim )
#' @format A data frame with 7 columns and 1000 rows.
#'
NULL
