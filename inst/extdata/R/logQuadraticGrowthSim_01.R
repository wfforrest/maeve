#' A data.frame with longitudinal data simulated from different quadratic trends.
#'
#' Data frame with 7 columns and 336 rows with a simulated study of three groups with ten animals each.  Each animal has a log-quadratic concave response pattern with amplitude that differs by group. See maeve/inst/scripts/simulate_maeve_test_data.R for details.
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
#' @name logQuadraticGrowthSim_01
#' @usage data( logQuadraticGrowthSim_01 )
#' @format A data frame with 7 columns and 336 rows.
#'
NULL
