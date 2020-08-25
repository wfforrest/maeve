#' A data.frame with longitudinal data simulated from four trigonometric functions on the unit interval.
#'
#' Data frame with 7 columns and 1930 rows with a simulated study of four groups with twenty-five animals each.  Each animal has a group-specific trigonometric response pattern evaluated over twenty-one equally-spaced time points {0.00, 0.05, 0.10, ..., 0.95, 1.00}. See maeve/inst/scripts/simulate_maeve_test_data.R for details.
#'
#' The columns are as follows:
#' \itemize{
#'   \item  study_id           factor five-digit DIVOS study identification number (or a simulation name)
#'   \item  group_name         factor group label name
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
#' @name effect_duration_trig_sim
#' @usage data( piecewise_nine_points )
#' @format A data frame with 7 columns and 1930 rows.
#'
NULL
