#' A data.frame with longitudinal measurements from a dose escalation study of vismodegib in a trichoblastoma allograft model.
#'
#' Data frame with 7 columns and 385 rows.
#' This is a dose escalation study of vismodegib (a small molecule inhibitor of the Hedgehog pathway)
#' in CD-1 nude mice bearing subcutaneous trichoblastoma allografts.
#' The study includes forty-six animals across ten dose levels.
#'
#' The columns are as follows:
#' \itemize{
#'   \item  study_id           factor five-digit DIVOS identification number
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
#' @name vismodegib
#' @usage data( vismodegib )
#' @format A data frame with 7 columns and 385 rows.
#'
NULL
