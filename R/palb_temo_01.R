#' A data.frame with longitudinal measurements from a combination study of palbociclib and temozolomide.
#'
#' Data frame with 7 columns and 668 rows recorded from NCr nude mice with subcutaneous human glioblastoma patient-derived xenografts.
#' This is a combination study with palbociclib (a SMI of CDK 4/6) at two levels and temozolomide (a DNA alkylating agent) at four levels.
#' Palbociclib was dosed for the first 21 days of the study, while temozolomide was dosed for the first 5 days.
#' Doses in mg/kg are encoded in levels of the group_name factor.
#'
#' The columns are as follows:
#' \itemize{
#'   \item  study_id           factor five-digit study identification number
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
#' @name palb_temo_01
#' @usage data( palb_temo_01 )
#' @format A data frame with 7 columns and 668 rows.
#'
NULL
