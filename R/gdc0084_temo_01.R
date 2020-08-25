#' A data.frame with longitudinal measurements from a combination study of GDC-0084 and temozolomide.
#'
#' Data frame with 7 columns and 1411 rows recorded from NCr nude mice with subcutaneous human glioblastoma patient-derived xenografts.
#' GDC-0084 (a.k.a. paxalisib, a PI3K/mTOR inhibitor) was administered at one of four levels daily for 21 days.
#' Temozolomide (a DNA alkylating agent) was administered at one of four levels daily for 5 days.
#' Dose levels in mg/kg are encoded in the group_name factor levels.
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
#' @name gdc0084_temo_01
#' @usage data( gdc0084_temo_01 )
#' @format A data frame with 7 columns and 1411 rows.
#'
NULL
