#' Check model fitting for a basis_vec-count-related error and recommend a new number.
#'
#' This is a function to check the output of maeve:::models_in_list() called from
#' maeve::model_study().  A relatively common error in the automatic
#' calling is to have an untenably low or high number of spline basis_vecs designated
#' in the formula to gamm4() within maeve:::models_in_list().  This function attempts
#' to auto-detect which error is being made (too few basis_vecs or too many?) and 
#' suggest an augmented count to facilitate a successful model fit.
#'
#' @author Bill Forrest <forrest@@gene.com>
#'
#' @param fitted_model_list any R object with a class() attribute.  Anything for which class( fitted_model_list ) != "try-error" will be returned unchanged.
#' @param Min_Basis_Vec_n numeric minimum number of basis_vecs required for gamm4() fit.  Should match the value sent to maeve::model_study()
#' @param Basis_Vec_n     numeric target number of basis_vecs required for gamm4() fit.  Should match the value sent to maeve::model_study()
#' @param Max_Basis_Vec_n numeric maximum number of basis_vecs required for gamm4() fit.  Should match the value sent to maeve::model_study()
#' @param step_up numeric number by which to increase the target basis_vec count if the number of basis_vecs appears to be too low.
#' @param step_down numeric number by which to decrease the target basis_vec count if the number of basis_vecs appears to be too high.
#' @param too_low_message  character expected error message if the number of basis_vecs is too low.
#' @param too_high_message character expected error message if the number of basis_vecs is too high.
#' @param stop_for_other_error logical should there be a stop if the input is a "try-error", but not with either of the expected error messages?
#' @param other_error_message character error message returned in the list if the function does not stop and returns a list from an alternate try-error.
#'
#' @return A list with current & suggested numbers of spline basis_vecs.
#'
#' @examples
#'   test_check_basis_vec_error <-
#'     maeve:::check_basis_vec_error(
#'                              try( log( 'cat' ), silent = TRUE ),
#'                              stop_for_other_error = FALSE
#'                              )
#'
#' @author Bill Forrest \email{forrest@@gene.com}
#'
#' @references \url{www.r-project.org}
#'
#' @keywords list
#'
check_basis_vec_error <- function( fitted_model_list,
                              Min_Basis_Vec_n = NA, # should match    "min_basis_vecs" in maeve::model_study()
                              Basis_Vec_n     = NA, # should match "number_basis_vecs" in maeve::model_study()
                              Max_Basis_Vec_n = NA, # should match    "max_basis_vecs" in maeve::model_study()
                              step_up    =  1, # how many basis_vecs to add if the value is too low?
                              step_down  =  1, # how many basis_vecs to remove if the regression is overdetermined?
                              too_low_message  = "Error: grouping factors must have > 1 sampled level\n",
                              too_high_message = paste0(
                               "Error in ",
                               "smooth.construct.tp.smooth.spec(object, dk$data, dk$knots): ",
                               "A term has fewer unique covariate combinations ",
                               "than specified maximum degrees of freedom\n"
                               ),
                              stop_for_other_error = TRUE,
                               other_error_message = paste("Unknown error in fitted_model_list object.",
                                                           "Hands-on investigation required.\n"
                                                          )
                             ){

### If the input is not a try-error, just return the input unchanged.  
  if( ! "try-error" == class( fitted_model_list ) ){ return( fitted_model_list ) }

  
### If execution proceeds, then "fitted_model_list" is of class "try-error".
###
  error_message <- as.character( attributes(fitted_model_list)[['condition']] )
###
  
  diagnostic_list <- # this will get returned from the function.
    list( min_basis_vec_n = Min_Basis_Vec_n,
          basis_vec_n = Basis_Vec_n,
          max_basis_vec_n = Max_Basis_Vec_n,
          problem = NULL,
          suggested_basis_vec_n = NA
         )
  
  if( too_low_message == error_message ){
    diagnostic_list$problem <- 'too few spline basis_vecs passed to gamm4().  Increase "number_basis_vecs".\n'
    diagnostic_list$suggested_basis_vec_n <- diagnostic_list$basis_vec_n + step_up
  }

  if( too_high_message == error_message ){
    diagnostic_list$problem <- 'too many spline basis_vecs passed to gamm4().  Decrease "number_basis_vecs".\n'
    diagnostic_list$suggested_basis_vec_n <- diagnostic_list$basis_vec_n - step_down
  }

  if( ! error_message %in% c(too_low_message, too_high_message) ){
    if( stop_for_other_error  ){
      stop( other_error_message )      
    } else{
       diagnostic_list$problem <- other_error_message
       diagnostic_list$suggested_basis_vec_n <- NA
    }
  }

  return( diagnostic_list )
  
} # end of 'check_basis_vec_error(...'
