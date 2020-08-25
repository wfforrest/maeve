#' Determine whether each identifier has a complete response (CR) at the end of study (EOS).
#'
#' @author Bill Forrest <forrest@@gene.com>
#'
#' @param YY data.frame with times and original-scale tumor burdens, etc., for each of several identifiers.  Typically this is from the cleaned output data of maeve::fit_longitudinal_models().
#' @param min_val numeric threshold below which a tumor size is classified as a Complete Response (CR).  CRs by this criterion get excluded from the PR count.
#' @param PR_threshold numeric threshold below which a tumor size is classified as a Partial Response (PR).
#' @param truncate_at_last_fit_day logical: Use days only for which the group had a model fit.
#'
#' @return A numeric sum total of Partial Response cases recorded in the group.
#'
#' @examples
#'  cat('Currently no working example for unexported function determine_PR().')
#'
#' @author Bill Forrest \email{forrest@@gene.com}
#'
#' @references \url{www.r-project.org}
#'
#' @keywords data.frame
#'
    determine_PR <-
      function( YY, min_val, PR_threshold, truncate_at_last_fit_day = FALSE ){

        ## optionally trim data to a specified first day
        first_day <- maeve_options( "summary_first_day" )
        if( !is.null( first_day ) ){
          YY %<>%
            dplyr::filter( x >= first_day )
        }
        
        tmp <- plyr::ddply( .data = YY,
                            .variables = 'ID',
                            .fun = function( YYY,
                                             ddply_min_val = min_val, ## evaluate to exclude EOS_CR-qualifiers from PR status.
                                             ddply_PR_threshold = PR_threshold ## Check whether raw endpoint values ever drop from baseline below this threshold.
                                            ){
                                      
                                      ## keep observations that did *NOT* have fits, so long as they have valid observed values.
                                      if( truncate_at_last_fit_day ){
                                        ### Get last day used for fitting:
                                        fit_last_day <- max( YYY$x[ !( is.na( YYY$pred_lin ) & is.na( YYY$pred_gam ) ) ] , na.rm = TRUE )
                                        YYY <- YYY[ !is.na( YYY$y_orig ) & YYY$x <= fit_last_day, ]
                                      } else{
                                        YYY <- YYY[ !is.na( YYY$y_orig )                        , ];
                                      }

                                      if( nrow( YYY ) == 0 ){
                                        return(NULL)
                                      }
                              
                                      YYY$y_orig_norm <-
                                        normalize_to_first_timepoint(
                                                                     YYY,
                                                                     y_name               = 'y_orig',
                                                                     centering_value      = 0,
                                                                     scale_value          = 1,
                                                                     average_multiple_obs = FALSE
                                                                     )
                                     
                                     output <- data.frame( ID = factor( unique( as.character( YYY$ID ) ) ), PR = NA )

                                      if( ## Two part test for whether a single ID had a partial response (PR):
                                           YYY[ YYY$x == max( YYY$x ), 'y_orig' ] > ddply_min_val & # *LAST* raw value exceeds minimum, usually zero.
                                           any( YYY[, 'y_orig_norm' ] <= ddply_PR_threshold ) # one or more normalized values below the PR threshold.
                                         ){
                                           output$PR <- TRUE  ### Yes, this ID has a partial response
                                      } else{
                                           output$PR <- FALSE ### No, no partial response
                                      }
                                        
                                     return(output)
                                     } ## end of ".fun = function( YYY, PR_threshold, min_val ){...",  within plyr::ddply(... 

                           ) ## end of "plyr::ddply( YY, .var = 'ID', ... "
        
         return( sum( tmp$PR ) ) # return the sum total of Partial Response cases recorded in the group.
      } ## end of "determine_PR <- function(..."
