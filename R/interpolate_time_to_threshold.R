#' Interpolate the first time at which a group's fitted curve increases by a given threshold over its baseline.
#'
#' @author Bill Forrest <forrest@@gene.com>
#'
#' @param YY a one-group subset of the 'clean_DF_pred' data frame returned from maeve::fit_longitudinal_models().
#' @param metric character vector specifying what metric(s) to use in longitudinal modeling.
#' @param full_inverse_function R function inverting the data transformation, taking the fitted values from YY back to the original units.
#' @param threshold numeric fold-change (in the original units, after "full_inverse_function()") from baseline that is desired to detect.
#' @param grid_n numeric number of points in the grid for interpolation.
#' @param auto_round logical: whether to automatically round the interpolated time to precision specified by "round_to"
#' @param round_to numeric number of digits to which we should round the interpolated progression time.
#'
#' @return A numeric progression time.
#'
#' @examples
#'  cat('No working example for unexported function interpolate_time_to_threshold().')
#'
#' @author Bill Forrest \email{forrest@@gene.com}
#'
#' @references \url{www.r-project.org}
#'
#' @keywords data.frame
#'
    interpolate_time_to_threshold <-
      function( YY, metric, full_inverse_function, threshold = 2, grid_n = 1000, auto_round = TRUE, round_to = 1 ){
        
      if (nrow(YY) < 1 | !any( YY$model_x_value ) ) {
        return(NA) # Return NA for an empty group
      }

     ## Determine the correct fitted values column
     pred_name <-
        switch( metric,
                ## linear mixed model
                   linear = 'pred_lin',
                ## GAM spline predictor from a GAMM
                     ITGR = 'pred_gam',
                      AUC = 'pred_gam',
                ## Piecewise-linear mixed model
                 ITGR_pwl = 'pred_pwl',
                  AUC_pwl = 'pred_pwl',
                ## Polynomial basis mixed model
                ITGR_poly = 'pred_poly',
                 AUC_poly = 'pred_poly',
                ## no match -- stop.
               stop( 'error in maeve:::interpolate_time_to_threshold(): ', metric, ' is not a recognized option for "metric"' )
               ) # end of 'switch( metric,...'


     if( ! pred_name %in% colnames( YY ) ){
         stop( 'error in maeve:::interpolate_time_to_threshold():', pred_name, 'is not a column in the data.frame passed' )
     }
     
     ## Optionally trim data to a specified first day
     first_day <- maeve_options( "summary_first_day" )
     if( !is.null( first_day ) ){
       YY %<>%
         dplyr::filter( x >= first_day )
     }
      
     YY$y_fit <- YY[ , pred_name ]
      
     YY <- data.frame(unique(YY[ !is.na(YY$y_fit) ,c('x','y_fit')])) # keep only what's necessary.

     if (nrow(YY) < 2){ # need at least 2 values to interpolate
         return(NA)
     }
      
     ## Now, set up a fine grid from which to interpolate the first time at which the fitted
     ## values (normalized to their first time point) exceed "threshold".
     x_grid = seq( min(YY$x), max(YY$x), length = grid_n )
     approx_list <- approx( x = YY$x, y = YY$y_fit, xout = x_grid )
     YY_grid <- data.frame( x = approx_list$x, y_fit = approx_list$y, y_orig_fit = full_inverse_function( approx_list$y ) )
      
     ### Now, scale the fitted original values to the first time point:
     YY_grid$y_orig_fit_norm <-
          normalize_to_first_timepoint(
                                       YY_grid,
                                       x_name = 'x',
                                       y_name = 'y_orig_fit',
                                       average_multiple_obs = FALSE,
                                       centering_value = 0,
                                       scale_value = 1
                                       )
     if( all( YY_grid$y_orig_fit_norm < threshold ) ){
        TTP <- NA
     } else{
       TTP <- min( YY_grid$x[YY_grid$y_orig_fit_norm >= threshold] )
       if( auto_round ){ TTP <- round( TTP, round_to ) }
     }
                       
     return( TTP )
      
   } # end of 'interpolate_time_to_threshold <- function(...'
