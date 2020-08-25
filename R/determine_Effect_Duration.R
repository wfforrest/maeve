#' Determine the duration of effect for each treatment
#'
#' @author Bill Forrest <forrest@@gene.com>
#'
#' @param XY data.frame with times and original-scale tumor burdens, etc., for each of several identifiers.  Typically this is from the cleaned output data of maeve::model_study().
#' @param colname_deriv1     character name of column with first derivative from GAM model fit.
#' @param colname_ref_deriv1 character name of column with first derivative from specified reference group of GAM model fit.
#' @param diff_threshold numeric threshold to differentiate from zero, i.e., values below this are treated as zeros in evaluating changes.
#' @param grid_n numeric number of equally spaced grid points across which to approximate the effect duration.
#' @param auto_round logical: automatically round results to "round_to" digits?
#' @param round_to numeric number of digits to which the effect duration should be rounded.
#'
#' @return A numeric sum total of Partial Response cases recorded in the group.
#'
#' @examples
#'  cat('Currently no working example for unexported function determine_Effect_Duration().')
#'
#' @author Bill Forrest \email{forrest@@gene.com}
#'
#' @references \url{www.r-project.org}
#'
#' @keywords data.frame
#'
    determine_Effect_Duration <-
     function( XY,
               colname_deriv1      = 'pred_gam_deriv1',
               colname_ref_deriv1  = 'base_pred_gam_deriv1',
               diff_threshold = -1e-4,
               grid_n = 1000,
               auto_round = TRUE,
               round_to = 4
              ){

       ## Sanity check that the column names are (1) characters and (2) in the column names of the 'XY' data.frame
       stopifnot( is.character( colname_deriv1 ) &
                  colname_deriv1 %in% colnames( XY ) &
                   ##
                  is.character( colname_ref_deriv1 ) &
                  colname_ref_deriv1 %in% colnames( XY )
                 )

       ## Optionally trim data to a specified first day
       first_day <- maeve_options( "summary_first_day" )
       if( !is.null( first_day ) ){
         XY %<>%
           dplyr::filter( x >= first_day )
       }
       
       XY %<>% # standardize internal working names for the derivative columns.
       dplyr::rename( 'pred_gam_deriv1'      = !!dplyr::sym( colname_deriv1 ),
                      'base_pred_gam_deriv1' = !!dplyr::sym( colname_ref_deriv1 )
                     )
       
       ## If "pred_gam_deriv1" and/or "base_pred_gam_deriv1" are all NA, then just return NA values:
       if( all( is.na( XY[,'pred_gam_deriv1']      ) ) |
           all( is.na( XY[,'base_pred_gam_deriv1'] ) )
          ){
          return( list( effect_start = NA, effect_end = NA, effect_duration = NA ) ) 
       } # end of 'if( all( is.na( XY[,'pred_gam_deriv1']      ) ) |...'
        


       ## Determine duration of effect.        
       XY %<>%
       dplyr::filter(!is.na(pred_gam)) %>% # x <= max(x[model_x_value])) %>%
       dplyr::select( group, x, pred_gam_deriv1, base_pred_gam_deriv1 ) %>%
       dplyr::filter(!is.na(base_pred_gam_deriv1)) %>%
       dplyr::rename( ref_pred_gam_deriv1 = base_pred_gam_deriv1 ) %>%
       dplyr::mutate( ref_pred_gam_deriv1_first_time = unique(ref_pred_gam_deriv1[ x == min(x) ]) ) %>%         
       dplyr::arrange( x ) %>%
       dplyr::filter( !is.na( pred_gam_deriv1 ) ) %>%
       unique

       if( nrow( XY ) < 2 ){
         return( list( effect_start = NA, effect_end = NA, effect_duration = NA ) )
       }
       
       ## Now, set up a fine grid from which to interpolate the first time at which the
       ## difference in derivatives (i.e., growth rates) between treatment and reference.
       
       x_grid = seq( min( XY$x ), max( XY$x ), length = grid_n )

       spline_list_treat   <- spline( x = XY$x, y = XY$pred_gam_deriv1,     xout = x_grid )
       spline_list_control <- spline( x = XY$x, y = XY$ref_pred_gam_deriv1, xout = x_grid )       
       XY_grid <-
         data.frame( x                                         = x_grid,
                     y_deriv1                                  = spline_list_treat$y,  
                     y_deriv1_control                          = spline_list_control$y,
                     y_deriv1_treat_minus_control_time_matched = spline_list_treat$y - spline_list_control$y,
                     y_deriv1_control_first_timepoint          = unique( XY$ref_pred_gam_deriv1_first_time )
                    )       

       effect_start <- effect_end <- NA
       
       ## Determine when the effect starts
       if( all( with( XY_grid, y_deriv1_treat_minus_control_time_matched >= diff_threshold ) ) ){
       ## In this case, treatment group has a faster growth rate than control at all times.
           effect_start <- effect_end <- max( XY_grid$x ) 
       } else{ ## Here, treatment group has a slower growth rate than control at least once.
           effect_start <- with( XY_grid, min( x[ y_deriv1_treat_minus_control_time_matched < diff_threshold ] ) )
       }

       ## Determine when the effect ends
       
       if( is.na( effect_end ) ){
         ## First time after "effect_start" when the growth rate of treatment
         ## exceeds the growth rate of control
         y_deriv1_control_at_effect_start = with( XY_grid,  y_deriv1_control[ x == effect_start ] )
         index <- with( XY_grid, x > effect_start & y_deriv1 - y_deriv1_control_at_effect_start >= diff_threshold )
         if( !any( index ) ){
         ## This means that "y_deriv1 - y_deriv1_control >= diff_threshold" is never TRUE
         ## after "effect_start", so "effect_end" is just the end of study.
           effect_end = max( XY_grid$x )
         } else{
         ## 'effect_end' is the first time strictly after effect_start when
         ## treatment growth rate exceeds that of control.
            effect_end = min( XY_grid[ index, 'x' ] )
          }
       } # end of 'if( is.na( effect_end ) ){...'
       
       effect_duration = effect_end - effect_start

       if( auto_round ){
         effect_start    <- round( effect_start,    round_to )
         effect_end      <- round( effect_end,      round_to )         
         effect_duration <- round( effect_duration, round_to )
       }
       
       return( list( effect_start = effect_start, effect_end = effect_end, effect_duration = effect_duration ) )

    }  ### end of function definition for 'determine_Effect_Duration(...'
