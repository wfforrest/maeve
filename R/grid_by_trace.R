#' Interpolate multiple longitudinally traced patterns of y-values onto a common grid of x-values.
#'
#' Creates interpolated (x,y) values onto a common grid of x-values
#' from each trace_ID, without any explicit model fit.  The
#' interpolation uses stats::approxfun() or stats::splinefun() with
#' one of {'linear', 'trapezoid', 'step', 'spline'} as its `method`,
#' where 'linear' and 'trapezoid' are synonyms in this function.
#' The grid onto which values are interpolated is the union of
#' all the distinct x-values across all traces and an evenly spaced
#' grid of 'grid_n' (see parameters) x-values spanning the x-grid range.
#'
#' The 'trace_ID' factor can have two or more levels nested
#' within a subject ID.  Two trace levels can denote, e.g.,
#' longitudinal scans on two different tissues within the
#' same animal, or two sequential longitudinal scans on the
#' same tissue but with different, sequentially applied treatments.
#'
#' If VST == TRUE, each ID-level vector of endpoint values
#' (specified in `endpoint_name`) will be transformed for variance
#' stabilization by adding a constant then transforming
#' (e.g., "y = log( 1 + [[endpoint_name]] )").
#' The ID-level vector can be optionally centered to its first (in time-order)
#' value, i.e, each y-value can have the first y-value subtracted from it.
#'
#' @author Bill Forrest <forrest@@gene.com>
#'
#' @param study_data_frame        data.frame a "well-prepared" data.frame (e.g., processed through maeve::check_study_data_frame() is recommended but not required) with the study data and appropriate columns.
#' @param check_data              logical   whether to pass "study_data_frame" through maeve::check_study_data_frame() to check for problems.
#' @param method                  character name method for computing AUC.  Passed directly to DescTools::AUC( ..., method = ... ).
#' @param VST                     logical   whether to apply a variance stabilizing transform to endpoint_name values.
#' @param xmin                    numeric  left / lower  end of the interval.
#' @param xmax                    numeric right / uppper end of the interval.
#' @param xtol                    machine epsilon value to decide whether an x-grid value matches one of the observed x-values.
#' @param x_grid                  numeric vector of points at which to interpolate y-values.
#' @param grid_n                  numeric number of points in the grid.
#' @param include_obs_x           logical whether to force inclusion of the union of observed 'x' times in each trace.
#' @param subtract_starting_value logical whether to subtract, for each trace_ID, the first value in the time series so that each series starts at zero.
#' @param na.rm                   logical whether to remove NA values when computing summaries.
#' @param singleton_to_NA         logical whether to assign trace_ID cases with a single observation NA or zero.
#' @param group_name              character column name for the group name factor
#' @param subject_ID              character column name for the subject name factor
#' @param trace_ID                character column name for the subject name factor
#' @param x_name                  character column name for the x-axis / time field
#' @param endpoint_name           character column name for the x-axis / time field
#' @param add_to_endpoint         numeric   offset value added to the endpoint before transformation
#' @param trans_func_char         character function name for transformation of (endpoint_name + add_to_endpoint)
#' @param inv_func_char           character inverse function for transformation of (endpoint_name + add_to_endpoint)
#' @param test_func_x             numeric   values with which to test that trans_func and inv_func are inverse functions.
#'
#' @return An R data.frame with one row per ID.
#'
#' @examples
#'  vismo21 <- dplyr::filter( vismodegib, DAY_OF_STUDY <= 21 )
#'  vismo_grid_by_trace <- grid_by_trace( vismo21 )
#'
#'
#' @author Bill Forrest \email{forrest@@gene.com}
#'
#' @references \url{www.r-project.org}
#'
#' @keywords data.frame
#' @export
#'
grid_by_trace <-
    function( study_data_frame,
                check_data = FALSE,
                    method = c( 'linear', 'trapezoid', 'step', 'spline' ),
                       VST = TRUE, # If TRUE, add to endpoint & apply variance-stabilizing transform before AUC
                      xmin = -Inf,
                      xmax =  Inf,
                      xtol = (.Machine$double.eps) * 100,
                    x_grid = NULL,
                    grid_n =  101,
             include_obs_x = TRUE,
   subtract_starting_value = FALSE, # If TRUE, subtract 1st value from all others, for each ID.
                     na.rm = TRUE,  # If TRUE, discard rows with NA for endpoint_name before calculations.
           singleton_to_NA = TRUE,  # If TRUE, singleton AUC = NA.  If FALSE, singleton AUC = 0.
            ##
                group_name = maeve_options("group_name"),
                subject_ID = maeve_options("subject_ID"),
                  trace_ID = maeve_options("trace_ID"),
                    x_name = maeve_options("x_name"),
             endpoint_name = maeve_options("endpoint_name"),
           add_to_endpoint = maeve_options("add_to_endpoint"),
           trans_func_char = maeve_options("trans_func_char"),
             inv_func_char = maeve_options("inv_func_char"),
               test_func_x = maeve_options("test_func_x")
             ){

        
    method = match.arg( method, several.ok = FALSE )

    dat <-
         study_data_frame %>%
         dplyr::select( group_name, trace_ID, x_name, endpoint_name )

    if( check_data ){
      dat %<>% maeve::check_study_data_frame(   group_name = group_name,
                                                subject_ID = trace_ID,
                                                    x_name = x_name,
                                             endpoint_name = endpoint_name,
                                            ##
                                            print_warnings = maeve_options('progress')
                                             )
    }

        
    ## "Window" the transformed data, interpolating to endpoints as needed.
    ## Note that by default, xmin = -Inf & xmax = Inf, in which case
    ## no truncation or interpolation will happen.
    dat %<>%
    maeve::window_by_id( group_name = group_name,
                         subject_ID = trace_ID,
                             x_name = x_name,
                      endpoint_name = endpoint_name,
                                 ##
                                VST = VST,
                    add_to_endpoint = add_to_endpoint,
                    trans_func_char = trans_func_char,
                      inv_func_char = inv_func_char,
                        test_func_x = test_func_x,
                                 ##
                             method = method,
                               xmin = xmin,
                               xmax = xmax,
                                ##
                 return_all_columns = TRUE,
                   unwindowed_to_NA = TRUE,
                         check_data = FALSE # already checked above
                           )

    ## Find a transformed endpoint?
    if( VST == TRUE ){ # Apply a Variance Stabilizing Transformation:
       ## Check the transformation function & assign it & its inverse:
       func_list  <- check_trans_func( trans_func_char, inv_func_char, test_func_x )
       trans_func <- func_list$trans_func;
         inv_func <- func_list$inv_func;
       ## Define 'y' as the transformed endpoint:
       dat %<>% dplyr::mutate( y = trans_func( get( endpoint_name ) + add_to_endpoint ) )
    } else{
       dat %<>% dplyr::mutate( y = get( endpoint_name ) )
    }

    ## find 'first_time' and 'last_time' for each ID.
    dat %<>%
    dplyr::mutate( x = get( x_name ) ) %>% ###, y_orig = get( endpoint_name ) ) %>%
    plyr::ddply(
              .var = c( eval( group_name ), eval( trace_ID ) ),
              .fun = function(XX, na_rm = na.rm ){
                  if( na_rm ){
                  XX %<>% dplyr::filter( !is.na( x ) & !is.na( y ) )
                  }
                  XX %<>%
                   dplyr::mutate( first_time     = min( XX[,x_name], na.rm = na_rm ),
                                  last_time      = max( XX[,x_name], na.rm = na_rm ),
                                  x_range_of_ID = last_time - first_time
                                ) %>%
                   maeve::round_numerics()
                  return(XX)
              }
              ) # end of 'plyr::ddply(...'

    ## Subtract out starting value for each ID?
    if( subtract_starting_value == TRUE ){
     dat %<>%
     plyr::ddply(
               .var = c( eval( group_name ), eval( trace_ID ), 'first_time', 'last_time', 'x_range_of_ID' ),
               .fun = function( XX, na_rm = na.rm ){
                   baseline_y <- XX %>% dplyr::filter( x <= first_time ) %>% dplyr::select( y ) %>% unlist
                   XX %<>% dplyr::mutate( y = y - baseline_y ) %>%  maeve::round_numerics()
                   return(XX)
               }
               ) # end of 'plyr::ddply(...'
    }


    ## Make a grid for x-values that will contain all the traces.
    if( is.null( x_grid ) ){
        x_grid <- seq( min( dat$first_time ), max( dat$last_time ), length = grid_n )
    }
        
    ## if include_obs_x == TRUE, include the observed x-values into the grid.
    if( include_obs_x ){
        x_grid <- sort( union( x_grid, dat$x ) )
    }
        
    y_grid <- rep( NA, length( x_grid ) )


    lgn  <- eval( group_name ) # lgn  = longitudinal grid name.
    ltid <- eval( trace_ID )   # ltid = longituidnal trace ID

    dat_grid <- expand.grid( levels( dat[, ltid ] ), x_grid )
    colnames( dat_grid ) <- c( ltid, 'x_grid' )

    dat_grid <- dplyr::inner_join( dat[, c( lgn, ltid ) ], dat_grid, by = ltid )
    dat_grid <- dat_grid[ order( dat_grid[,lgn], dat_grid[,ltid], dat_grid[,'x_grid'] ), ]

    dat_grid %<>%
      maeve::round_numerics() %>%
      dplyr::mutate( y_grid = NA ) %>%
      dplyr::distinct()


    dat_grid <-
      plyr::ddply( .data = dat_grid,
                   .var  = eval( trace_ID ),
                   .fun  = function( X, # data.frame with a grid of desired 'x' values and NA for the 'y' values.
                                     obs = dat, # observed data.frame with actual 'x' and 'y' values.
                                     interp_method = method,
                                     local_trace_ID = trace_ID,
                                     local_x_name = x_name
                                   ){ # start ddply .fun defintion.

                      current_trace = unique( as.character( X[, eval(local_trace_ID) ] ) ) # current trace, as a character string.
                      obs <- subset( obs, obs[, eval(local_trace_ID) ]  ==  current_trace )  # restrict real data to that from the current trace

                      xv <- obs[ , eval( local_x_name )  ]
                      yv <- obs[ , 'y' ]

                      ## logical vector for which grid x-values from the current trace are
                      ## in the range [ min( xv ), max( xv ) ].
                      ind <- dplyr::between( X$x_grid,  left = min( xv ), right = max( xv ) )

                      ## NB: This is a *different* interpolation function for each subject_ID.
                      endpoint_interp_function <-
                          switch( interp_method,
                                 'linear'    = stats::approxfun( xv, yv, method = 'linear' ), # synonym for 'trapezoid' in this context.
                                 'trapezoid' = stats::approxfun( xv, yv, method = 'linear' ), # synonym for 'linear'    in this context.
                                 'step'      = stats::approxfun( xv, yv, method = 'constant', f = 0 ),
                                 'spline'    = stats::splinefun( xv, yv, method = 'fmm' )
                                )

                      if( any( ind ) ){
                          ## Will it *always* work without this if(...) qualifier?
                          X[ ind, 'y_grid' ] <- c(endpoint_interp_function( X[ ind, 'x_grid' ] ))
                      }

                      return( X )

                    } # end of internal '.fun = function( X, ...' definition.

                   ) # end of 'plyr::ddply(...'


    colnames( dat_grid )[ colnames( dat_grid ) == 'x_grid' ] <-  x_name
    colnames( dat_grid )[ colnames( dat_grid ) == 'y_grid' ] <- 'y'

    ## Make a factor column that is the same length as "x_grid", but
    ## which tracks which of the values are originally observations,
    ## as opposed to interpolated values.
    y_interp_obs <- apply( abs( outer( dat_grid[,x_name], sort(unique(dat$x)), '-') ), 1, min ) > xtol

    y_type   <- ifelse( y_interp_obs,  'interpolated', 'observed' ) %>%
                factor(    levels = c( 'interpolated', 'observed' ) )
    dat_grid <- cbind( dat_grid, type = y_type )
    dat_grid <- swap_columns( dat_grid, 'y', 'type' )

    ## if a variance-stabilizing transform (VST) is applied,
    ## then the transformed endpoint is called 'y'.  Otherwise
    ## we keep the existing name.
    endpoint_name_to_use <- 'y'

    ## Back-transform the endpoint to the original scale.
    if( VST == TRUE ){ # We applied a Variance Stabilizing Transformation (VST).
       dat_grid[, endpoint_name ] <- inv_func( dat_grid[, endpoint_name_to_use ] ) - add_to_endpoint
    } else{
       ## Define 'y' as the original, untransformed endpoint:
       dat_grid[, endpoint_name ] <- dat_grid[, endpoint_name_to_use ]
    }

    if( endpoint_name != 'y' ){
       ## If the original endpoint_name was not "y", then take out the working columns we've been calling "y":
       dat_grid <- dat_grid[, -which( colnames( dat_grid ) == 'y' ) ]
    }

    return( dat_grid )

} # end of 'grid_by_trace <- function(...'
