#' Calculate ID-level longitudinal summaries (linear slope, ITGR, or AUC) from raw data.
#'
#' Computes a longitudinal summary measure for the (x,y) values
#' from each subject_ID, without any explicit model fit.  The 
#' AUC is calculated using `DescTools::AUC(...)`, which takes
#' one of {'trapezoid', 'step', 'spline'} as its `method`.
#'
#' If VST == TRUE, each ID-level vector of endpoint values 
#' (specified in `endpoint_name`) will be transformed for variance
#' stabilization by adding a constant then transforming
#' (e.g., "y = log( 1 + [[endpoint_name]] )").
#' If subtract_starting_value = TRUE, the ID-level vector is centered to
#' its first (in time-order) value, i.e, each y-value will have the first
#' y-value subtracted from it. These transformed & centered values then have
#' their AUC relative to the line y = 0 computed by the DescTools::AUC(...)
#' function and given `method`. ITGR and AUC summaries will be x-range normalized
#' based on the value of 'xrange_norm_method'. Under default settings, each
#' should then be close to the linear slope summary when the longitudinal
#' trace is fairly linear. With the default settings, the "AUC" and "ITGR"
#' statistics are empirical, subject-specific versions of the "eGaIT" and "eDOT"
#' summary statistics, while the "linear" statistic is the least-squares slope
#' of "y" regressed on "x" after any transformations.
#'
#' If a subject_ID has only one observation, its summary is ambiguous, since,
#' its literal AUC or vertical shift must be zero, but so is the x-range over
#' which it is found. By default, these receive NA, but are set to zero if
#' `singleton_to_NA == FALSE.`
#'
#' @author Bill Forrest <forrest@@gene.com>
#'
#' @param study_data_frame        data.frame with factors for group_name and subject_ID, numeric columns for x_name (i.e., time) and endpoint_name (i.e., response).
#' @param metric                  character name method for longitudinal summary.
#' @param method                  character name method for computing AUC.  Passed directly to DescTools::AUC( ..., method = ... ).
#' @param check_data              logical   whether to pass "study_data_frame" through maeve::check_study_data_frame() to check for problems.
#' @param VST                     logical   whether to apply a variance stabilizing transform to endpoint_name values.
#' @param xmin                    numeric  left / lower  end of the interval.
#' @param xmax                    numeric right / uppper end of the interval.
#' @param subtract_starting_value logical whether to subtract, for each subject_ID, the first value in the time series so that each series starts at zero.
#' @param na.rm                   logical whether to remove NA values when computing summaries.
#' @param singleton_to_NA         logical whether to assign subject_ID cases with a single observation NA or zero.
#' @param group_name              character column name for the group name factor
#' @param subject_ID              character column name for the subject name factor
#' @param x_name                  character column name for the x-axis / time field
#' @param endpoint_name           character column name for the x-axis / time field
#' @param add_to_endpoint         numeric   offset value added to the endpoint before transformation
#' @param trans_func_char         character function name for transformation of (endpoint_name + add_to_endpoint)
#' @param inv_func_char           character inverse function for transformation of (endpoint_name + add_to_endpoint)
#' @param test_func_x             numeric   values with which to test that trans_func and inv_func are inverse functions.
#' @param xrange_norm_method      character method for normalization of spline summary statistics.
#' 
#' @return An R data.frame with one row per ID.
#'
#' @examples
#'  vismo_summary_by_id <- summarize_by_id( vismodegib, xmin = 0, xmax = 21 )
#'
#' @author Bill Forrest \email{forrest@@gene.com}
#'
#' @references \url{www.r-project.org}
#'
#' @keywords data.frame
#' @export
#'
summarize_by_id <-
    function( study_data_frame,
              metric     = c( 'linear', 'ITGR', 'AUC' ), ## one or more of c( 'linear', 'ITGR', 'AUC' )
              method     = c( 'trapezoid', 'step', 'spline' ),
              check_data = FALSE,
              VST        = TRUE, # If TRUE, add to endpoint & apply variance-stabilizing transform before AUC
              xmin       = -Inf,
              xmax       =  Inf,
              subtract_starting_value = TRUE, # If TRUE, subtract 1st value from all others, for each ID.
              na.rm      = TRUE, # If TRUE, discard rows with NA for endpoint_name before calculations.
              singleton_to_NA  = TRUE, # If TRUE, singleton AUC = NA.  If FALSE, singleton AUC = 0.
              ##
              group_name         = maeve_options("group_name"),
              subject_ID         = maeve_options("subject_ID"),
              x_name             = maeve_options("x_name"),
              endpoint_name      = maeve_options("endpoint_name"),
              add_to_endpoint    = maeve_options("add_to_endpoint"),
              trans_func_char    = maeve_options("trans_func_char"),
              inv_func_char      = maeve_options("inv_func_char"),
              test_func_x        = maeve_options("test_func_x"),
              xrange_norm_method = maeve_options("xrange_norm_method")
             ){

    metrics_allowed <- c( 'linear', 'ITGR', 'AUC' )

    metric <- unique( metric ) ## ignore any repeats.
        
    mismatched_metric <- metric[ which( is.na( match( metric, metrics_allowed ) ) ) ] ## setdiff( metric, metrics_allowed )
           
    if( length( mismatched_metric ) > 0 ){
        stop_msg_string_01 <- paste( mismatched_metric, collapse = '  ' )
        stop_msg_string_02 <- paste( metrics_allowed,   collapse = '  ')
        stop( 'error in maeve::summarize_by_id(): the following metric value(s):\n',
               stop_msg_string_01,
              '\n',
              'fail(s) to match perfectly with any of the allowed metric values:\n',
               stop_msg_string_02,
              '\n'
            ) # end of 'stop(...'
    } # end of 'if( length( mismatched_metric ) > 0 ){...'

    metric = match.arg( metric, choices = metrics_allowed, several.ok = TRUE )

    ## If two or more metrics are requested (e.g., "linear" and "AUC"), call
    ## this function recursively to compute each separately, then merge the
    ## results and return them.
    if( length( metric ) > 1 ){
        ## if 2+ metrics are requested, use recursion:
        out_list <- vector( "list", length = length( metric ) )
        names( out_list ) <- metric
        for( metric_name in metric ){
            
            out_list[[ metric_name ]] <-
                ## call this function (i.e., recursively) but with only 1 "metric" value:
                summarize_by_id( study_data_frame,
                                 check_data = check_data,
                                 metric = metric_name,
                                 method = method,
                                 VST = VST,                                
                                 xmin = xmin,
                                 xmax = xmax,
                                 subtract_starting_value = subtract_starting_value,
                                 na.rm = na.rm,
                                 singleton_to_NA = singleton_to_NA,
                                 ##
                                 group_name         = group_name,
                                 subject_ID         = subject_ID,
                                 x_name             = x_name,
                                 endpoint_name      = endpoint_name,
                                 add_to_endpoint    = add_to_endpoint,
                                 trans_func_char    = trans_func_char,
                                 inv_func_char      = inv_func_char,
                                 test_func_x        = test_func_x,
                                 xrange_norm_method = xrange_norm_method
                                )
        } ## end of 'for( metric_name in metric ){...'
        
        merge_column_names <- c( group_name, subject_ID, 'first_day', 'last_day' )

        ## The data frames in the list differ only by their last (i.e., 5th) column.
        ## Create a single data frame that has the first four columns in common, and
        ## adds the 5th column from each of the others to make a multi-metric summary:
        out_multiple_summaries <-
            base::Reduce( function(dtf1, dtf2) merge(dtf1, dtf2, by = merge_column_names, all.x = TRUE ), out_list )

        out_multiple_summaries[, group_name] <- factor( out_multiple_summaries[, group_name ],
                                                        levels = levels( study_data_frame[, group_name ] )
                                                       )

        out_multiple_summaries[, subject_ID] <- factor( out_multiple_summaries[, subject_ID ],
                                                        levels = levels( study_data_frame[, subject_ID ] )
                                                       )
        
        out_multiple_summaries %<>% dplyr::arrange( !!dplyr::sym( group_name ), !!dplyr::sym( subject_ID ) )
        
        return( out_multiple_summaries )
        
    } ## end of 'if( length( metric ) > 1 ){...'

    ## Which method is used to interpolate between the points?
    method = match.arg( method,  several.ok = FALSE )
      
    dat <-
        study_data_frame %>%
        dplyr::select( group_name, subject_ID, x_name, endpoint_name )

    if( check_data ){
      dat %<>% maeve::check_study_data_frame(    group_name = group_name,
                                                 subject_ID = subject_ID,
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
                         subject_ID = subject_ID,
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
       
    ## find 'first_day' and 'last_day' for each ID.
    dat %<>%
     dplyr::mutate( x = get( x_name ) ) %>%
     plyr::ddply(
               .var = c( eval( group_name ), eval( subject_ID ) ),
               .fun = function(XX, na_rm = na.rm ){
                        if( na_rm ){
                        XX %<>% dplyr::filter( !is.na( .data$x ) & !is.na( .data$y ) )
                        }
                        XX %<>%
                        dplyr::mutate( first_day     = min( XX[,x_name], na.rm = na_rm ),
                                       last_day      = max( XX[,x_name], na.rm = na_rm ),
                                       x_range_of_ID = .data$last_day - .data$first_day
                                      ) %>%
                        maeve::round_numerics()                                   
                        return(XX)
                     }
               ) # end of 'plyr::ddply(...'

    ## Subtract out starting value for each ID?       
    if( subtract_starting_value == TRUE ){
     dat %<>%
     plyr::ddply(
               .var = c( eval( group_name ), eval( subject_ID ), 'first_day', 'last_day', 'x_range_of_ID' ),
               .fun = function( XX, na_rm = na.rm ){
                   baseline_y <- XX %>% dplyr::filter( .data$x <= .data$first_day ) %>% dplyr::select( .data$y ) %>% unlist
                   XX %<>% dplyr::mutate( y = .data$y - baseline_y ) %>% maeve::round_numerics()
                   return(XX)
               }
               ) # end of 'plyr::ddply(...'
    }

    ## Call DescTools::AUC() for each ID.
    ## If the x-range is zero, return NA.
    ## If the x-range is positive, divide the AUC for each ID by
    ##    half the square of the x-range, by default. Other denominators
    ##    are determined by the "norm_method" parameter.
    dat_ID <-
     dat %>%
     plyr::ddply(
               .var = c( eval( group_name ), eval( subject_ID ) ),
               .fun = function(XX, na_rm = na.rm, AUC_method = method, norm_method = xrange_norm_method ){
                       ##
                       x_range_of_ID = unique( XX[,'x_range_of_ID'] )
                       if( length( x_range_of_ID ) > 1 ){
                         stop('error in summarize_by_id(): there should be just one value for ID-level x-range')
                       }
                       ##
                       if( x_range_of_ID <= 10^(-12) ){
                         if( singleton_to_NA ){
                           value = NA # set cases with 1 observation to NA value
                         } else{
                           value = 0  # set cases with 1 observation to zero value
                               }
                       } else{
                           
                         if( metric == 'AUC' ){
                             value = DescTools::AUC( x = XX$x, y = XX$y, method = AUC_method )
                             norm_value =
                               switch( norm_method,
                                       'none' = 1,
                                       'xrange' = x_range_of_ID,
                                       'slope_equivalent' = ( 0.5 * x_range_of_ID^2 ), ## empirical eGaIT
                                       stop('error in maeve::summarize_by_id(): xrange_norm_method must match one of {"none","xrange","slope_equivalent"}')
                                      )
                             value = value / norm_value
                         } ## end of 'if( metric == 'AUC' ){...'

                         if( metric == 'linear' ){
                             value =  stats::lm( y ~ x, data = XX )$coef[['x']]
                         } ## end of 'if( metric == 'linear' ){...'

                         if( metric == 'ITGR' ){
                             value =  XX$y[ XX$x == max( XX$x ) ] - XX$y[ XX$x == min( XX$x ) ]
                             ##norm_value = max( XX$x ) - min( XX$x )
                             norm_value =
                               switch( norm_method,
                                       'none' = 1,
                                       'xrange' = x_range_of_ID,
                                       'slope_equivalent' = x_range_of_ID, ## for ITGR, "slope_equivalent" scaling just divides by the x-range --> "empirical eDOT"
                                       stop('error in maeve::summarize_by_id(): xrange_norm_method must match one of {"none","xrange","slope_equivalent"}')
                                      )
                             value = value / norm_value
                         } ## end of 'if( metric == 'ITGR' ){...'
                           
                       }
                       out_DF <-  data.frame( XX[ 1, c( group_name, subject_ID, 'first_day', 'last_day' ) ], value = value )
                       colnames( out_DF )[5] <- metric
                       return( out_DF )
                     } ## end of '.fun = function(XX, ...'
                 ) ## end of 'plyr::ddply(...'
       
      return( dat_ID )
       
    } ## end of 'auc_by_id <- function(...'
