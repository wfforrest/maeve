#' Restrict longitudinal data to a range, interpolating at endpoints as needed.
#'
#' Filter out only the values in the interval given for each subject_ID,
#' but also interpolate the response endpoint value at the interval end points
#' based on the method that will be used to find the AUC (trapezoid/linear, step, spline).
#'
#' @author Bill Forrest <forrest@@gene.com>
#'
#' @param study_data_frame        data.frame a "well-prepared" data.frame (e.g., processed through maeve::check_study_data_frame() is recommended but not required) with the study data and appropriate columns.
#' @param method                  character name method for computing AUC.  Passed directly to DescTools::AUC( ..., method = ... ).
#' @param xmin                    numeric  left / lower  end of the interval.
#' @param xmax                    numeric right / uppper end of the interval.
#' @param VST                     logical   whether to apply a variance stabilizing transform to endpoint_name values.
#' @param check_data              logical whether to pass "study_data_frame" through maeve::check_study_data_frame() to check for problems.
#' @param return_all_columns      logical whether to return all the columns in the original data frame, or only the four for group_name, subject_ID, x_name, and endpoint_name. 
#' @param print_warnings          logical whether to print warnings in odd circumstances.
#' @param unwindowed_to_NA        logical whether to set the values for "bystander" numeric vector entries (which are not being interpolated) to NA at the newly added endpoints.  Recommended.
#' @param group_name              character column name for the group name factor
#' @param subject_ID              character column name for the subject name factor
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
#'  vismo_interval_by_id <- window_by_id( vismo21, xmin = 4.5, xmax = 15.5 )
#'
#'
#' @author Bill Forrest \email{forrest@@gene.com}
#'
#' @references \url{www.r-project.org}
#'
#' @keywords data.frame
#' @export
#'


window_by_id <-
  function( study_data_frame,
            method = c( 'trapezoid', 'step', 'spline', 'linear' ),
            xmin = -Inf,
            xmax =  Inf,
            ##
            VST  = TRUE, # If TRUE, add to endpoint & apply variance-stabilizing transform before AUC
            ##
            check_data         = TRUE,
            return_all_columns = FALSE, ### if FALSE, return a minimal set of columns.
            print_warnings     = TRUE,  ### if TRUE, print warnings in weird corner cases.
            unwindowed_to_NA   = TRUE,  ### if TRUE, fill in endpoints of windowed vectors with NAs.
            ##
            group_name      = maeve_options("group_name"),
            subject_ID      = maeve_options("subject_ID"),
            x_name          = maeve_options("x_name"),
            endpoint_name   = maeve_options("endpoint_name"),
            add_to_endpoint = maeve_options("add_to_endpoint"),
            trans_func_char = maeve_options("trans_func_char"),
            inv_func_char   = maeve_options("inv_func_char"),
            test_func_x     = maeve_options("test_func_x")
            ##
           ){

### create "windowing" capability so that the data for ID-level AUCs via auc_by_id() are
### (1) truncated to fall within an interval, but
### (2) interpolated to have values exactly at the interval endpoints for IDs with values
###     outside the interval.  The interpolation method will be determined by the "method"
###     parameter in maeve::auc_by_id(), which is passed directly to DescTools::AUC.
###
###   The 'method' options have the following effects:
###   "method = 'trapezoid'" --> "approxfun( ..., method = 'linear' )"
###   "method = 'step'     " --> "approxfun( ..., method = 'constant', f = 0 )"
###   "method = 'spline'   " --> "splinefun( ... )
###

    method = match.arg( method, several.ok = FALSE )

    windowed_data_frame <- study_data_frame
        
    ### Transform endpoint?
    if( VST == TRUE ){ # Apply a Variance Stabilizing Transformation:
       ## Check the transformation function & assign it & its inverse:
       func_list  <- check_trans_func( trans_func_char, inv_func_char, test_func_x )
       trans_func <- func_list$trans_func;
         inv_func <- func_list$inv_func;
       ## Define 'y' as the transformed endpoint:
       windowed_data_frame %<>% dplyr::mutate( y = trans_func( get( endpoint_name ) + add_to_endpoint ) )
    } else{
       ## Define 'y' as the original, untransformed endpoint:
       windowed_data_frame %<>% dplyr::mutate( y = get( endpoint_name ) )
    }

    ## if a variance-stabilizing transform (VST) is applied,
    ## then the transformed endpoint is called 'y'.  Otherwise
    ## we keep the existing name.
    endpoint_name_to_use <- ifelse( VST, 'y', eval( endpoint_name ) )
        
    windowed_data_frame %<>%
    plyr::ddply( .var = c( eval( group_name ), eval( subject_ID ) ),
                 .fun = function( X,
                                  interp_method = method,
                                  endpoint_local = endpoint_name_to_use,
                                  interp_function = endpoint_interp_function
                                 ){
                       
                       xv <- X[ , x_name ]
                       yv <- X[ , endpoint_local ]

                      ## NB: This is a *different* interpolation function for each subject_ID.
                      endpoint_interp_function <-
                          switch( interp_method,
                                 'trapezoid' = stats::approxfun( xv, yv, method = 'linear' ),
                                 'linear'    = stats::approxfun( xv, yv, method = 'linear' ), # synonym for 'trapezoid' in this context.
                                 'step'      = stats::approxfun( xv, yv, method = 'constant', f = 0 ),
                                 'spline'    = stats::splinefun( xv, yv, method = 'fmm' )
                                )
                       

                       ### Figure out which are the numeric columns that are uninvolved and set them to NA.
                       ## Get the column names of all the "numeric" or "integer" columns in 'X'
                       numeric_column_names <- X %>% lapply( class ) %>% unlist %>% grep( 'numeric', ., value = TRUE ) %>% names
                       integer_column_names <- X %>% lapply( class ) %>% unlist %>% grep( 'integer', ., value = TRUE ) %>% names
                       numeric_column_names <- c( numeric_column_names, integer_column_names ) # just glob them together for this.
                       
                       unwindowed_numeric <-
                         numeric_column_names %>%
                         ## Next line is NOT "NA" for the 'x_name' and 'endpoint_local' columns.
                         ## Any other columns are what we term "bystander" numeric columns.
                         pmatch( table = c( eval( x_name ), endpoint_local ) ) %>%
                         ## Next two lines will subset out column names of bystander numeric columns,
                         ## which are NA when pmatch'ed against 'x_name' and 'endpoint_local':
                         is.na %>%
                         subset( numeric_column_names, . ) 
                       
                       if( length( unwindowed_numeric ) == 0 ){
                          unwindow_to_NA <- FALSE # There are no "bystander" numeric columns that need to have NA values at the endpoints.
                       }
                       
                        low_x <-  xv < xmin # "x" falls to  left of "xmin"
                       high_x <-  xv > xmax # "x" falls to right of "xmax"

                        ## Case:  All the x-values are to the left of the interval [xmin, xmax]
                        if( all( low_x ) & !any( high_x ) ){
                         if( print_warnings ){
                            current_ID = as.character( unique( X[ , eval( subject_ID ) ] ) )
                            warning( paste( 'In window_by_id(): x-values all to the left of requested interval for', eval( subject_ID ), current_ID ) )
                         }
                         return(NULL)
                        }                      
                       
                        ## Case:  All the x-values are to the right of the interval [xmin, xmax]
                        if( !any( low_x ) & all( high_x ) ){
                         if( print_warnings ){
                            current_ID = as.character( unique( X[ , eval( subject_ID ) ] ) )
                            warning( paste( 'In window_by_id(): x-values all to the right of requested interval for', eval( subject_ID ), current_ID ) )
                         }
                         return(NULL)
                        }                      

                       if( all( low_x | high_x ) & print_warnings ){
                            ## NB: interpolation of AUC can (and does) actually still happen in this case.
                            current_ID = as.character( unique( X[ , eval( subject_ID ) ] ) )
                            warning( paste( 'In window_by_id(): No x-values within requested interval for', eval( subject_ID ), current_ID ) )
                        }
                        
                        if( any( low_x ) ){ ## one or more "x" values are below the allowed "xmin"
                          ##
                          ## Make a one-row data.frame to hold the interpolated value on the left:
                          X_left <- dplyr::slice( X, 1 )
                          
                          X_left[,         x_name ] <- xmin
                          X_left[, endpoint_local ] <- interp_function( xmin )
                          if( unwindowed_to_NA ){ 
                             X_left[,unwindowed_numeric] <- NA
                          }

                          X %<>% dplyr::filter( get( x_name ) > xmin ) %>% base::rbind( X_left, . )
                         }
                       
                        if( any( high_x ) ){ ## one or more "x" values are above the allowed "xmax"
                          ##
                          ## Make a one-row data.frame to hold the interpolated value on the right:
                          X_right <- dplyr::slice( X, 1 )
                          
                          X_right[,         x_name ] <- xmax
                          X_right[, endpoint_local ] <- interp_function( xmax )
                          if( unwindowed_to_NA ){ 
                             X_right[, unwindowed_numeric ] <- NA
                          }

                          X %<>% dplyr::filter( get( x_name ) < xmax ) %>% base::rbind( X_right )
                         }

                        if( !return_all_columns ){
                           X %<>% dplyr::select( eval( group_name ),
                                                 eval( subject_ID ),
                                                 eval( x_name ),
                                                 eval( endpoint_local )
                                                )
                        }
                       
                        return( X ) # This is all for one subject_ID.

                     } ## end of '.fun = function( X, interp_method = method ){...'
                    ) ## end of 'plyr::ddply( .var = c( eval(group_name), eval(subject_ID) ),...'


    ## Back-transform the endpoint to the original scale.
    if( VST == TRUE ){ # We applied a Variance Stabilizing Transformation (VST).
       windowed_data_frame[, endpoint_name ] <- inv_func( windowed_data_frame[, endpoint_name_to_use ] ) - add_to_endpoint
       if( endpoint_name != 'y' ){
         ## If the original endpoint_name was not "y", then take out the working columns we've been calling "y":
         windowed_data_frame <- windowed_data_frame[, -which( colnames( windowed_data_frame ) == 'y' ) ]
         }
           
    } else{
       ## Define 'y' as the original, untransformed endpoint:
       windowed_data_frame[, endpoint_name ] <- windowed_data_frame[, endpoint_name_to_use ]
    }

    return( windowed_data_frame )
        
  } ## end of function definition for 'window_by_id()'.
