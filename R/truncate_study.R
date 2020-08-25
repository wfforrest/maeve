#' Truncate x-range for a data-frame based on x-axis values and optionally y-axis response values.
#'
#' Takes a data frame and character identifiers for various columns; returns a logical vector the same length as the number of rows in the data frame.
#'
#' @param study_data_frame data.frame with data to filter.
#' @param group_name             character with column name of treatment group factor.
#' @param subject_ID             character with column name of subject ID factor.
#' @param x_name                 character with column name of x-axis numeric variable
#' @param endpoint_name          optional character with column name of a y-axis numeric variable.
#' @param fit optional           character with column name of predicted values for a y-axis numeric variable.
#' @param overall_x_min          numeric hard limit on the lower range of x-values.
#' @param overall_x_max          numeric hard limit on the upper range of x-values.
#' @param min_n_in_group         numeric x-values in a group will be truncated to last x-value when at least this number are still in the group.
#' @param min_frac_in_group      numeric x-values in a group will be truncated to last x-value when at least this fraction are still in the group.
#' @param min_frac_in_study      numeric x-values in a group will be truncated to last x-value when at least this fraction are still in the study..
#' @param print_warning          logical whether to print warnings.
#' @param droplevels             logical whether to restrict factors to levels present in the data frame provided.
#' @param drop_singletons        logical whether to exclude *all* observations from a group if, after truncation, it has no more than one observation per subject.
#' @param drop_singleton_IDs     logical whether to exclude *all* observations for one ID   if, after truncation, it has no more than one observation total.
#' @param singleton_number       numeric If all IDs in a group have "singleton_number" or few observations and drop_singletons == TRUE, the group will get dropped.
#' @param truncation_return_type character string to describe output content.  Must be one of "logical", "data.frame", or "list".
#' @param reset_names            logical whether to reset the column names to original values in optional output.
#'
#' @return a logical vector length equal to the number of rows in study_data_frame.
#'
#' @examples
#'  cat('no working example yet')
#'
#' @author Bill Forrest \email{forrest@@gene.com}
#'
#' @keywords truncate
#'
#' @seealso \code{\link{data.frame}}
#' @export
#'
 truncate_study <-
     function(
              study_data_frame,
              group_name     = maeve_options('group_name'),
              subject_ID     = maeve_options('subject_ID'),
              x_name         = maeve_options(    'x_name'),
              endpoint_name  = NULL,
              fit            = NULL,
              ##
              overall_x_min  = maeve_options('overall_x_min'),
              overall_x_max  = maeve_options('overall_x_max'),
              ##
              min_n_in_group    = maeve_options('min_n_in_group'),  # all times in a group are truncated when less than this number of subjects are left in the group.
              min_frac_in_group = maeve_options('min_frac_in_group'),  # all times in a group are truncated when less than this fraction of subjects are left in the group.
              ##
              min_frac_in_study = maeve_options('min_frac_in_study'),  # all times in a study are truncated when less than this fraction of subjects are left in the entire study.
              ##
              print_warning          = TRUE,
              droplevels             = TRUE,  # ensure that only factor levels actually in the data are used?
              drop_singletons        = FALSE, # drop *all* observations from a group if, post-truncation, it has only 1 observation per remaining subject.
              drop_singleton_IDs     = TRUE,  # drop *all* observations for an ID    if, post-truncation, it has only 1 observation total.
              singleton_number       = 1,     # If all IDs in a group have "singleton_number" or few observations and drop_singletons == TRUE, the group will get dropped.
              truncation_return_type = maeve_options('truncation_return_type'), # return type from this function.
              reset_names            = TRUE   # reset data.frame names to original in optional output?
              ){ # start of function

       ## Check whether group_name, subject_ID, and x_name are all
       ## columns of factors (group_name & subject_ID) or numerics (x_name)
       ## in "study_data_frame" passed to the function.  A 3-step check:
       ##
       ## Is each required name a character string?
       stopifnot( is.character( group_name ) &
                  is.character( subject_ID ) &
                  is.character( x_name )
                 )
       ##
       ## Is each character string a column name in study_data_frame?
       stopifnot( all( c( group_name, subject_ID, x_name ) %in% colnames( study_data_frame ) ) )
       ##
       ## Are the columns for group_name and subject_ID factors?  Is the column for x_name numeric?
       stopifnot( is.factor(  study_data_frame[ , group_name ] ) &
                  is.factor(  study_data_frame[ , subject_ID ] ) &
                  is.numeric( study_data_frame[ , x_name     ] )
                 )

       
       ## Check whether the optional observed AND predicted endpoint column names
       ## are character strings or NULL.  This function requires that either both
       ## are given or neither, since we can't do anything with one anyway.
       ## 
       ##
       stopifnot( is.null( endpoint_name ) | is.character( endpoint_name ) ) # NULL (default) or character (matching a column name).
       stopifnot( is.null(           fit ) | is.character(           fit ) ) # NULL (default) or character (matching a column name).

       if( ## stop if one of them is character, but the other is NULL.  Can't use such discrepant types for filtering.
                 ( is.character( endpoint_name ) &   is.null(      fit ) ) |
                 ( is.null(      endpoint_name ) &   is.character( fit ) )
            ){
              stop('error in maeve:truncate_study(): either both or neither of "endpoint_name" and "fit" must be provided character strings')
       }


       ## Check whether character strings for observed & fitted endpoint values are:
       ## (1) column names of the data.frame in "study_data_frame"
       ## (2) pointing us to *numeric* columns.
       if( is.character( endpoint_name ) & is.character( fit ) ){

         ## check that 'endpoint_name' is in the column names of the data frame
         if( ! endpoint_name %in% colnames( study_data_frame ) ){
             ##
             stop_msg_string <- paste( colnames( study_data_frame ), collapse = ' ' )
             stop( 'error in maeve:truncate_study(): provided "endpoint_name" character string ',
                    endpoint_name,
                   'is not in the column names of the data frame provided:\n',
                    stop_msg_string
                  )
         } # end of 'if( ! endpoint_name %in% colnames( study_data_frame ) ){...'

         stopifnot( is.numeric( study_data_frame[, endpoint_name ] ) ) # confirm that the column is actually numeric.

         ## check that 'fit' is in the column names of the data frame
         if( ! fit %in% colnames( study_data_frame ) ){
             ##
             stop_msg_string <- paste( colnames( study_data_frame ), collapse = ' ' )
             stop( 'error in maeve:truncate_study(): provided "fit" character string ',
                    fit,
                   'is not in the column names of the data frame provided:\n',
                    stop_msg_string
                  )
             } # end of 'if( ! fit %in% colnames( study_data_frame ) ){...'

         stopifnot( is.numeric( study_data_frame[, fit ] ) ) # confirm that the column is actually numeric.

       } # end of 'if( is.character( endpoint_name ) & is.character( fit ) ){...'



       
       ## select out only the columns that will be used and assign within-function names.
       ##
       ## If endpoint_name and fit were left as NULL, then they are *not* included.

       study_data_frame_original <- study_data_frame
       
       if( is.null( endpoint_name ) ){
             study_data_frame %<>%
                 dplyr::select( !!dplyr::sym( group_name ), !!dplyr::sym( subject_ID ), !!dplyr::sym( x_name ) ) %>%
                 dplyr::rename( 'group'  = !!dplyr::sym( group_name ),
                                   'ID'  = !!dplyr::sym( subject_ID ),
                                    'x'  = !!dplyr::sym( x_name )     
                              )
       } # end of 'if( is.null( endpoint_name ) ){'

       ##
       ## If endpoint_name and fit are character strings, then they *are* included.
       if( ! is.null( endpoint_name ) ){
             study_data_frame %<>%
                 dplyr::select( !!dplyr::sym( group_name ),
                                !!dplyr::sym( subject_ID ),
                                !!dplyr::sym( x_name ),
                                !!dplyr::sym( endpoint_name ),
                                !!dplyr::sym( fit )
                               ) %>%
                 dplyr::rename( 'group'  = !!dplyr::sym( group_name ),
                                   'ID'  = !!dplyr::sym( subject_ID ),
                                    'x'  = !!dplyr::sym( x_name ),
                                           ##
                                    'y'  = !!dplyr::sym( endpoint_name ),
                               'y_pred'  = !!dplyr::sym( fit )
                              )
       } # end of 'if( ! is.null( endpoint_name ) ){...'


       ## Do we want to drop to only the levels present?
       ##
       if( base::isTRUE( droplevels ) ){
           study_data_frame %<>% base::droplevels()
       }



       ## There are three criteria to evaluate.  Each of these will put an upper
       ## bound on the x-value allowed.  In two cases the upper bound is group-specific,
       ## while in the third it is study-wide.  We find all three upper bounds as vectors
       ## with the same length as study_data_frame, then take the minimum across the 3 times.
       ##
       ## For the case with 'y' or 'y_pred' (i.e., 'endpoint_name' or 'fit'), there will be a
       ## fourth criterion included near the end.       

         
       ## make an interim data frame with 1 row per group
       n_in_group <-
       study_data_frame                   %>%
       dplyr::group_by( group )           %>%
       dplyr::summarise( group_n = nlevels( base::droplevels( ID ) )  )

       
       ## data frame with 1 row per subject_ID.
       x_min_max_by_ID <-
       study_data_frame                   %>%
       ## There should not be any NA values, but drop them if there are:
       dplyr::filter( !is.na( group ) &
                      !is.na( ID )    &
                      !is.na( x )
                     )                    %>%
       ##
       dplyr::group_by( group, ID )       %>%
       ## Find & record the lowest & highest x-value for each subject identifier:
       dplyr::summarise(
                           x_min = min( x ),
                           x_max = max( x )
                       )                  %>%
       ##
       dplyr::left_join( n_in_group, . , by = 'group' ) %>%
       dplyr::select( group, ID, group_n, x_min, x_max ) %>%
       ##
       data.frame()


       ## (1) 'min_n_in_group' should be the minimum number of subjects out through which prediction takes place for the group.
       ## E.g.,
       ## if "min_n_in_group  = 0", then there is no x-truncation *at all* based on this criterion.
       ## if "min_n_in_group  = 1", then there is x-truncation after the last subject has its last measurement.
       ##                           This should be the same as "min_n_in_group  = 0" if you have only the observed
       ##                           data.frame, but can be different if you have a data.frame with predicted values
       ##                           which can be extrapolated in some groups.
       ## if "min_n_in_group  = 3", then there is x-truncation after the *third*-to-last subject has its last measurement.
       ## if "min_n_in_group == nlevels( ID )", then there is x-truncation after the first subject to go has its last measurement.
       ## if "min_n_in_group  > nlevels( ID )", then all predictions will be set to NA, with a warning.
       ## 
       ## Below we determine an x-value "x_max_via_min_n_in_group" based on
       ## the x-values across group_name & subject_ID, then record it for
       ## truncation at the end.

       stopifnot( is.numeric( min_n_in_group ) )
       min_n_in_group <- ceiling( min_n_in_group ) # round up if it's between two integers.
       ##

       ## We must determine this x-value separately for each group.
       ## While clunky, it seemed easiest -- at least initially -- to just create a vector and do calculations in a loop.
       x_max_via_min_n_in_group_VECTOR <- rep( Inf, nlevels( x_min_max_by_ID$group ) )

       if( min_n_in_group > 0 ){ # If min_n_in_group <= 0, just keep all the times as positive infinity.
       ## 
         for( ii in 1:nlevels( x_min_max_by_ID$group ) ){
              ##
              loop_data <-
                x_min_max_by_ID %>%
                dplyr::filter( group == ( levels( x_min_max_by_ID$group )[ii] ) ) %>%
                droplevels
              ##
              if( min_n_in_group > unique( loop_data$group_n ) ){
                ##
                x_max_via_min_n_in_group_VECTOR[ii] <- -Inf # No admissible x-values at all.
                ##
                if( print_warning ){
                warning( paste( 'In maeve::truncate_study(): min_n_in_group is more than',
                                'total number in the group for',
                                unique( as.character( loop_data$group ) ),
                                'so all data for this entire group will be truncated.'
                               )
                        )
                } # end of 'if( print_warning ){...'
              } # end of 'if( min_n_in_group > unique(loop_data$group_n) ){...'
              ##
              if( min_n_in_group <= unique( loop_data$group_n ) ){ # This is what usually happens...
                
              ## If we're still going, then "min_n_in_group" is an integer from 1 to the number of ID values in the group.
              ## Find the ranks for individual 'x_max" values in decreasing order (i.e., "largest x_max" <--> "1", etc.).              
              rank_from_min_n_in_group <- with( loop_data, rank( -x_max, ties = 'random' ) ) # "-x_max" <--> "rank in decreasing order".

              ## Next step in words,
              ## "Take the ranks for the x_max values for each subject_ID in the group, where "1" is the highest.  Then,
              ##  find all the x_max values with ranks at or below the integer 'min_n_in_group' and take the minimum of
              ##  such x_max values, and record it in position 'ii' of the output vector."
              
              x_max_via_min_n_in_group_VECTOR[ii] <- with( loop_data, min( x_max[ rank_from_min_n_in_group <= min_n_in_group ] ) )
              
              } # end of 'if( min_n_in_group <= unique(loop_data$group_n) ){ ...'
         } # end of 'for( ii in 1:nlevels( x_min_max_by_ID$group ) ){...'
       } # end of 'if( min_n_in_group > 0 ){...'


       
       
       ## (2) 'min_frac_in_group' should be the minimum fraction of subjects in a group out through which prediction takes place for that group.
       ## E.g.,
       ## if "min_frac_in_group  = 0",    then there is no x-truncation *at all* based on this criterion.
       ## if "min_frac_in_group  = 0.25", then there is x-truncation after the largest x-value from the first subject in the top quarter of x_max values.
       ## if "min_frac_in_group  = 1",    then there is x-truncation after the largest x-value from *any* subject (so after the *smallest* x_max value).
       ##                                 
       stopifnot( is.numeric( min_frac_in_group ) )
         
       if( min_frac_in_group < 0 | min_frac_in_group > 1 ){
           stop('error in maeve::truncate_study(): min_frac_in_group must be a numeric in the range [0, 1]')
       }

       x_max_via_min_frac_in_group_VECTOR <- rep( Inf, nlevels( x_min_max_by_ID$group ) )

       if( min_frac_in_group > 1e-12 ){ # if min_frac_in_group == 0, keep upper bounds at positive infinity.
         ##
         for( ii in 1:nlevels( x_min_max_by_ID$group ) ){
              ##
              loop_data <-
                x_min_max_by_ID %>%
                dplyr::filter( group == (levels( x_min_max_by_ID$group )[ii]) ) %>%
                droplevels

              ## Next steps in words,
              ## 
              ##  Take the ranks for the x_max values for each subject_ID in the group, where '1' is the lowest x_max value.
              ##  Divide by the number of distinct ID levels in the group (i.e., 'group_n') to scale these ranks
              ##  to within ( 1 / group_n, 1], then subtract the result from "1" to get values in from 
              ##  { 0, 1 / group_n, 2 / group_n, ...,  1 - (1/group_n) }.  We call this result "fraction_above" because it
              ##  quantifies the fraction of samples that fall above the x_max value for a given sample.  

                fraction_above <- with( loop_data, 1 - rank( x_max, ties = 'random' ) / group_n )

              ##
              ##  Note that the extreme value of 'fraction_above' is
              ##  (a) '1 - (1 / group_n)', corresponding to the subject_ID with the *smallest* x_max value, and...
              ##  (b) "0.0", corresponding to the subject_ID with the *largest* x_max value.
              ##
              ##  When the exit of a subject_ID makes the fraction_above fall strictly below "min_frac_in_group", 
              ##  record the x_max value for that subject as the "x_max_via_min_frac_in_group_VECTOR[ii]" value.
              ##
              ##  Two special cases to note:
              ##  (a) When min_frac_in_group == 0, the fraction_above remaining in the group cannot get below this, so we just leave
              ##      the ii'th group value "x_max_via_min_frac_in_group_VECTOR[ii]" at positive infinity.
              ##
              ##  (b) When min_frac_in_group > 0 but sufficiently small (smaller than  1 / group_n ), then 
              ##      the "fraction_above" *does* get below min_frac_in_group when the *last* subject_ID exits the group,
              ##      so record "x_max_via_min_frac_in_group_VECTOR[ii]" as the largest x_max value in that group.
              ##
 
              eligible <- fraction_above < min_frac_in_group # logical indicator for x_max values that could be our threshold.

              if( any( eligible ) ){ # should happen only if 
                     x_max_via_min_frac_in_group_VECTOR[ii] <- min( loop_data$x_max[ eligible ] )
                } else{ # min ( fraction_above ) is zero, but min_frac_in_group > 0, so this shouldn't happen.
                     stop('error in maeve::truncate_study(): something is wrong; min_frac_in_group > 0 and min( fraction_above ) should be zero')
               } 
         } # end of 'for( ii in 1:nlevels( x_min_max_by_ID$group ) ){...'
       } # end of 'if( min_frac_in_group > 1e-12 ){...'


       
       ## (3) Check for when the fraction of animals remaining across *all* groups
       ##     falls below min_frac_in_study.  This will provide a value at which
       ##     times are truncated.
       ##
       stopifnot( is.numeric( min_frac_in_study ) )
         
       if( min_frac_in_study < 0 | min_frac_in_study > 1 ){
         stop('error in maeve::truncate_study(): min_frac_in_study must be a numeric in the range [0, 1]')
       }

       x_max_via_min_frac_in_study <-  Inf # No truncation on account of this criterion.
       
       if( min_frac_in_study > 1e-12 ){
         ## which x_max values are in the upper "min_frac_in_study" percentile?
         eligible <- with( x_min_max_by_ID, ( 1 - ( (rank( x_max, ties = 'random' ) - 1) / length(group_n) ) ) <= min_frac_in_study )
         ##
         if( !any( eligible ) ){
             x_max_via_min_frac_in_study <- max( x_min_max_by_ID$x_max )
         } else{
             x_max_via_min_frac_in_study <- min( x_min_max_by_ID$x_max[eligible] )
         }

       } # end of 'if( min_frac_in_study > 1e-12 ){...'

         
       stopifnot( 1 == length( x_max_via_min_frac_in_study ) ) # sanity check.


       ## Put together the three thresholding criteria just computed so we can use them to truncate the data set:
       x_min_max_by_ID %<>%
       dplyr::left_join(
                         data.frame( group = factor(      x = levels( x_min_max_by_ID$group ),
                                                     levels = levels( x_min_max_by_ID$group )
                                                    ),
                                     x_max_via_min_n_in_group    = as.numeric( x_max_via_min_n_in_group_VECTOR ),  
                                     x_max_via_min_frac_in_group = as.numeric( x_max_via_min_frac_in_group_VECTOR ),
                                     x_max_via_min_frac_in_study = as.numeric( x_max_via_min_frac_in_study )
                                    ), # end of 'data.frame( group = ...'
                         by = 'group'
                        ) # end of 'dplyr::left_join(...'

       
       ## include truncation from y-values if available:
       if( !is.null( fit ) ){
         ##
         in_y_range_vec <-
            with( study_data_frame,
                  y_pred <= max( y, na.rm = TRUE ) &
                  y_pred >= min( y, na.rm = TRUE )
                 )
       }
         
       if( is.null( fit ) ){
         in_y_range_vec <- rep( TRUE, nrow( study_data_frame ) )
       }


       study_data_frame_augmented <-
         dplyr::left_join( study_data_frame, x_min_max_by_ID, by = c('group','ID') )

       ## Aggregation of three x_max summary measures for gRED:
       ##       
       ## (1) At a timepoint for which there are fewer than 3 mice in a group, truncate remaining time points from that group.
       ##       
       ## (2) At a timepoint for which
       ##     (i)  *fewer* than half the mice remain from a group 
       ##          **AND**  
       ##     (ii) *fewer* less than 30% of the mice remain from a the entire study,
       ##     truncate remaining time points from that group.
       ##        
       ## To implement (2), we take the row-wise maximum of "x_max_via_min_frac_in_group" and "x_max_via_min_frac_in_study",
       ## since once we have exceeded *both* of these x_max times, then the two conditions (2)(i) and (2)(ii) are both TRUE.       
       ##            
       ## To implement (1), we simply record "x_max_via_min_n_in_group".
       ##
       ## To implement the case when (1) **OR** (2) is TRUE, we take the row-wise minimum of "x_max_via_min_n_in_group"
       ## and the row-wise maximum of "x_max_via_min_frac_in_group" and "x_max_via_min_frac_in_study".
       ##       
       study_data_frame_augmented %<>%
       dplyr::mutate( frac_max = base::pmax( x_max_via_min_frac_in_group, x_max_via_min_frac_in_study ),
                      frac_and_group_min = base::pmin( frac_max, x_max_via_min_n_in_group )
                     ) %>%
       dplyr::mutate( include_x = (x >= overall_x_min) & (x <= overall_x_max) & (x <= frac_and_group_min) )

       

       ## If, *after* applying the truncation criteria, a group has no subjects with multiple observations,
       ## then it cannot be used for longitudinal modeling, and will result in an error when the truncated
       ## data are modeled.  To preclude such an error, this section will automatically set *all* the observations
       ## from such a group to have "include_x = FALSE' so that they can be filtered out from the data set passed
       ## to the modeling functions.         
       if( drop_singletons ){
       ## Split by group and count the number of observations per subject.
       ## If the maximum number of observations per subject in a group is 'singleton_number',
       ## drop the group.  By default, 'singleton_number = 1'.

       study_data_frame_augmented %<>%
       ## count the number of time points per ID in the truncated data           
       dplyr::group_by(   group, ID )  %>%
       dplyr::summarise(  n_included_timepoints_by_ID = sum( include_x ) ) %>% # count up number of included time points for each ID.
       ## determine the maximum number of time points per ID for each group
       dplyr::group_by(   group ) %>%
       dplyr::summarise(  max_n_timepoints_by_group  = max( n_included_timepoints_by_ID ) ) %>%
       ## Define 'group_inclusion = TRUE' if group has at least one subject with requisite number of observations:
       dplyr::mutate(     group_inclusion = max_n_timepoints_by_group > singleton_number ) %>%
       ## Merge 'group_inclusion' into the original data frame.
       dplyr::left_join( x = study_data_frame_augmented, y = . , by = 'group' ) %>%
       ## Further filter 'include_x' by also requiring 'group_inclusion':
       dplyr::mutate(     include_x = include_x & group_inclusion ) %>%
       ## MJ: filter animals with only 1 value
       dplyr::group_by(   ID ) %>%
       ## WF:: Added 20200424 to avoid downstream stop in predict_study() from model_study(), which calls truncate_study().
       ## The 'drop_singleton_IDs' parameter was just added, and should keep Michal Jakubczak's (MJ's) desired behavior
       ## when it is set to TRUE (the default), but when it is set to FALSE, it will just make the next 'mutate(...)'
       ## line irrelevant -- the augmenting values to 'include_x & ...' will all be TRUE.
       ## dplyr::mutate(     include_x = include_x &     sum(include_x) > 1 ) %>%
       dplyr::mutate(     include_x = include_x & ( ( sum(include_x) > 1 ) | !drop_singleton_IDs ) ) %>%
       ## leave results in a data.frame
       data.frame()

       } # end of 'if( drop_singletons ){...'





       if( reset_names ){ # change the within-function names back to the ones that were passed.
         ##
         x_min_max_by_ID %<>%
                 dplyr::rename( !!group_name  := group ,
                                !!subject_ID  := ID
                              )
         ##         
         study_data_frame_augmented %<>%
                 dplyr::rename( !!group_name  := group ,
                                !!subject_ID  := ID ,
                                !!x_name      := x 
                              )
         ##
         if( !is.null( fit ) ){ # in this case, need to switch back response + fit names, too.
         study_data_frame_augmented %<>%
                 dplyr::rename( !!endpoint_name := y ,
                                !!fit           := y_pred
                              )
         } # end of 'if( !is.null( fit ) ){...'

       } # end of 'if( reset_names ){...'


       ## Return one of three data types with truncation results.              

       ## Return from the original data frame those rows that pass the 'x-value' filter.
       if( truncation_return_type == 'data.frame' ){
          return( study_data_frame_original[ study_data_frame_augmented$include_x , ] )
       } # end of if( truncation_return_type == 'data.frame' ){...


       ## Return from the logical vector of the 'x-value' filter.
       if( truncation_return_type == 'logical' ){
          return( study_data_frame_augmented$include_x )
       } # end of 'if( truncation_return_type == 'logical' ){...'

       ## Return a list with lots of output.
       ## Note that this include 'y_pred_in_y_range' for setting to NA predicted response values outside the data range.
       if( truncation_return_type == 'list' ){
          return( list( include_x                  = study_data_frame_augmented$include_x,
                        y_pred_in_y_range          = in_y_range_vec,                      
                        study_data_frame           = study_data_frame_original,
                        study_data_frame_augmented = study_data_frame_augmented,
                        x_min_max_by_ID            = x_min_max_by_ID,
                        min_n_in_group             = min_n_in_group,
                        min_frac_in_group          = min_frac_in_group,
                        min_frac_in_study          = min_frac_in_study,
                        overall_x_min              = overall_x_min,
                        overall_x_max              = overall_x_max
                       )
                 )
       } # end of 'if( truncation_return_type == 'list' ){...'

       
       stop('error in maeve::truncate_study(): should have returned a value before this point')
       
   } # end of 'truncate_study <- function(...){...'
