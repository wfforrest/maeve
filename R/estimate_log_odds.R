#' Estimate the log odds ratio and stanard error for counts of treatment relative to reference
#'
#' @author Bill Forrest <forrest@@gene.com>
#'
#' @param count_data_frame         data.frame with one row per group, total number per group, and number of positive counts out of the total
#' @param group_name_char          character name of column with group factor identifier.
#' @param reference                character name of level to use as a reference; must be in levels of factor designated by group_name_char.
#' @param number_on_first_day_char character name of column with total number in a each group.
#' @param count_desc_char          character name of column with number of positive cases in each group.
#' @param additive_offset          numeric value to add to each count in a 2 x 2 table before computing log odds ratio.  Default gives the Haldane-Anscombe-Gart correction of adding 0.5 to every count.
#' @param return_original          logical whether to return the original counts in the table with the estimated log_OR and SE_log_OR.
#'
#' @return A data.frame with log odds ratio (log_OR) and its standard error (SE_log_OR) for each non-reference group relative to reference.
#'
#' @examples
#'  test_log_odds <- data.frame( group = factor( c( 'group_01', 'group_02', 'group_03' ) ),
#'                               N_first_day = rep( 10, 3 ),
#'                               PR = c( 0, 2, 6 )
#'                              )
#'  log_odds_est <- maeve:::estimate_log_odds( test_log_odds, reference = 'group_01' )
#'
#' @author Bill Forrest \email{forrest@@gene.com}
#'
#' @references \url{www.r-project.org}
#'
#' @keywords data.frame
#'
estimate_log_odds <-
  function( count_data_frame,
            group_name_char = 'group',
            reference, ### = levels( count_data_frame[,group_name_char] )[1], 
            number_on_first_day_char = 'N_first_day',
            count_desc_char = c( 'PR', 'EOS_CR', 'PR_or_EOS_CR' )[1],
            additive_offset = 0.5, # "0.5" <--> Haldane-Anscombe-Gart correction. See A. Agresti, Biometrics (1999).
            return_original = TRUE
           ){

   stopifnot( group_name_char          %in% colnames( count_data_frame ) && is.factor(  count_data_frame[,group_name_char] ) )
   stopifnot( number_on_first_day_char %in% colnames( count_data_frame ) && is.numeric( count_data_frame[,number_on_first_day_char] ) )

   if( ! count_desc_char %in% colnames( count_data_frame ) ){ # more detailed error, since a more likely error point.
      stop_msg_string <- paste(' ', paste( colnames( count_data_frame ), collapse = ', ' )  ,' ')          
      stop( 'error in maeve::estimate_log_odds(): designated count description ',
             count_desc_char, ' ',
            'is not in column names of the supplied data.frame:  ',
             stop_msg_string
           ) # end of 'stop(...'
   }

   stopifnot( is.numeric( count_data_frame[, count_desc_char ]) ) # check that count descriptor is numeric
    
   group_name_levels <- # get character string with ordered group levels
      count_data_frame %>%
      dplyr::select( !!dplyr::sym( group_name_char ) ) %>%
      unlist %>%
      levels
    
   if( ! reference %in% group_name_levels ){
      stop_msg_string <- paste( ' ', paste( group_name_levels, collapse = ', ' ), ' ' )
      stop( 'error in maeve::estimate_log_odds(): designated reference group name ',
             reference, ' ',
            'is not in levels of supplied grouping factor:\n  ',
             stop_msg_string,
            '\n\n'
           ) # end of 'stop(...'
   } # 'if( ! reference %in% group_name_levels ){...'
    
   reference_index <- match( reference, group_name_levels )
   stopifnot( is.numeric( reference_index ) & (length( reference_index ) == 1) )

   log_OR <- SE_log_OR <- rep( NA, nrow( count_data_frame ) )
    
   log_OR[ reference_index ] <- SE_log_OR[ reference_index ] <- 0
    
   for( ii in setdiff( 1:length( group_name_levels ), reference_index ) ){
      current_tab <-
        count_data_frame %>%
        dplyr::slice(  c( reference_index, ii ) ) %>%
        dplyr::select( !!dplyr::sym( number_on_first_day_char ), !!dplyr::sym( count_desc_char ) ) %>%
        dplyr::mutate( no_response = get( number_on_first_day_char ) - get( count_desc_char ),
                          response =                                   get( count_desc_char )
                      ) %>%
        dplyr::select( no_response, response )                

      current_mat <- as.matrix( current_tab ) + additive_offset # 2 x 2 matrix, then add, e.g., 0.5 to each cell.

         log_OR[ ii ] <- sum( log( current_mat ) * matrix( c( 1, -1, -1, 1 ), 2, 2 ) )
      SE_log_OR[ ii ] <- sqrt( sum( c( 1 / current_mat ) ) )
      
   } # end of 'for( ii in setdiff( 1:length( group_name_levels ), reference_index ) ){...'
      
   if( ! return_original){
       ##
       data.frame( factor(group_name_levels, levels = group_name_levels), log_OR, SE_log_OR ) %>%
       setNames( c( group_name_char, 'log_OR', 'SE_log_OR' ) ) %>%
       return()           
       ##
   } else{ # include input data with returned log odds ratio and standard error:
       ##
       data.frame(
                  factor(group_name_levels),
                  count_data_frame[,number_on_first_day_char],
                  count_data_frame[,count_desc_char],
                  log_OR,
                  SE_log_OR
                  ) %>%
       setNames( c( group_name_char, number_on_first_day_char, count_desc_char, 'log_OR', 'SE_log_OR' ) ) %>%
       return()
       ##
   } # end of 'else{...'
    
  } # end of "estimate_log_odds <- function(....){..."
