#' Calculate group-level summaries based solely on raw data (i.e., that do not require any modeling).
#'
#' @author Bill Forrest <forrest@@gene.com>
#'
#' @param study_data_frame a well-prepared data.frame with the study data and appropriate columns.
#' @param group_name      character name of treatment regimen factor
#' @param subject_ID      character identifier for individual within a group.
#' @param x_name          character name of independent variable (usually time or concentration).
#' @param endpoint_name   character name of   dependent variable (e.g., tumor volume, body weight).
#' @param reference       character name of designated reference group.  Must be a level from the factor named by group_name.
#' @param response        character name of by-group count summary.
#' @param na.rm           logical   whether to remove NA values when computing summaries.
#' @param EOS_CR_minval   numeric   passed to internal function to decide what constitutes a "complete response" for the the End-Of-Study Complete Response (EOS_CR) value on the original, untransformed response scale.
#' @param PR_threshold    numeric   passed to internal function to decide what constitutes a "partial response" for the PR value.  Fractional reduction is computed by-ID, from an ID-specific baseline value on the original, untransformed scale.
#' @param additive_offset numeric   value to add to each count in a 2 x 2 table before computing log odds ratio.  Default gives the Haldane-Ascombe-Gart correction of adding 0.5 to every count.
#'
#' @return An R data.frame with one row per group.
#'
#' @examples
#'  vismo_tally <- tally_study( vismodegib )
#'
#' @author Bill Forrest \email{forrest@@gene.com}
#'
#' @references \url{www.r-project.org}
#'
#' @keywords data.frame
#' @export
#'
tally_study <-
    function( study_data_frame,
                group_name = maeve_options('group_name'),
                subject_ID = maeve_options('subject_ID'),
                    x_name = maeve_options('x_name'),
             endpoint_name = maeve_options('endpoint_name'),
                 reference = levels( study_data_frame[ , group_name ] )[1],
                  response = c( 'none', 'PR', 'EOS_CR', 'PR_or_EOS_CR' )[1],
             EOS_CR_minval = maeve_options('EOS_CR_minval'),
              PR_threshold = maeve_options('PR_threshold'),
           additive_offset = 0.5, # "0.5" <--> Haldane-Ascombe-Gart correction. See A. Agresti, Biometrics (1999).             
                     na.rm = TRUE
             ){

       allowed_response_types <- c( 'none', 'PR', 'EOS_CR', 'PR_or_EOS_CR' )
      
       mismatched_reponse <- response[ which( is.na( match( response, allowed_response_types ) ) ) ] ## setdiff( response, allowed_response_types )
       
       if( length( mismatched_reponse ) > 0 ){
         stop_msg_string_01 <- paste( mismatched_reponse, collapse = '  ' )
         stop_msg_string_02 <- paste( allowed_response_types, collapse = '  ')
         stop( 'error in maeve::tally_study(): the following response value(s):\n',
                stop_msg_string_01,
               '\n',
               'fail(s) to match perfectly with any of the allowed reponse values:\n',
                stop_msg_string_02,
               '\n'
              ) # end of 'stop(...'
       } # end of 'if( length( mismatched_reponse ) > 0 ){...'

        
    response = match.arg( response, choices = allowed_response_types, several.ok = TRUE )
      
    ## summary values to tally (one row per group_name level
    ##  1.  group_name   factor level of the group shown in the row.
    ##  2.  first_day    numeric first day for that group with an observation (numeric)
    ##  3.  last_day     numeric last  day for that group with an observation (numeric)
    ##  4.  common_start logical: Do all the subjects in the group have their first observation on first_day?
    ##  5.  N_in_group   numeric: How many subjects are in the group total?
    ##  6.  PR           numeric: How many partial responses are in the group?
    ##  7.  EOS_CR       numeric: How many end-of-study complete responses are in the group?
    ##  8.  PR + EOS_CR  numeric: How many total responses (PR + EOS_CR) are in the group?
    ##  9.  log_OR       numeric: log odds ratio between the group in that row and the group in "reference"
    ## 10.  SE_log_OR    numeric: standard error for the estimated log odds ratio.
    dat <-
         study_data_frame %>%
         dplyr::select( group_name, subject_ID, x_name, endpoint_name )

    ## find 'first_day' and 'last_day' for each group.
    dat %<>%
      plyr::ddply(
                  .var = eval( group_name ),
                  .fun = function(XX, na_rm = na.rm ){
                         XX %<>%
                         dplyr::mutate( first_day  = min( XX[,x_name], na.rm = na_rm ),
                                        last_day   = max( XX[,x_name], na.rm = na_rm ),
                                        N_in_group = XX %>% droplevels %>% dplyr::select( subject_ID ) %>% unlist %>% nlevels()
                                      )
                         return(XX)
                  }
                  ) # end of 'plyr::ddply(...'


    ## find common_start and common_end
    dat_ID <-
      dat %>%
      plyr::ddply(
                  .var = c( eval( group_name ), eval( subject_ID ) ),
                  .fun = function(XX, na_rm = na.rm, GRP_NAME = eval( group_name ) ){
                         XX %<>%
                         dplyr::mutate( first_day_ID = min( XX[,x_name], na.rm = na_rm ),
                                         last_day_ID = max( XX[,x_name], na.rm = na_rm )
                                      ) %>%
                         maeve::round_numerics()
                    return( XX )
                  }
                  )

        
    study_tally_DF <-
      dat_ID %>%
      plyr::ddply(.var = eval( group_name ),
                  .fun = function( ZZ, na_rm = na.rm, GRP_NAME = eval( group_name ) ){
                                   ZZ %<>%
                                   dplyr::mutate(
                                               common_start = ifelse( diff( range( ZZ$first_day_ID, na.rm = na_rm ) ) < 10^(-12), TRUE, FALSE ),
                                               common_end   = ifelse( diff( range( ZZ$last_day_ID,  na.rm = na_rm ) ) < 10^(-12), TRUE, FALSE )
                                               ) %>%
                                   maeve::round_numerics() %>%
                                   dplyr::select( GRP_NAME, N_in_group, first_day, common_start, last_day, common_end ) %>%
                                   unique()
                  } # end of 'function( ZZ, na_rm = na.rm ){...'
                  ) # 'plyr::ddply( .var = eval( group_name ), ...'


    ## get the group levels and check that the reference group name is among them: 
    group_name_levels <- # get character string with ordered group levels
      study_data_frame %>%
      dplyr::select( !!dplyr::sym( group_name ) ) %>%
      unlist %>%
      levels
     
    if( ! reference %in% group_name_levels ){
       stop_msg_string <- paste( group_name_levels, collapse = ', ' )
       stop( 'error in maeve::tally_study(): designated reference group name ',
              reference,
             ' is not in levels of supplied grouping factor:\n  ',
              stop_msg_string
            ) # end of 'stop(...'
    } # end of 'if( ! reference %in% group_name_levels ){...'
       
    ## define an input data frame with the expected internal names for calculating response rates:
    response_input_data_frame <-
      dat %>%
      dplyr::rename(
                     'group' = !!dplyr::sym( group_name ),  
                        'ID' = !!dplyr::sym( subject_ID ),  
                         'x' = !!dplyr::sym( x_name ),      
                    'y_orig' = !!dplyr::sym( endpoint_name )
                   ) %>%
      dplyr::select( group, ID, x, y_orig )

    ## partial response rate:
    PR_data_frame <- 
      plyr::ddply(
                  .data = response_input_data_frame,
                  .var  = 'group',
                  .fun  = function(XX){ determine_PR( XX, min_val = EOS_CR_minval, PR_threshold = PR_threshold ) }
                 ) %>%
      stats::setNames( c( group_name, 'PR' ) )

    ## end-of-study complete response rate:
    EOS_CR_data_frame <- 
      plyr::ddply(
                  .data = response_input_data_frame,
                  .var  = 'group',
                  .fun  = function(XX){ determine_EOS_CR( XX, min_val = EOS_CR_minval ) }
                 ) %>%
      stats::setNames( c( group_name, 'EOS_CR' ) )


    ## Add partial response (PR) and end-of-study complete response (EOS_CR) counts:
    PR_or_EOS_CR_data_frame <-
      dplyr::left_join( PR_data_frame, EOS_CR_data_frame, by = group_name ) %>%
      dplyr::mutate( PR_or_EOS_CR = PR + EOS_CR ) %>%
      dplyr::select( !!dplyr::sym( group_name ), !!dplyr::sym( 'PR_or_EOS_CR' ) )

        
    ## There are three possible values for 'response' beyond 'none':
    ## (a) response == 'PR'
    ## (a) response == 'EOS_CR'
    ## (a) response == 'PR_or_EOS_CR' (this is just the sum of "PR" and "EOS_CR", a "total response" measure).

    output_data_frame <- study_tally_DF

        
    if( 'PR' %in% response ){
      ## partial response:
      output_data_frame %<>%
        dplyr::left_join( PR_data_frame, by = group_name ) %>%
        estimate_log_odds(
                               group_name_char = group_name,
                                     reference = reference,
                      number_on_first_day_char = 'N_in_group',
                               count_desc_char = 'PR',
                               additive_offset = additive_offset,
                               return_original = FALSE
                                ) %>%
        maeve::round_numerics() %>%
        dplyr::rename( PR_log_OR = log_OR, PR_SE_log_OR = SE_log_OR )  %>%                                       
        dplyr::left_join( PR_data_frame,     y = . , by = group_name ) %>%
        dplyr::left_join( output_data_frame, y = . , by = group_name )
        ##
    } ## end of 'if( 'PR' %in% response ){...'

       
    if( 'EOS_CR' %in% response ){
      ## end-of-study complete response
      output_data_frame %<>%
        dplyr::left_join( EOS_CR_data_frame, by = group_name ) %>%
        estimate_log_odds(
                               group_name_char = group_name,
                                     reference = reference,
                      number_on_first_day_char = 'N_in_group',
                               count_desc_char = 'EOS_CR',
                               additive_offset = additive_offset,
                               return_original = FALSE
                                ) %>%
        maeve::round_numerics()      %>%
        dplyr::rename( EOS_CR_log_OR = log_OR, EOS_CR_SE_log_OR = SE_log_OR ) %>%                                       
        dplyr::left_join( EOS_CR_data_frame, y = . , by = group_name ) %>%
        dplyr::left_join( output_data_frame, y = . , by = group_name )
    } ## end of 'if( 'EOS_CR' %in% response ){...'

       
    if( 'PR_or_EOS_CR' %in% response ){
      ## added together counts from partial response & end-of-study complete response
      output_data_frame %<>%
        dplyr::left_join( PR_or_EOS_CR_data_frame, by = group_name ) %>%
        estimate_log_odds(
                               group_name_char = group_name,
                                     reference = reference,
                      number_on_first_day_char = 'N_in_group',
                               count_desc_char = 'PR_or_EOS_CR',
                               additive_offset = additive_offset,
                               return_original = FALSE
                                ) %>%
        maeve::round_numerics() %>%
        dplyr::rename( PR_or_EOS_CR_log_OR = log_OR, PR_or_EOS_CR_SE_log_OR = SE_log_OR ) %>%                                       
        dplyr::left_join( PR_or_EOS_CR_data_frame, y = . , by = group_name ) %>%
        dplyr::left_join( output_data_frame, y = . , by = group_name )
    } ## end of 'if( 'PR_or_EOS_CR' %in% response ){...'

        
    return( output_data_frame )
       
  } # end of 'tally_study <- function(...'
