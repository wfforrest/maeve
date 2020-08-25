#' Create a list with Effect_Duration summary statistics
#'
#' @author Bill Forrest <forrest@@gene.com>
#'
#' @param model_list R list with model objects from maeve::model_study().
#' @param clean_DF data.frame with fitted data, usually from model_list$data$clean_DF_pred.
#' @param study_ID_value character string with five digit DIVOS study ID, OR a DivoStudy object.
#' @param metric character vector specifying what metric(s) to use in longitudinal modeling.
#' @param progress logical determining whether to send interim progress messages.
#' @param xmin numeric lower bound of definite integral
#' @param xmax numeric upper bound of definite integral
#' @param norm_to_xrange logical whether or not to normalize estimated effects to interval length.
#' @param extended_output logical whether to return a full range of inferred interim table values.
#' @param CI_lower_quantile numeric value in the range (0,1) denoting the lower quanitle for confidence intervals. 
#' @param CI_upper_quantile numeric value in the range (0,1) denoting the upper quanitle for confidence intervals.
#' @param x_time_spacing spacing of time points across which to evaluate the splines.  If time values are recorded as integers, then the default should be sufficient.
#' @param x_min_count_distinct numeric integer.  If the number of distinct x-values at which splines are evaluated is less than this, a warning is issued.
#' @param K_boot integer number of bootstrap samples to draw.
#'
#' @return An R list with named output.
#'
#' @examples
#'  cat('No example for unexported function generate_Effect_Duration_summary_statistics().')
#'
#' @author Bill Forrest \email{forrest@@gene.com}
#'
#' @references \url{www.r-project.org}
#'
#' @keywords data.frame
#'
generate_Effect_Duration_summary_statistics <-
  function( model_list,
            clean_DF,
            study_ID_value = '', 
            metric = c( 'AUC', 'ITGR', 'linear' ),
            progress        = FALSE,
            xmin            = NULL, 
            xmax            = NULL,           
            norm_to_xrange  = TRUE,
            extended_output = FALSE,
            CI_lower_quantile    = .025,
            CI_upper_quantile    = .975,
            x_time_spacing       = .250,
            x_min_count_distinct = 10,
            K_boot = 1000
           ){

    metric = match.arg( metric )

    if( length( metric ) != 1 )
      stop('error in maeve::generate_Effect_Duration_summary_statistics(): "metric" must be exactly one of c("linear","AUC","ITGR").')

    if( CI_lower_quantile < 0 )
      stop('error in maeve::generate_Effect_Duration_summary_statistics(): CI_lower_quantile must be in [0,1].')

    if( CI_upper_quantile < 0 )
      stop('error in maeve::generate_Effect_Duration_summary_statistics(): CI_upper_quantile must be in [0,1].')

    if( CI_lower_quantile > CI_upper_quantile  )    
      stop('error in maeve::generate_Effect_Duration_summary_statistics(): CI_lower_quantile must be less than or equal to CI_upper_quantile.')
    
    if(progress)
      cat('Starting summary table generation.\n')
    
    ## Get a one-row-per-group summary table:
    if(progress)
      cat("Making 'Identity' comparisons.\n")


    fixed_Effect_Duration_table <- 
    bootstrap_parallel( model_list,
                        clean_DF = clean_DF,
                        bootstrap_statistic = "Effect_Duration",
                        full_inverse_function = function(x){x}, # not used when bootstrap_statistic = "Effect_Duration"
                        bootstrap_behavior = 'fixed',
                        K_boot               = 1,
                        x_time_spacing       = x_time_spacing,
                        x_min_count_distinct = x_min_count_distinct,
                        norm_to_xrange       = norm_to_xrange, 
                        xmin                 = xmin, 
                        xmax                 = xmax
                        ) %>%
    dplyr::rename( effect_start = effect_start, effect_end = effect_end, effect_duration = effect_duration )


    random_Effect_Duration_DF_boot <-
    bootstrap_parallel( model_list,
                        clean_DF = clean_DF,
                        bootstrap_statistic = "Effect_Duration",
                        full_inverse_function = function(x){x}, # not used when bootstrap_statistic = "Effect_Duration"
                        bootstrap_behavior = 'random',
                        K_boot               = K_boot,
                        x_time_spacing       = x_time_spacing,
                        x_min_count_distinct = x_min_count_distinct,
                        norm_to_xrange       = norm_to_xrange, 
                        xmin                 = xmin, 
                        xmax                 = xmax
                        )

    CI_Effect_Duration <-
    random_Effect_Duration_DF_boot %>%
    plyr::ddply( .var = 'group',
                 .fun = function(x){ data.frame( group = factor(unique(as.character(x$group)) ),
                  ##
                  CI_lower_effect_start    = setNames( quantile( x$effect_start,    CI_lower_quantile ), NULL ),
                  CI_upper_effect_start    = setNames( quantile( x$effect_start,    CI_upper_quantile ), NULL ),
                    StdDev_effect_start    = setNames(       sd( x$effect_start,                      ), NULL ),
                  ##
                  CI_lower_effect_end      = setNames( quantile( x$effect_end,      CI_lower_quantile ), NULL ),
                  CI_upper_effect_end      = setNames( quantile( x$effect_end,      CI_upper_quantile ), NULL ),
                    StdDev_effect_end      = setNames(       sd( x$effect_end,                        ), NULL ),
                  ##
                  CI_lower_effect_duration = setNames( quantile( x$effect_duration, CI_lower_quantile ), NULL ),
                  CI_upper_effect_duration = setNames( quantile( x$effect_duration, CI_upper_quantile ), NULL ),
                    StdDev_effect_duration = setNames(       sd( x$effect_duration,                   ), NULL )
                 ) }
               )

    output_table <-
      fixed_Effect_Duration_table %>%
      dplyr::select( -boot_replicate ) %>%
      dplyr::left_join( CI_Effect_Duration, by = 'group' ) %>%
      dplyr::select( group,
                     CI_lower_effect_start,    effect_start,    CI_upper_effect_start,    StdDev_effect_start,
                     CI_lower_effect_end,      effect_end,      CI_upper_effect_end,      StdDev_effect_end,
                     CI_lower_effect_duration, effect_duration, CI_upper_effect_duration, StdDev_effect_duration
                    )
    
    if( ! extended_output ){
      return( output_table )
    } else{
      return( list( output_table = output_table, random_Effect_Duration_DF_boot = random_Effect_Duration_DF_boot ) )
    }
    
  } # end of "generate_Effect_Duration_summary_statistics()".
