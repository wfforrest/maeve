#' Create a list with TGI and TGI_baseline summary statistics
#'
#' @author Bill Forrest <forrest@@gene.com>
#'
#' @param model_list R list with model objects from maeve::model_study( ..., return_all = FALSE).
#' @param clean_DF data.frame with fitted data from maeve::predict_study().
#' @param full_inverse_function R function that inverts the response analyzed back to its original scale.
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
#'  cat('Noo working example for unexported function generate_TGI_summary_statistics().')
#'
#' @author Bill Forrest \email{forrest@@gene.com}
#'
#' @references \url{www.r-project.org}
#'
#' @keywords data.frame
#'
generate_TGI_summary_statistics <-
  function( model_list,
            clean_DF,
            full_inverse_function = switch( maeve_options('inv_func_char'),
                                            Identity = function(x){                                         x  - maeve_options('add_to_endpoint') },
                                                       function(x){ (get( maeve_options('inv_func_char') ))(x) - maeve_options('add_to_endpoint') }
                                            ),
            study_ID_value = levels( model_list$data$clean_DF_pred$study_id ),
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

    if( length( metric ) != 1 ){
        stop('error in maeve:::generate_TGI_summary_statistics(): "metric" must be exactly one of c("linear","AUC","ITGR").')
    }

    if( CI_lower_quantile < 0 ){
        stop('error in maeve:::generate_TGI_summary_statistics(): CI_lower_quantile must be in [0,1].')
    }

    if( CI_upper_quantile < 0 ){
        stop('error in maeve:::generate_TGI_summary_statistics(): CI_upper_quantile must be in [0,1].')
    }

    if( CI_lower_quantile > CI_upper_quantile  ){
        stop('error in maeve:::generate_TGI_summary_statistics(): CI_lower_quantile must be less than or equal to CI_upper_quantile.')
    }
    
    if(progress){
        cat('Starting summary table generation.\n')
    }
    
    ### Get a one-row-per-group summary table:
    if(progress){
        cat("Making 'Identity' comparisons.\n")
    }


    fixed_TGI_table <- 
    bootstrap_parallel( model_list,
                        clean_DF = clean_DF,
                        full_inverse_function = full_inverse_function,
                        metric = metric,
                        progress = progress,
                        bootstrap_statistic = 'AUC_originalScale',
                        ##
                        bootstrap_behavior   = 'fixed',
                        K_boot               = 1,
                        x_time_spacing       = x_time_spacing,
                        x_min_count_distinct = x_min_count_distinct,
                        norm_to_xrange       = norm_to_xrange, 
                        xmin                 = xmin, 
                        xmax                 = xmax
                        ) %>%
    dplyr::rename( TGI = TGI_approx, TGI_baseline = TGI_approx_baseline )                                                  


    random_TGI_DF_boot <-
    bootstrap_parallel( model_list,
                        clean_DF = clean_DF,
                        full_inverse_function = full_inverse_function,
                        metric = metric,
                        progress = progress,
                        bootstrap_statistic = 'AUC_originalScale',
                        ##
                        bootstrap_behavior   = 'random',
                        K_boot               = K_boot,
                        x_time_spacing       = x_time_spacing,
                        x_min_count_distinct = x_min_count_distinct,
                        norm_to_xrange       = norm_to_xrange, 
                        xmin                 = xmin, 
                        xmax                 = xmax
                        )

    CI_TGI <-
    random_TGI_DF_boot %>%
    plyr::ddply( .var = 'group',
                 .fun = function(x){ data.frame( group = factor(unique(as.character(x$group))),
                  CI_lower_TGI =          setNames( quantile( x$TGI_approx,          CI_lower_quantile ), NULL ),
                  CI_upper_TGI =          setNames( quantile( x$TGI_approx,          CI_upper_quantile ), NULL ),
                    StdDev_TGI =          setNames(       sd( x$TGI_approx,                            ), NULL ),
                  CI_lower_TGI_baseline = setNames( quantile( x$TGI_approx_baseline, CI_lower_quantile ), NULL ),
                  CI_upper_TGI_baseline = setNames( quantile( x$TGI_approx_baseline, CI_upper_quantile ), NULL ),
                    StdDev_TGI_baseline = setNames(       sd( x$TGI_approx_baseline,                   ), NULL )
                ) }
              ) # end of "plyr::ddply( .var = 'group',..."

    output_table <-
      fixed_TGI_table %>%
      dplyr::select( -replicate ) %>%
      dplyr::left_join( CI_TGI, by = 'group' ) %>%
      dplyr::select( group:TGI, CI_lower_TGI:StdDev_TGI, TGI_baseline, CI_lower_TGI_baseline:StdDev_TGI_baseline )
    
    if( ! extended_output ){
      return( output_table )
    } else{
      return( list( output_table = output_table, random_TGI_DF_boot = random_TGI_DF_boot ) )
    }
    
  } # end of "generate_TGI_summary_statistics()".
