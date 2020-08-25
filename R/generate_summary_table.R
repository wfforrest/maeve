#' Create a summary table making Dunnett comparisons.
#'
#' This is included principally for its options to generate legacy statistics, and is
#' not recommended for general use.
#'
#' @author Bill Forrest <forrest@@gene.com>
#'
#' @param model_list R list with model objects from maeve::model_study().
#' @param clean_DF data.frame with fitted data from maeve::model_study(), e.g., "model_list$data$clean_DF_pred".
#' @param reference_Dunnett character string with exact match to an element of grpnames.  The exact match will be the reference group in Dunnett contrasts.
#' @param full_inverse_function R function that inverts the response analyzed back to its original scale.
#' @param study_ID_value character string with five digit DIVOS study ID, OR a DivoStudy object.
#' @param metric character vector specifying what metric(s) to use in longitudinal modeling.
#' @param progress logical determining whether to send interim progress messages.
#' @param xmin numeric lower bound of definite integral
#' @param xmax numeric upper bound of definite integral
#' @param xrange_norm_method character string determining *how* the xrange normalization within leia() should be done.  Must be one of "none", "xrange", "slope_equivalent".
#' @param extended_output logical whether to return a full range of inferred interim table values.
#' @param conf_int character vector designating for which quantities a parametric bootstrap should be run to get confidence intervals.  Allowed options are "none" (the default", "TGI", "Effect_Duration", or c("TGI", "Effect_Duration").
#' @param EOS_CR_minval numeric passed to internal function to decide what constitutes a "complete response" for the the End-Of-Study Complete Response (EOS_CR) value on the original, untransformed response scale.
#' @param PR_threshold numeric passed to internal function to decide what constitutes a "partial response" for the PR value.  Fractional reduction is computed by-ID, from an ID-specific baseline value on the original, untransformed scale.
#' @param CI_lower_quantile numeric value in the range (0,1) denoting the lower quanitle for confidence intervals. 
#' @param CI_upper_quantile numeric value in the range (0,1) denoting the upper quanitle for confidence intervals.
#' @param x_time_spacing spacing of time points across which to evaluate the splines.  If time values are recorded as integers, then the default should be sufficient.
#' @param x_min_count_distinct numeric integer.  If the number of distinct x-values at which splines are evaluated is less than this, a warning is issued.
#' @param K_boot integer number of bootstrap samples to draw.
#'
#' @return An R list with named output.
#'
#' @examples
#'  cat('Currently no working example for generate_summary_table().')
#'
#' @author Bill Forrest \email{forrest@@gene.com}
#'
#' @references \url{www.r-project.org}
#'
#' @keywords data.frame
#' @export
#'
generate_summary_table <-
  function( model_list,
            clean_DF,
            reference_Dunnett = maeve_options('reference_Dunnett'),
            full_inverse_function = switch( maeve_options('inv_func_char'),
                                            Identity = function(x){                                         x  - maeve_options('add_to_endpoint') },
                                                       function(x){ (get( maeve_options('inv_func_char') ))(x) - maeve_options('add_to_endpoint') }
                                            ),
            study_ID_value       = '', 
            metric               = c( 'AUC', 'ITGR', 'linear', 'AUC_pwl', 'ITGR_pwl', 'AUC_poly', 'ITGR_poly' ), # maeve_options('metrics_supported')
            progress             = maeve_options( 'progress' ),
            xmin                 = NULL, 
            xmax                 = NULL,           
            xrange_norm_method   = maeve_options( 'xrange_norm_method' ),
            extended_output      = FALSE,
            conf_int             = c( 'none', 'TGI', 'Effect_Duration' )[1], # just listing other 2 options; the whole vector is hard-coded below.
            EOS_CR_minval        = maeve_options('EOS_CR_minval'),
            PR_threshold         = maeve_options('PR_threshold'), 
            CI_lower_quantile    = .025,
            CI_upper_quantile    = .975,
            x_time_spacing       = 0.25, # spacing for numerical integration grid in legacy metrics.
            x_min_count_distinct = 10,   # minimum number of distinct x-values for numerical integration.  Actual number depends on range of x-values and 'x_time_spacing' value.
            K_boot               = 1000  # sample size for parametric bootstrap
           ){

    metric = match.arg( metric, several.ok = TRUE )

    conf_int_allowed <- c( 'none', 'TGI', 'Effect_Duration' )
    conf_int_unrecognized <- conf_int[ ! conf_int %in% conf_int_allowed ] # which confidence interval options passed were not recognized?

    if( !all( conf_int %in% conf_int_allowed ) ){
    ## This block is to head off the occurrence of passing multiple
    ## arguments, making a typo in one, and then match.arg() letting  
    ## the typo slide because the other argument matches, and just
    ## proceeding quietly with one argument.  Couldn't figure out a
    ## more graceful workaround.
      stop_msg_string_01 <- paste( conf_int_unrecognized, collapse = ', ' )
      stop_msg_string_02 <- paste( conf_int_allowed,      collapse = ', ' )
      stop( 'error in generate_summary_table(): ',
            'the following values passed to "conf_int" are not in the allowed ',
            'vector of options: ',
             stop_msg_string_01,
            '; \n',
            'allowed options are:\n',
             stop_msg_string_02,
            '\n'
           )
    } # if( !all( conf_int %in% conf_int_allowed ) ){...
    conf_int = match.arg( conf_int, choices = conf_int_allowed, several.ok = TRUE ) %>% unique

    if( 'none' %in% conf_int & length( conf_int ) > 1 ){
      stop('error in maeve::generate_summary_table(): if argument "conf_int" includes "none", then no other options are allowed')
    }

    xrange_norm_method = match.arg( xrange_norm_method )
    ## define an internal logical flag. We use only whether xrange_norm_method == 'none' or not.
    norm_to_xrange <- !identical( xrange_norm_method, 'none' ) # if not 'none', then we normalize.
    
    if( CI_lower_quantile < 0 ){
      stop('error in maeve::generate_summary_table(): CI_lower_quantile must be in [0,1]')
    }
      
    if( CI_upper_quantile < 0 ){
      stop('error in maeve::generate_summary_table(): CI_upper_quantile must be in [0,1]')
    }
      
    if( CI_lower_quantile > CI_upper_quantile  ){
      stop('error in maeve::generate_summary_table(): CI_lower_quantile must be less than or equal to CI_upper_quantile')
    }
      
    if(progress){
      cat('Starting summary table generation.\n')
    }
      
    ### Get a one-row-per-group summary table:
    if(progress){
      cat("Making 'Identity' comparisons.\n")
    }


    if( length( metric ) > 1 ){

       ## Implement multi-metric summary table generation by recursion.
       metric_names = as.list( metric ); names( metric_names ) <- metric

       if( extended_output ){
        stop('error in generate_summary_table(): "extended_output" must be FALSE to run multiple metrics at once')
       }
       
       list_with_one_metric_per_entry <-
         metric_names %>%
           plyr::llply( .fun =
                          function( ONE_METRIC_NAME ){
                            ### re-call the function recursively with just one argument:
                            one_metric <- 
                              generate_summary_table( model_list = model_list,
                                                      clean_DF = clean_DF,
                                                      reference_Dunnett = reference_Dunnett,
                                                      full_inverse_function = full_inverse_function,
                                                      study_ID_value = study_ID_value,
                                                      metric = ONE_METRIC_NAME,
                                                      progress = progress,
                                                      xmin = xmin,
                                                      xmax = xmax,
                                                      xrange_norm_method = xrange_norm_method,
                                                      extended_output = extended_output,
                                                      conf_int = conf_int,
                                                      EOS_CR_minval = EOS_CR_minval,
                                                      PR_threshold = PR_threshold,
                                                      CI_lower_quantile = CI_lower_quantile,
                                                      CI_upper_quantile = CI_upper_quantile,
                                                      x_time_spacing = x_time_spacing,
                                                      x_min_count_distinct = x_min_count_distinct,
                                                      K_boot = K_boot
                                                   )
                              return( one_metric )
                          }
                       ) # end of 'plyr::llply( .fun =...'
       summary_table_combined_across_metric <- do.call( 'rbind', list_with_one_metric_per_entry )
       rownames( summary_table_combined_across_metric ) <- NULL
       return( summary_table_combined_across_metric )  
    } # end of 'if( length( metric ) > 1 ){...'


    
    Identity_comparison <-
      maeve::compare_groups( model_list = model_list,
                             study_ID_value = study_ID_value,
                             reference_Dunnett = reference_Dunnett,
                             metric = metric,
                             contrast = 'Identity',
                             xmin = xmin,
                             xmax = xmax,
                             xrange_norm_method = xrange_norm_method,
                             draw_figures = FALSE )

    Identity_table <- Identity_comparison$data$effectDF %>% dplyr::select( metric, contrast, Estimate )

    
  
    ## Figure out the reference group for Dunnett's contrasts
    ##
    if( is.null( reference_Dunnett ) ){
      reference_index <- 1
      reference_Dunnett <- levels( clean_DF[ , 'group' ] )[reference_index]
    } # end of ' if( is.null( reference_Dunnett ) ){...'

    if( ! is.null( reference_Dunnett ) ){
      
      reference_index <-  match( reference_Dunnett, levels( clean_DF[ , 'group' ] ) )

      stopifnot( length( reference_index ) > 0 )
      
      if( is.na( reference_index ) ){
        stop_msg_string <- paste( levels( clean_DF[,'group'] ), collapse = '  ' )
        stop( 'error in maeve::generate_summary_table():\n\n  ',
               reference_Dunnett,
              '\n\n',
              'not found in the group_name factor levels:\n\n  ',
              stop_msg_string,
              '\n\n'
             )
      } # end of 'if( is.na( reference_index ) ){...'
      
    } # end of 'if( ! is.null( reference_Dunnett ) ){...'


    

    ## Compute approximate AUC on original scale.
      
    if( !is.function( full_inverse_function ) ){
      stop('error in maeve::generate_summary_table(): must provide a valid inverse function explicitly')
    }

    pred_name <- switch( metric,
                         ##
                         linear   = 'pred_lin',
                         ##
                         ITGR     = 'pred_gam',
                         AUC      = 'pred_gam',
                         ##
                         ITGR_pwl = 'pred_pwl',
                          AUC_pwl = 'pred_pwl',
                         ##
                        ITGR_poly = 'pred_poly',
                         AUC_poly = 'pred_poly',
                         ##
                         stop( 'error in maeve::generate_summary_table():', metric, 'is not a recognized option for "metric".' )
                          ) # end of 'switch( metric,...'

    local_DF <-
    clean_DF %>%
    dplyr::select( !!dplyr::sym( 'group' ), !!dplyr::sym( 'x' ), !!dplyr::sym( pred_name ) ) %>%
    dplyr::arrange( group, x ) %>%
    (base::unique) %>%         
    dplyr::rename(  'pred_model' = !!dplyr::sym( pred_name ) ) %>%
    dplyr::mutate( pred_originalScale = full_inverse_function( pred_model ) ) %>%
    dplyr::filter( !is.na( pred_model ) )
      
    ## (1) If "xmin" is NULL, then we just find it from the data separately by group.
    ##     If xmin is *non*-NULL, check that xmin is numeric and greater than or
    ##     equal to the lowest x-value in each group.
    ##     Otherwise, the numerical integration down to xmin will try to evaluate
    ##     NA values for the predicted curve (for at least one group) and cause
    ##     a crash -- the curves do not extrapolate.
    reference_x_range <- local_DF %>%
      dplyr::filter(group == reference_Dunnett) %>%
      dplyr::pull(x) %>%
      range()
     
    if (is.null(xmin)){
      xmin <- min(reference_x_range)
    } else {
      if( !is.numeric( xmin ) ){
        stop('error in maeve::generate_summary_table(): "xmin" must be numeric.\n')
      }
    }

    ## (2) If "xmax" is NULL, then we just find it from the data separately by group.
    ##     If xmax is *non*-NULL, check that xmax is numeric and less than or
    ##     equal to the greatest x-value in the group. 
    ##     Otherwise, the numerical integration up to xmax will try to evaluate
    ##     NA values for the predicted curve (for at least one group) and cause
    ##     a crash -- the curves do not extrapolate.

    if (is.null(xmax)){
      xmax <- max(reference_x_range)
    } else {
      if( !is.numeric( xmax ) ){
        stop('error in maeve::generate_summary_table(): "xmax" must be numeric.\n')
      }
    }
      
    ##
    ## Some groups may not be in [xmin, xmax] range defined for AUC calculation, so
    ## they shouldn't be included into such operations. However, they shouldn't be
    ## excluded from other metrics (like 2x, 5x grow etc)
    ##
    valid_auc_groups <- local_DF %>%
      dplyr::group_by(group) %>%
      dplyr::summarise( 
        x_value_min = min(x, na.rm = TRUE),
        x_value_max = max(x, na.rm = TRUE),
        is_in_range = xmin >= x_value_min & xmax <= x_value_max
      ) %>%
      dplyr::filter(is_in_range) %>%
      dplyr::pull(group)
      
     
    local_DF %>%
      plyr::ddply(
        .var = 'group',
        .fun = function(my_dat){ 
          my_dat <- droplevels( my_dat )
          group <- my_dat$group[1]
          is_valid_auc_group <- group %in% valid_auc_groups
           
          integral_lower_limit <- 'if'(is_valid_auc_group, xmin, NA)
          integral_upper_limit <- 'if'(is_valid_auc_group, xmax, NA)
           
          ## Add a numeric to represent the xrange width by which to normalize (i.e., divide).
          ## If norm_to_xrange == FALSE, set this to 1.  This will provide a value not only for
          ## scaling the AUC_originalScale when norm_to_xrange == TRUE, but also a value for
          ## scaling appropriately the group-specific baseline-time tumor burden when computing
          ## TGI_baseline down below.
          ##
          ## "xrange_norm_width" will be "max(x)-min(x)" when norm_to_xrange == TRUE, or "1" when norm_to_xrange == FALSE
          xrange_norm_width <- integral_upper_limit - integral_lower_limit
            
          if (is_valid_auc_group){
            group_func <- stats::approxfun( my_dat$x, my_dat$pred_originalScale )
            integral_definite <- stats::integrate(
              f     = group_func, 
              lower = integral_lower_limit, 
              upper = integral_upper_limit, 
              stop.on.error = FALSE
            ) # end of 'stats::integrate(...'
            AUC_originalScale <- integral_definite$value / ifelse( norm_to_xrange, xrange_norm_width, 1 )
            firstDay_mean_originalScale <- mean( my_dat$pred_originalScale[ my_dat$x == max(my_dat$x[ my_dat$x <= integral_lower_limit ], na.rm = TRUE) ] )
          } else {
            AUC_originalScale <- NA
            firstDay_mean_originalScale <- NA
          }
           
          data.frame( 
            group = group,
             firstDay_mean_originalScale = firstDay_mean_originalScale,
             xrange_norm_width = xrange_norm_width,
             AUC_originalScale = AUC_originalScale
           )
         }
       ) %>% # end of 'plyr::ddply(...'
    dplyr::mutate(
                 AUC_originalScale_reference = AUC_originalScale[ group == reference_Dunnett ],
       firstDay_mean_originalScale_reference = firstDay_mean_originalScale[ group == reference_Dunnett ]
                  ) %>%
     dplyr::mutate(
     ## 20171220:  Two notes on computing TV_change:
     ##                   
     ## (1) Each group's TV_change is normalized to its group-specific baseline value, as estimated in the model.
     ##                   
     ## (2) If "AUC_originalScale" was normalized to the xrange width, then the "rectangle" of
     ## response region needs to be normalized to that width as well.  Since it is a rectangle
     ## of height "firstDay_mean_originalScale" and width "xrange_norm_width", normalizing it will just
     ## leave the value "firstDay_mean_originalScale".  However, if "AUC_originalScale" was *not* normalized
     ## to the xrange width, then the rectangle of response region should not be normalized either, and should
     ## be an area of "firstDay_mean_originalScale * xrange_norm_width".
                    TV_change           = AUC_originalScale - firstDay_mean_originalScale * ifelse( norm_to_xrange, 1, xrange_norm_width ),
                    TV_change_reference = TV_change[ group == reference_Dunnett ]
                   ) %>%
     dplyr::select( group,
                    xrange_norm_width,
                    ##
                    firstDay_mean_originalScale,
                    firstDay_mean_originalScale_reference,
                    ##
                    AUC_originalScale,
                    AUC_originalScale_reference,
                    ##
                    TV_change,
                    TV_change_reference
                    ##
                   ) %>%
     dplyr::mutate(
     ## Two definitions of tumor growth inhibition:
     ## (1) Find original scale AUC for each group as a percentage of reference, written on "1 minus" scale to show "inhibition":
                   TGI_approx          =  ( 1 - AUC_originalScale / AUC_originalScale_reference ) * 100 ,
     ## (2) Find original scale change of AUC from its first-day baseline defined as
     ##      ( AUC_trt  / (range of days) ) - First_trt_day_AvgValue  / (range of days) ... This is in "TV_change", defined above.
     ##                   divided by                   
     ##      ( AUC_ctrl / (range of days) ) - First_ctrl_day_AvgValue / (range of days) ... This is in "TV_change_reference", defined above.
                   TGI_approx_baseline =  ( 1 - TV_change  / TV_change_reference ) * 100
                   ) -> TGI_DF


      
     ## Expand levels of the factor TGI_DF$group to include all the levels in Identity_table$contrast (including any that were truncated away)
     ## and merge with Identity_table$contrast to include missing rows:
     TGI_DF %<>%
     dplyr::mutate( group = factor( group, levels = levels( Identity_table$contrast ) ) ) %>%
     dplyr::right_join(   
                         y = Identity_table %>% dplyr::select( contrast ),
                        by = c( 'group' = 'contrast' )
                       )

      
    
    ### Get a summary table with one row for each non-reference group (i.e,. Dunnett contrast):
    if(progress){
        cat("Making 'Dunnett' comparisons.\n")
    }

    Dunnett_comparison <-
      maeve::compare_groups( model_list = model_list,
                             study_ID_value = study_ID_value,
                             reference_Dunnett = reference_Dunnett,
                             metric = metric,
                             contrast = 'Dunnett',
                             xmin = xmin,
                             xmax = xmax,
                             xrange_norm_method = xrange_norm_method,
                             extended_output = TRUE,
                             draw_figures = FALSE
                            )

    Dunnett_table <- Dunnett_comparison$data$effectDF
    
    if(progress){
        cat("Merging Identity & Dunnett results.\n")
    }
    
    Identity_contrast_char <- as.character( Identity_table$contrast )
     Dunnett_contrast_char <- as.character(  Dunnett_table$contrast )
    
    alignment_index <- rep( NA, nrow( Identity_table ) )
    refIdx <- match(reference_Dunnett, Identity_contrast_char)
    
    for( ii in setdiff( 1:length(alignment_index), refIdx ) ){

      match_position <- match( paste(Identity_contrast_char[ii], reference_Dunnett, sep = ' - ' ), Dunnett_contrast_char )
      
      if( length( match_position ) != 1 | !is.numeric( match_position) | is.na( match_position ) ){
        stop('error in maeve::generate_summary_table(): group names appear to be non-unique.')
      }
      alignment_index[ii] <- match_position
    } # end of 'for( ii in 1:length(alignment_index) ){...'

    output_table <- data.frame( Identity_table[,c('metric', 'contrast')], Estimate = 0, sigma = 0, lwr = 0, upr = 0, tstat = NA, pvalues = NA )
    
    columns_to_fill <- c('Estimate','sigma','lwr','upr','tstat','pvalues')
    for( ii in 1:length(alignment_index) ){
      if( !is.na(alignment_index[ii]) ){
        group <- output_table$contrast[[ii]]
        
        table_fill <- if (group %in% valid_auc_groups){
          Dunnett_table[ alignment_index[ii], columns_to_fill ]
        } else {
          NA
        }
        
        output_table[ii, columns_to_fill] <- table_fill
      }
    }

    output_table <-
     output_table %>%
     dplyr::rename( Difference_from_reference = Estimate ) %>%        
     dplyr::left_join( Identity_table, ., by = c('metric', 'contrast') ) %>%
     dplyr::rename( group = contrast ) %>%
     dplyr::mutate( group = factor( group, levels = Identity_contrast_char ) )        

    ## Next, we need to retrieve group-specific information from
    ## the analysis (e.g., # animals per group) and merge it.
    
    if(progress){
        cat("Identifying 'End-Of-Study Complete Responses' and 'Times To Progression'.\n")
    }

    clean_DF %>%
    plyr::ddply(
          .var = 'group',
          .fun = function(XX){
                    XX %<>% droplevels()
            
                     ## Define an indicator for rows in which all four model predictions are missing:
                     all_pred_missing <- ( is.na( XX$pred_lin ) &
                                           is.na( XX$pred_gam ) &
                                           is.na( XX$pred_pwl ) &
                                           is.na( XX$pred_poly )
                                          )
                     
                     effect_Duration <- determine_Effect_Duration( XX )
                       
                     data.frame(                group = factor( levels( XX$group ) ),
                                          N_first_day = length( unique( XX$ID ) ),
                                            first_day = min( XX$x, na.rm = TRUE ),
                                integration_first_day = xmin,
                                integration_last_day  = xmax,
                                fit_last_day = ifelse( all( all_pred_missing ), NA, max( XX$x[ ! all_pred_missing ] , na.rm = TRUE ) ), # last day used in fitted curves.
                                    last_day = max( XX$x, na.rm = TRUE ), # last day for raw data values.                                                               
                                           PR    = determine_PR(     XX, min_val = EOS_CR_minval, PR_threshold = PR_threshold ),
                                          EOS_CR = determine_EOS_CR( XX, min_val = EOS_CR_minval ),
                                 effect_start    = effect_Duration$effect_start,   
                                 effect_end      = effect_Duration$effect_end,     
                                 effect_duration = effect_Duration$effect_duration,
                                TTP_2X = interpolate_time_to_threshold( XX, metric, full_inverse_function = full_inverse_function, threshold = 2 ),
                                TTP_5X = interpolate_time_to_threshold( XX, metric, full_inverse_function = full_inverse_function, threshold = 5 )
                                ) 
                             } ## end of 'function(XX){...'
               ) %>%  ### end of "plyr::ddply( .var = 'group', ..."
    dplyr::mutate(group = factor(group, levels = levels(output_table$group))) %>%
    dplyr::left_join( output_table, by = 'group' ) %>%
    dplyr::left_join( TGI_DF,       by = 'group' ) %>%
    dplyr::rename(     Diff_Ref =  Difference_from_reference,
                  firstDay_orig =  firstDay_mean_originalScale,
                       AUC_orig =  AUC_originalScale,
                            TGI =  TGI_approx,
                   TGI_baseline =  TGI_approx_baseline
                      ) -> output_table 

    if( ! metric %in% c( 'AUC', 'ITGR' ) ){
      ## If the metric requested is not for the GAM spline, do not provide effect duration summaries.
      output_table$effect_start    <-
      output_table$effect_end      <-
      output_table$effect_duration <- NA
    }

      

      
 ## Confidence intervals via parametric bootstrap.
 ##
 ## The AUC, TGI, and TGI_baseline statistics, in
 ## addition to the effect_start, effect_end, effect_duration
 ## statistics do not have tractable asymptotic distributions,
 ## so uncertainty is assessed by parametric bootstrap, which
 ## can be slow (several minutes in testing) for large studies.
 ##    
 if( 'TGI' %in% conf_int ){     
    
    if(progress){
        cat("Estimating confidence intervals for TGI.\n")
    }


    
    TGI_DF_boot <-
    bootstrap_parallel( model_list            = model_list,
                        clean_DF              = clean_DF %>% dplyr::filter( model_x_value, group %in% valid_auc_groups ) %>% droplevels,
                        reference_Dunnett     = reference_Dunnett,
                        metric                = metric,
                        bootstrap_statistic   = 'AUC_originalScale',
                        bootstrap_behavior    = 'random',
                        K_boot                = K_boot,
                        x_time_spacing        = x_time_spacing,
                        x_min_count_distinct  = x_min_count_distinct,
                        norm_to_xrange        = norm_to_xrange,
                        full_inverse_function = full_inverse_function,
                        xmin = xmin,
                        xmax = xmax
                       ) %>%
    dplyr::mutate( group = factor( group, levels = levels( clean_DF$group ) ) )

    extra_levels_DF <- expand.grid( group = levels( TGI_DF_boot$group ), # contains any levels that were dropped from the original modeling.
                                    replicate = levels( TGI_DF_boot$replicate )
                                   )

    TGI_DF_boot %<>% dplyr::left_join( x = extra_levels_DF, y = . , by = c('group','replicate') )
    
    CI_TGI <-
    TGI_DF_boot %>%
    plyr::ddply( .var = 'group',
                 .fun = function(x){
                   data.frame( group = factor(unique(as.character(x$group)), levels = levels( output_table$group ) )  ,
                        CI_lower_TGI = setNames( quantile( x$TGI_approx,          CI_lower_quantile, na.rm = TRUE ), NULL ),
                        CI_upper_TGI = setNames( quantile( x$TGI_approx,          CI_upper_quantile, na.rm = TRUE ), NULL ),
                          StdDev_TGI = setNames(       sd( x$TGI_approx                                           ), NULL ),
               CI_lower_TGI_baseline = setNames( quantile( x$TGI_approx_baseline, CI_lower_quantile, na.rm = TRUE ), NULL ),
               CI_upper_TGI_baseline = setNames( quantile( x$TGI_approx_baseline, CI_upper_quantile, na.rm = TRUE ), NULL ),
                 StdDev_TGI_baseline = setNames(       sd( x$TGI_approx_baseline                                  ), NULL )
                             )
                        } # end of '.fun = function(x){...'
                )
    } else{ # end of 'if( 'TGI' %in% conf_int ){ ...'
      TGI_DF_boot <- CI_TGI <- NULL
    }

    ## 20180913: New code to use a parametric bootstrap to put confidence intervals on
    ## effect_start, effect_end, and effect_duration.

    if( 'Effect_Duration' %in% conf_int ){     
    
    if( metric %in% c('AUC', 'ITGR') ){
            
      if(progress){
          cat("Estimating confidence intervals for Effect Duration statistics.\n")
      }

    Effect_Duration_DF_boot <-
    bootstrap_parallel( model_list            = model_list,
                        clean_DF              = clean_DF %>% dplyr::filter( model_x_value ) %>% droplevels,
                        reference_Dunnett     = reference_Dunnett,
                        metric                = metric,
                        bootstrap_statistic   = 'Effect_Duration',
                        bootstrap_behavior    = 'random',
                        K_boot                = K_boot,
                        x_time_spacing        = x_time_spacing,
                        x_min_count_distinct  = x_min_count_distinct,
                        norm_to_xrange        = norm_to_xrange,
                        full_inverse_function = full_inverse_function
                        ) %>%
    dplyr::mutate( group = factor( group, levels = levels( clean_DF$group ) ) )

    extra_levels_DF <- expand.grid( group = levels( Effect_Duration_DF_boot$group ), # contains any levels that were dropped from the original modeling.
                                    boot_replicate = levels( Effect_Duration_DF_boot$boot_replicate )
                                   )

    Effect_Duration_DF_boot %<>% dplyr::left_join( x = extra_levels_DF, y = . , by = c('group','boot_replicate') )

    CI_Effect_Duration <-
    Effect_Duration_DF_boot %>%
    plyr::ddply( .var = 'group',
                 .fun = function(x){
                   ##
                   data.frame(
                               group = factor( unique( as.character( x$group ) ), levels = levels( output_table$group ) ),
                   ##
                  CI_lower_Eff_Start = setNames( quantile( x$effect_start,    CI_lower_quantile, na.rm = TRUE  ), NULL ),
                  CI_upper_Eff_Start = setNames( quantile( x$effect_start,    CI_upper_quantile, na.rm = TRUE  ), NULL ),
                    StdDev_Eff_Start = setNames(       sd( x$effect_start,                      ), NULL ),
                   ##
                    CI_lower_Eff_End = setNames( quantile( x$effect_end,      CI_lower_quantile, na.rm = TRUE  ), NULL ),
                    CI_upper_Eff_End = setNames( quantile( x$effect_end,      CI_upper_quantile, na.rm = TRUE  ), NULL ),
                      StdDev_Eff_End = setNames(       sd( x$effect_end,                        ), NULL ),
                   ##
                    CI_lower_Eff_Dur = setNames( quantile( x$effect_duration, CI_lower_quantile, na.rm = TRUE  ), NULL ),
                    CI_upper_Eff_Dur = setNames( quantile( x$effect_duration, CI_upper_quantile, na.rm = TRUE  ), NULL ),
                      StdDev_Eff_Dur = setNames(       sd( x$effect_duration,                   ), NULL )
                   ##
                              ) # end of 'data.frame(...'
                 } # end of '.fun = function(x){...'
                ) # end of "plyr::ddply( .var = 'group',..."

    } else{ # end of ' if( ! metric %in% c('AUC', 'ITGR') ){...'

    ## for non-spline metrics, return NA values:
    CI_Effect_Duration <-
        data.frame(
                  group = factor( unique( as.character( output_table$group ) ) ),
                 ##
                CI_lower_Eff_Start = NA,
                CI_upper_Eff_Start = NA,
                  StdDev_Eff_Start = NA,
                 ##
                  CI_lower_Eff_End = NA,
                  CI_upper_Eff_End = NA,
                    StdDev_Eff_End = NA,
                 ##
                  CI_lower_Eff_Dur = NA,
                  CI_upper_Eff_Dur = NA, 
                    StdDev_Eff_Dur = NA
                 ##
                  ) # end of 'data.frame(...'

    } # end of "else{ #...' alternative to 'if( ! metric %in% c('AUC', 'ITGR') ){..."  

    } else{ # end of 'if( 'Effect_Duration' %in% conf_int ){ ...'
        Effect_Duration_DF_boot <- CI_Effect_Duration <- NULL
    }  


    ## Join results of parametric bootstrap into output table as appropriate.
    ## Four mutually exclusive cases:
      
    if( 'none' %in% conf_int ){
        NULL # no action...
    }

      
    if(   ( 'TGI'             %in% conf_int ) &
        ! ( 'Effect_Duration' %in% conf_int )
       ){ # join results of TGI parametric bootstrap
     ##
     output_table %<>%
     dplyr::left_join( CI_TGI,             by = 'group' ) %>%
     dplyr::select( group:pvalues,
                    firstDay_orig, AUC_orig,
                    TGI,             CI_lower_TGI,          CI_upper_TGI,          StdDev_TGI,
                    TGI_baseline,    CI_lower_TGI_baseline, CI_upper_TGI_baseline, StdDev_TGI_baseline
                   )
    } # end of "if(   ( 'TGI'             %in% conf_int ) &..."


      
      
    if( ! ( 'TGI'             %in% conf_int ) &
          ( 'Effect_Duration' %in% conf_int )
       ){ # join results of Effect_Duration parametric bootstrap
      ##
      output_table %<>%
      dplyr::left_join( CI_Effect_Duration, by = 'group' ) %>%
      dplyr::select( group:EOS_CR, TTP_2X:pvalues,
                     firstDay_orig, AUC_orig, TGI, TGI_baseline,
                     ### 
                     effect_start,    CI_lower_Eff_Start,    CI_upper_Eff_Start,    StdDev_Eff_Start,
                     effect_end,      CI_lower_Eff_End,      CI_upper_Eff_End,      StdDev_Eff_End,
                     effect_duration, CI_lower_Eff_Dur,      CI_upper_Eff_Dur,      StdDev_Eff_Dur
                    )

    } # end of 'if( ! ( 'TGI'             %in% conf_int ) & # 'Effect_Duration' == conf_int...'

      

      
    if( 'TGI'             %in% conf_int &
        'Effect_Duration' %in% conf_int ){

      output_table %<>%
      dplyr::left_join( CI_TGI,             by = 'group' ) %>%
      dplyr::left_join( CI_Effect_Duration, by = 'group' ) %>%
      dplyr::select( group:EOS_CR, TTP_2X:pvalues,
                     firstDay_orig, AUC_orig,
                     TGI,             CI_lower_TGI,          CI_upper_TGI,          StdDev_TGI,
                     TGI_baseline,    CI_lower_TGI_baseline, CI_upper_TGI_baseline, StdDev_TGI_baseline,
                     effect_start,    CI_lower_Eff_Start,    CI_upper_Eff_Start,    StdDev_Eff_Start,
                     effect_end,      CI_lower_Eff_End,      CI_upper_Eff_End,      StdDev_Eff_End,
                     effect_duration, CI_lower_Eff_Dur,      CI_upper_Eff_Dur,      StdDev_Eff_Dur
                    )
    } # end of 'if( 'TGI'             %in% conf_int &...'
      
      
    if( ! extended_output ){
      return( output_table )
    } else{
      return(
      list( output_table = output_table,
            TGI_DF_boot = TGI_DF_boot, CI_TGI = CI_TGI,
            Effect_Duration_DF_boot = Effect_Duration_DF_boot, CI_Effect_Duration = CI_Effect_Duration
           )
            )
    }

    
  } # end of "generate_summary_table()".
