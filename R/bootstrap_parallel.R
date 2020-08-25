#' Employ *parallel* parametric bootstrap to estimate rapidly the variation in summary efficacy metrics.
#'
#' @author Bill Forrest <forrest@@gene.com>
#'
#' @param model_list list from maeve::model_study().
#' @param clean_DF data.frame.  It should be "model_list$data$clean_DF_pred".
#' @param reference_Dunnett character string with exact match to an element of grpnames.  The exact match will be the reference group in Dunnett contrasts.
#' @param full_inverse_function R function that inverts the response analyzed back to its original scale.
#' @param metric character string describing which response curve summary metric to use.
#' @param progress logical whether to print progress messages.
#' @param bootstrap_statistic character string:  Statistic for which the parametric bootstrap will assess uncertainty.
#' @param bootstrap_behavior character string:  Use fixed or random coefficient draws for parametric bootstrap?  The "fixed" option is intended for testing.
#' @param bootstrap_covariance_scale numeric scalar by which to scale multivariate Gaussian covariance matrix.
#' @param design_matrix matrix of numerics with properly ordered design matrix from "predict( md, type = 'lpmatrix' )", where "md" is model extracted from model_list.  This is found automatically if left as NULL (the default), but can be supplied by the user to speed up repeated execution, since it is typically unchanging within any given bootstrap simulation.
#' @param integral_grid_n integer number of grid points for numerical integration.
#' @param xmin numeric lower bound of range over which to evaluate the spline.
#' @param xmax numeric upper bound of range over which to evaluate the spline.
#' @param x_time_spacing spacing of time points across which to evaluate the splines.  If time values are recorded as integers, then the default should be sufficient.
#' @param x_min_count_distinct numeric integer.  If the number of distinct x-values at which splines are evaluated is less than this, a warning is issued.
#' @param norm_to_xrange logical for AUC calcuations: whether to normalize numeric definite integral to x-range.
#' @param K_boot integer number of bootstrap samples to draw.
#'
#' @return An R data.frame with a number of summary metrics (one value per treatment group).
#'
#' @examples
#'  cat('Currently no working example for bootstrap_parallel().')
#'
#' @author Bill Forrest \email{forrest@@gene.com}
#'
#' @references \url{www.r-project.org}
#'
#' @keywords list
#'
bootstrap_parallel <-
    function( model_list,
              clean_DF,  
              reference_Dunnett = NULL,
              full_inverse_function = NULL,
              metric = c( 'AUC', 'ITGR', 'linear', 'AUC_pwl', 'ITGR_pwl', 'AUC_poly', 'ITGR_poly' ),
              progress = FALSE,
              bootstrap_statistic = c( 'AUC_originalScale', 'Effect_Duration' ),
              bootstrap_behavior   = c( 'fixed', 'random' ),
              bootstrap_covariance_scale = 1,
              design_matrix = NULL,
              integral_grid_n = 101,
              xmin = NULL,
              xmax = NULL,
              x_time_spacing = 0.25,
              x_min_count_distinct = 10,
              norm_to_xrange = TRUE,
              K_boot = 1 ## size of bootstrap simulation
             ){


                   metric = match.arg( metric )
      bootstrap_statistic = match.arg( bootstrap_statistic )
       bootstrap_behavior = match.arg( bootstrap_behavior )

###
### Figure out the reference group for Dunnett's contrasts
###
    if( is.null( reference_Dunnett ) ){
      reference_index <- 1
      reference_Dunnett <- levels( clean_DF[ , 'group' ] )[reference_index]
    } # end of ' if( is.null( reference_Dunnett ) ){...'

    if( ! is.null( reference_Dunnett ) ){
      
      reference_index <-  match( reference_Dunnett, levels( clean_DF[ , 'group' ] ) )
      
      if( is.na( reference_index ) ){
        stop_msg_levels_string <- paste( levels( clean_DF[,'group'] ), collapse = '  ' )
        stop(
             'error in maeve:::bootstrap_parallel():\n\n  ',
              reference_Dunnett,
             '\n\n',
             'not found in the group_name factor levels:\n\n  ',
              stop_msg_levels_string,
             '\n\n'
             )
      } # end of 'if( is.na( reference_index ) ){...'
      
    } # end of 'if( ! is.null( reference_Dunnett ) ){...'



                   
     if( 'AUC_originalScale' == bootstrap_statistic && !is.function( full_inverse_function ) ) {
                     
        if( is.null( full_inverse_function ) ){ # case when it was not provided at all
         stop(
              'error in maeve:::bootstrap_parallel(): ',
              '"full_inverse_function" must be provided when bootstrap_statistic = "AUC_originalScale"\n'
             )
        }

        stop( # case when it was provided, but is not a valid function.
             'error in maeve:::bootstrap_parallel(): ',
             'the "full_inverse_function" argument provided is not a valid R function, ',
             'but a valid function is required when bootstrap_statistic = "AUC_originalScale"\n'
            )

      } # end of 'if( 'AUC_originalScale' == bootstrap_statistic && !is.function( full_inverse_function ) ) {...
      

                   
      pred_name <- switch( metric,
                         ## linear mixed model
                            linear = 'pred_lin',
                         ## GAM spline predictor from a GAMM
                              ITGR = 'pred_gam',
                               AUC = 'pred_gam',
                        ## Piecewise-linear mixed model
                          ITGR_pwl = 'pred_pwl',
                           AUC_pwl = 'pred_pwl',
                        ## Polynomial basis mixed model
                         ITGR_poly = 'pred_poly',
                          AUC_poly = 'pred_poly',
                        ## no match -- stop.
                          stop( 'error in maeve:::bootstrap_parallel():', metric, 'is not a recognized option for "metric".' )
                          ) # end of 'switch( metric,...'

      
      md <- with( model_list,
                 switch( metric, # get the model implied by the endpoint provided
                         ## linear mixed model
                           linear = md4_lmer,
                         ## GAM spline predictor from a GAMM
                             ITGR = md4_gamm4$gam,
                              AUC = md4_gamm4$gam,
                        ## Piecewise-linear mixed model
                         ITGR_pwl = md4_lmer_pwl,
                          AUC_pwl = md4_lmer_pwl,
                        ## Polynomial basis mixed model
                        ITGR_poly = md4_lmer_poly,
                         AUC_poly = md4_lmer_poly,
                        ## no match -- stop.
                        stop( 'error in bootstrap_parallel():', metric, 'is not a recognized "metric" option.' )
                        ) # end of 'switch( metric,...'
                 ) # end of 'with( model_list,...'

      
      ###  Create "even_dat', an evenly-spaced grid of times, by group.
      even_dat <- 
        clean_DF %>%
        dplyr::filter(  !is.na( x ) ) %>%              
        dplyr::filter(  !is.na( !!dplyr::sym( pred_name ) ) ) %>%              
        dplyr::select( !!dplyr::sym('group'), !!dplyr::sym('x') ) %>%
        base::unique() %>%
        base::droplevels()

      ### Interruption in the creation of the data.frame "even_dat":"
      ### Check on whether separate values of xmin and / or xmax were provided.
      ### If not, we need to get them from the current "even_dat".
      ###
      if( is.null( xmin ) ) { xmin <- min( even_dat$x )  } # use lowest  value from clean_DF that could be predicted via the GAM
      if( is.null( xmax ) ) { xmax <- max( even_dat$x )  } # use highest value from clean_DF that could be predicted via the GAM
      
      if( ! ( is.numeric( xmin ) & is.numeric( xmax ) & ( xmin <= xmax ) ) ){ # very basic error checks.
          stop( 'error in maeve:::bootstrap_parallel(): xmin & xmax must be correctly ordered numerics' )
      }
     ###

      even_dat <- # complete creation of "even_dat":
       with( even_dat, expand.grid( x = seq( xmin, xmax, by = x_time_spacing ), group = unique( group ) ) )

      if( length(unique(even_dat$x)) < x_min_count_distinct ){
        warning( paste0("Warning: spline is evaluated at ",
                        length(unique(even_dat$x)),
                        " points. Consider decreasing 'x_time_spacing'",
                        " in maeve:::bootstrap_parallel()"
                        )
                )
      }
      
      
      if( pred_name == 'pred_lin' ){ # simple linearmixed model case
        ## NB: commands in this 'if(...' block all assume that
        ## the model object 'md' is a 'merMod' object from the 'lme4::lmer' function with a simple linear basis.
        
        fixed_effects_lin <- lme4::fixef( md )
        ## 20180909:
        ## Next, define the design matrix for simple linear regression but evaluated at the 'even_dat' values 
        ## for group and 'x' values (e.g., time, concentration).  I thought this could be done directly with
        ## "model.matrix( md, data = even_dat )", but that uses the data associated with the model object and
        ## ignores the 'even_dat' data.frame provided.  For now, we add a hack to get model.matrix to recognize
        ## the data.frame provided.
        formula_fixed_effects_lin <- formula( '~ group + group:x' )

        XX <- model.matrix( formula_fixed_effects_lin, data = even_dat ) # design matrix evaluated at even_dat.
           
        beta_boot <- ### this is the randomly chosen fixed effect vector for the parametric bootstrap. 
          switch( bootstrap_behavior,
                  fixed = matrix( fixed_effects_lin, ncol = K_boot, nrow = length( fixed_effects_lin ) ),
                 random = (MASS::mvrnorm( n = K_boot, mu = fixed_effects_lin, Sigma = vcov( md, unconditional = TRUE ) * bootstrap_covariance_scale ) %>% t %>% matrix( ncol = K_boot ) )
                 )

      } # end of 'if( pred_name == 'pred_lin' ){ ...'


      
      if( pred_name == 'pred_gam' ){
        ## Design matrix from the gamObject, with the new, evenly-spaced data:
        ## NB: commands in this 'if(...' block all assume that
        ## the model object 'md' is a 'gam' object from the 'mgcv' package.
          XX <- predict( md, newdata = even_dat, type = 'lpmatrix' )
        
        beta_boot <- ### this is the randomly chosen fixed effect vector for the parametric bootstrap. 
          switch( bootstrap_behavior,
                  fixed  = matrix( coef( md ), ncol = K_boot, nrow = length( coef( md ) ) ),
                  random = (MASS::mvrnorm( n = K_boot, mu = coef( md ), Sigma = vcov( md, unconditional = TRUE ) * bootstrap_covariance_scale ) %>% t %>% matrix( ncol = K_boot ) )
                 )

      } # end of 'if( pred_name == 'pred_gam' ){ ...'


      if( pred_name == 'pred_pwl' ){ # piecewise-linear case
        ## NB: commands in this 'if(...' block all assume that
        ## the model object 'md' is a 'merMod' object from the 'lme4::lmer' function with a piecewise linear basis.

        fixed_effects_pwl <- lme4::fixef( md )
           XX <- even_dat %>%
                 dplyr::select( group, x ) %>%
                 construct_design_matrix_from_basis( 'tent', group_name = 'group', x_name = 'x' )
           
        beta_boot <- ### this is the randomly chosen fixed effect vector for the parametric bootstrap. 
          switch( bootstrap_behavior,
                  fixed = matrix( fixed_effects_pwl, ncol = K_boot, nrow = length( fixed_effects_pwl ) ),
                 random = (MASS::mvrnorm( n = K_boot, mu = fixed_effects_pwl, Sigma = vcov( md, unconditional = TRUE ) * bootstrap_covariance_scale ) %>% t %>% matrix( ncol = K_boot ) )
                 )

      } # end of 'if( pred_name == 'pred_pwl' ){ ...'


      if( pred_name == 'pred_poly' ){ # piecewise-linear case
        ## NB: commands in this 'if(...' block all assume that
        ## the model object 'md' is a 'merMod' object from the 'lme4::lmer' function with a simple polynomial basis.

        fixed_effects_poly <- lme4::fixef( md )
           XX <- even_dat %>%
                 dplyr::select( group, x ) %>%
                 construct_design_matrix_from_basis( 'poly', group_name = 'group', x_name = 'x', use_poly_coefs = TRUE )
           
        beta_boot <- ### this is the randomly chosen fixed effect vector for the parametric bootstrap. 
          switch( bootstrap_behavior,
                  fixed = matrix( fixed_effects_poly, ncol = K_boot, nrow = length( fixed_effects_poly ) ),
                 random = (MASS::mvrnorm( n = K_boot, mu = fixed_effects_poly, Sigma = vcov( md, unconditional = TRUE ) * bootstrap_covariance_scale ) %>% t %>% matrix( ncol = K_boot ) )
                 )

      } # end of 'if( pred_name == 'pred_poly' ){ ...'

      
      boot_DF_names <- c( 'group', 'x', paste( 'bootrep', maeve::lead_char( 1:K_boot ), sep = '_' )  )





      
    if( bootstrap_statistic == 'AUC_originalScale' ){ # This block will assess uncertainty in AUC, TGI, TGI_baseline.
    ## This block should work with any of the four models selected (linear, spline, piecewise, or simple polynomial)
      
      even_dat                          %>%
      dplyr::select( !!dplyr::sym('group'), !!dplyr::sym('x') )   %>%
      base::data.frame( Y_star = full_inverse_function( XX %*% beta_boot ) ) %>%
      stats::setNames( boot_DF_names  ) %>%
      dplyr::arrange( group, x )        %>%
      base::unique()                    %>%
      tidyr::gather( key = 'replicate', value = 'y_origScale', -group, -x ) -> local_DF 

      ### Record the *first* time point's predicted value by group & replicate:
      first_timepoint_DF <-
        local_DF %>%
        dplyr::filter( x == min( x ) ) %>%
        dplyr::select( -x ) %>%
        dplyr::rename( y_first_timepoint = y_origScale )

      ### Find the time-normalized Area-Under-Curve for each group & replicate:
      AUC_DF <-
        local_DF %>%
      ## Define "xrange_norm_width" to use consistently with normalization
      ## in AUC, TGI, and TGI_baseline.
        dplyr::mutate( xrange_norm_width = diff( range( x ) ) ) %>%
        dplyr::group_by( group, replicate,
                         xrange_norm_width ### added 20171219
                        ) %>%
      ## NB: denominator in the next line is diff( range( x ) ) when norm_to_xrange == TRUE, 1 otherwise.
        dplyr::summarise( AUC = sum( maeve::simpson_coefficients( x ) * y_origScale ) / ifelse( norm_to_xrange, unique(xrange_norm_width), 1 ) ) %>%  
        data.frame()          
      
      joint_DF <-
        dplyr::left_join( first_timepoint_DF, AUC_DF, by = c('group', 'replicate') ) %>%
        plyr::ddply( .var = 'replicate',
                     .fun = function(x) data.frame(
                                                    x,
                                                    AUC_reference     = with( x, AUC[ group == reference_Dunnett ] ),
                                          y_first_timepoint_reference = with( x, y_first_timepoint[ group == reference_Dunnett ] )
                                                    )
                     ) %>% ### end of "ddply( .var = 'replicate', ... "

### In the next step, the "TV_change" value can be defined either as
###                     
### (1) Fractional change in the current group's AUC from ITS OWN baseline, or
###                     
### (2) Fractional change in the current group's AUC from REFERENCE GROUP baseline.
###
  dplyr::mutate( TV_change =   AUC - y_first_timepoint * ifelse( norm_to_xrange, 1, unique(xrange_norm_width) ) ) %>%                         
    plyr::ddply( .var = 'replicate',
                 .fun = function(x) data.frame( x, TV_change_reference = with( x, TV_change[ group == reference_Dunnett ] ) )
                ) %>%
### Changed "replicate" to "boot_replicate" here to attempt to harmonize terminology between the 'TGI' and 'Effect_Duration' sections.
  dplyr::mutate( replicate = factor( replicate ) ) %>%
  dplyr::select( group, replicate, xrange_norm_width,
                    ##
                 y_first_timepoint,
                 y_first_timepoint_reference,
                    ##
                 AUC,
                 AUC_reference,
                    ##
                 TV_change,
                 TV_change_reference
                    ##
                   ) %>%
     dplyr::mutate(

### Two definitions of tumor growth inhibition:
### (1) Find original scale AUC for each group as a fraction of reference.
###     Subtract this ratio from "1" and multiply by "100" to arrive at
###    "percent inhibition":
                   TGI_approx          = ( 1 - AUC / AUC_reference ) * 100,
### (2) Find original scale change of AUC from its first-day baseline defined as
###      ( AUC_trt / (range of days) ) /  First_trt_day_AvgValue - 1 ...   This is in "TV_change", defined above.
###                   divided by                   
###      ( AUC_ctrl / (range of days) ) /  First_ctrl_day_AvgValue - 1 ... This is in "TV_change_reference", defined above.
###
### At the end, subtract the ratio from "1" and multiply by "100" to put the metric on the "percentage inhibition" scale:                   
                   TGI_approx_baseline = ( 1 - TV_change  / TV_change_reference ) * 100
                   ) -> TGI_DF 

  return( TGI_DF )

 } # end of 'if( bootstrap_statistic 'AUC_originalScale' ){...'




      
  if( bootstrap_statistic == 'Effect_Duration' ){  # This block will assess uncertainty in effect_start, effect_end, effect_duration

### Need to estimate first derivatives from stats::spline() interpolant of the fitted values
### but first we need those...  Start here:    

    even_dat                             %>%
    dplyr::select( !!dplyr::sym('group'),  !!dplyr::sym('x') )      %>%
    base::data.frame( XX %*% beta_boot ) %>% # bootstrap predicted values.
    stats::setNames( boot_DF_names )     %>%
    dplyr::arrange( group, x )           %>%
    base::unique()                       %>%
    tidyr::gather( key = 'boot_replicate', value = 'y_hat_star', -group, -x ) -> local_DF

### Need code to extract, for each bootstrap replicate, the fitted values from
### the baseline reference group and copy them over next to each group (including itself)
### aligned with the x-values:    

tmp2 <-     
    local_DF %>%
      plyr::ddply( .var = c('boot_replicate','group'),
                   .fun = function( DAT ){
                          DAT %<>% dplyr::mutate( first_derivative = approximate_first_derivative( x = x, y = y_hat_star ) )
                   }
                 )

tmp3 <-
  tmp2 %>%
    plyr::ddply( .var = 'boot_replicate',
                 .fun = function( DAT ){
                   OUT <- 
                     DAT %>%
                       dplyr::filter( group == reference_Dunnett ) %>%
                       dplyr::select( x, first_derivative ) %>%
                       dplyr::rename( base_first_derivative = first_derivative ) %>%
                       dplyr::left_join( DAT, . , by = 'x' )      
                 }
                )

### Now need to pass the two columns of derivative values to the function determine_Effect_Duration()
tmp4 <-
  tmp3 %>%
    plyr::ddply( .var = c('boot_replicate','group'),
                 .fun = function( DAT ){
                   OUT <-
                     DAT %>%
                       determine_Effect_Duration(
                                   colname_deriv1 = 'first_derivative',
                               colname_ref_deriv1 = 'base_first_derivative'
                                                 ) %>%
                     data.frame() # main function returns a list, not a data.frame
                 }
                )
      

### 20190114: Set 'boot_replicate' to be a factor, not a character string. Also, put 'group' first, left-to-right:
    tmp4 %<>%
      dplyr::mutate( boot_replicate = factor( boot_replicate, levels = sort( unique( as.character( boot_replicate ) ) ) ) ) %>%
      dplyr::select( group, boot_replicate, effect_start, effect_end, effect_duration )
    
 return( tmp4 )
    
  }
      
stop('error in bootstrap_parallel(): function should have returned a value or stopped before this -- something is wrong...')
      
} ### end of function definition "bootstrap_parallel <- function(...){"
