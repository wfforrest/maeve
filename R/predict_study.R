#' Take fit longitudinal models and raw data and return data frames with various predicted values.
#'
#' @author Bill Forrest <forrest@@gene.com>
#'
#' @param model_list list with two components returned from maeve:::models_in_list() and/or maeve::model_study().
#' @param clean_DF_restricted data frame with data restricted to those used in fitting the models.  Most often set to 'clean_DF_restricted' data.frame returned from maeve::model_study().
#' @param clean_DF_full_xrange data frame including data beyond that used for the modeling.  Most often set to NULL or to the 'clean_DF_full_xrange' data.frame returned from maeve::model_study().
#' @param return_list logical: return a whole bunch of output in a list.
#'
#' @param reference_Dunnett     character.      See ?maeve_options().
#' @param modeling_data_frame   data.frame.     See ?maeve_options().
#' @param full_study_data_frame data.frame.     See ?maeve_options().
#' @param metric                character.      See ?maeve_options().
#' @param progress              logical.        See ?maeve_options().
#' @param contrast              character.      See ?maeve_options().
#' @param truncate_fit          logical.        See ?maeve_options().
#' @param group_name            character.      See ?maeve_options().
#' @param subject_ID            character.      See ?maeve_options().
#' @param x_name                character.      See ?maeve_options().
#' @param endpoint_name         character.      See ?maeve_options().
#' @param x_pred_type           character.      See ?maeve_options().
#' @param x_pred_vec            numeric vector. See ?maeve_options().
#' @param x_pred_interior_grid  logical.        See ?maeve_options().
#' @param x_pred_spacing        numeric.        See ?maeve_options().
#' @param include_newdata_ID    logical.        See ?maeve_options().
#' @param N_integration_grid    numeric.        See ?maeve_options() for documentation.
#' @param add_to_endpoint       numeric.        See ?maeve_options().
#' @param trans_func_char       character.      See ?maeve_options(). 
#' @param inv_func_char         character.      See ?maeve_options().
#' @param test_func_x           numeric.        See ?maeve_options().
#'
#' @return An R data.frame OR an R list with several different data.frames & other objectd, depending on "return_list" value.
#'
#' @examples
#'  cat('Example for predict_study() in dontrun code block.')
#'  \dontrun{
#'  data( vismodegib )
#'  vismo21 <- dplyr::filter( vismodegib, DAY_OF_STUDY <= 21 )
#'  model_list <- model_study( vismo21 )
#'  pred_data_frame = predict_study( model_list )
#' }
#'
#' @author Bill Forrest \email{forrest@@gene.com}
#'
#' @references \url{www.r-project.org}
#'
#' @keywords data.frame
#' @export
#'
predict_study <-
  function( model_list,
            clean_DF_restricted  = NULL,
            clean_DF_full_xrange = NULL,
            return_list          = FALSE, # return additional data frames, etc. in a list.
            ##
            reference_Dunnett     = maeve_options("reference_Dunnett"),
            modeling_data_frame   = maeve_options("modeling_data_frame"),
            full_study_data_frame = maeve_options("full_study_data_frame"),
            metric                = maeve_options("metric"),
            progress              = maeve_options("progress"),
            contrast              = maeve_options("contrast"),
            truncate_fit          = maeve_options("truncate_fit"),
            ##
            group_name            = maeve_options("group_name"),
            subject_ID            = maeve_options("subject_ID"),
            x_name                = maeve_options("x_name"),
            endpoint_name         = maeve_options("endpoint_name"),
            ##
            x_pred_type           = maeve_options("x_pred_type"),
            x_pred_vec            = maeve_options("x_pred_vec"),
            x_pred_spacing        = maeve_options("x_pred_spacing"),
            x_pred_interior_grid  = maeve_options("x_pred_interior_grid"),
            include_newdata_ID    = maeve_options("include_newdata_ID"),
            ##
            N_integration_grid    = maeve_options('N_integration_grid'),
            ##
            add_to_endpoint       = maeve_options("add_to_endpoint"),
            trans_func_char       = maeve_options("trans_func_char"),
            inv_func_char         = maeve_options("inv_func_char"),
            test_func_x           = maeve_options("test_func_x")
            ##
           ){


    ## If no data.frame restricted to the modeling values was passed,
    ## try to get it from maeve_options('modeling_data_frame'), which is
    ## automatically set from *within* maeve::model_study() if
    ## maeve_options('autoset_model_data') == TRUE.
    if( is.null( clean_DF_restricted ) ){
      if( is.null( modeling_data_frame ) ){ # see value in maeve_options('modeling_data_frame')
        stop( 'error in maeve::predict_study(): ',
              'both "clean_DF_restricted" and maeve_options("modeling_data_frame") ',
              'are NULL, but one must be an applicable data.frame'
             )
      } else{ ## alternative to 'if( is.null( modeling_data_frame ) ){...'
         stopifnot( is.data.frame( maeve_options( 'modeling_data_frame' ) ) ) # "modeling_data_frame" needs to be a data.frame.
         clean_DF_restricted  <-   maeve_options( 'modeling_data_frame' )
      }
    }


    ## If no data.frame ("DF") based on the full data was passed,
    ## try to get it from maeve_options('full_study_data_frame'), which is
    ## automatically set from *within* maeve::model_study() if
    ## maeve_options('autoset_full_study_data') == TRUE.
    if( is.null( clean_DF_full_xrange ) ){
      if( is.null( full_study_data_frame ) ){ # see value in maeve_options('full_study_data_frame')
        stop( 'error in maeve::predict_study(): ',
              'both "clean_DF_full_xrange" and maeve_options("full_study_data_frame") ',
              'are NULL, but one must be an applicable data.frame'
             )
      } else{
         stopifnot( is.data.frame( maeve_options( 'full_study_data_frame' ) ) ) # "full_study_data_frame" needs to be a data.frame.
         clean_DF_full_xrange  <-  maeve_options( 'full_study_data_frame' )
      }
    }

    
    ## Figure out the reference group for Dunnett's contrasts
    if( is.null( reference_Dunnett ) ){
      reference_index <- 1
      reference_Dunnett <- levels( clean_DF_restricted[ , group_name ] )[reference_index]
    } ## end of ' if( is.null( reference_Dunnett ) ){...'

      
    if( ! is.null( reference_Dunnett ) ){
      
        reference_index <-  match( reference_Dunnett, levels( clean_DF_restricted[ , group_name ] ) )

        stopifnot( length( reference_index ) > 0 )
      
        if( is.na( reference_index ) ){
          ##            
          stop_msg_string <- paste( levels( clean_DF_restricted[, group_name ] ), collapse = '\n ' )
          stop( 'error in maeve::predict_study(): ',
                 reference_Dunnett,
                'not found in the group_name factor levels: ',
                 stop_msg_string
              )
          ##
        } # end of 'if( is.na( reference_index ) ){...'
      
    } # end of 'if( ! is.null( reference_Dunnett ) ){...'

    
    if( length( metric ) == 0 ){
        stop( 'error in maeve::predict_study(): at least one metric must be provided' )
    }

    
    ## Check the transformation function & assign it & its inverse:
    func_list <- check_trans_func( trans_func_char, inv_func_char, test_func_x )
    trans_func <- func_list$trans_func; inv_func <- func_list$inv_func

    md4_lmer      <- model_list[['md4_lmer']]
    md4_gamm4     <- model_list[['md4_gamm4']]
    md4_lmer_pwl  <- model_list[['md4_lmer_pwl']]
    md4_lmer_poly <- model_list[['md4_lmer_poly']]

    re_ID <- extract_random_effects( md4_lmer, md4_gamm4, md4_lmer_pwl, md4_lmer_poly )

    ## if md4_gamm4 was fit, execute this section to get first derivative fits:
    if( !is.null(md4_gamm4) && any( c('AUC', 'ITGR') %in% metric ) ){
      ##
      ## "gamm_AUC"
      ## The AUC of the the growth curve, by group.
      if( progress ){
          cat( paste0('Starting leia analysis of to get derivatives.\n') )
      }
      ##
      ## "gamm_derivative"
      ## The AUC of the the first derivatives of the growth curves, by group.
      ## (Note that, per the Fundamental Theorem of Calculus, this is just the
      ##  difference in a group's growth curve values at the endpoints of the
      ##  time interval, but the "average growth rate" is in part what makes
      ##  this a compelling measure, IMO, and I already had leia() written!).
      ##
      gamm4_leia_deriv1 <-
        maeve::leia( md4_gamm4$gam,
                     derivative = 1,
                     return_list = TRUE,
                     contrast = contrast,
                     N = N_integration_grid, # number points for Simpson's Rule.
                     xname = 'x'
                    )
      ##
      ## 20180616: Added this to deal succinctly with the one-group situation.
      if( nlevels( clean_DF_restricted$group ) == 1 ){
          colnames( gamm4_leia_deriv1$Xp )[1] <- 'group' # gets misnamed in the 1-group scenario.
      }
      ##
    } else{ ## alternative to 'if( !is.null(md4_gamm4) && any( c('AUC', 'ITGR') %in% metric ) ){...'

      gamm4_leia_deriv1 <- NULL

    } # end of 'else{...' alternative to ' if( any( c('AUC', 'ITGR') %in% metric ) ){ ...}'


    
    ## add the group-predicted curve to the data.frame:
    newdata_gam <-
         construct_newdata( clean_DF_restricted,
                            ## If there were any options passed to predict_study(), pass them on:
                            x_pred_type          = x_pred_type,
                            x_pred_vec           = x_pred_vec,
                            x_pred_spacing       = x_pred_spacing,
                            x_pred_interior_grid = x_pred_interior_grid,
                            include_newdata_ID   = include_newdata_ID
                           )

    
    ## Compute the group-fitted response values, and ID-specific fitted
    ## responses (with BLUPs for intercepts & slopes):
    ##

    ## Make a data.frame with the BLUP random effects for each ID:
    full_newdata_DF <-
      cbind( newdata_gam, in_pred_x_set = TRUE ) %>%
      round_numerics() %>% # maeve function to facilitate joining by numeric columns
      dplyr::full_join( ( clean_DF_restricted %>% round_numerics()  ), by = c( 'group', 'ID', 'x' ) ) %>%
      ## Every row originally from clean_DF_restricted should have 'model_x_value' TRUE by definition
      ## Rows from newdata_gam but NOT from clean_DF_restricted should have this set to FALSE:
      dplyr::mutate( model_x_value = ifelse( is.na( model_x_value ), FALSE, model_x_value ) ) %>% 
      dplyr::arrange( group, ID, x )
      
    ## Impute missing covarate values from the full_join() of interpolated predictions and actual observations:
    full_newdata_DF %<>% impute_NA_covariate( 'group', group_name ) # harmonize internal 'group' with group_name moniker.
    full_newdata_DF %<>% impute_NA_covariate(    'ID', subject_ID ) # harmonize internal 'ID'    with subject_ID moniker.
    full_newdata_DF %<>% impute_NA_covariate(     'x',     x_name ) # harmonize internal 'x'     with x_name moniker.
    
    re_data_frame <-
      re_ID[['re_lmer']] %>%  
      dplyr::left_join( re_ID[['re_gamm4']],     by = 'ID' ) %>%
      dplyr::left_join( re_ID[['re_lmer_pwl']],  by = 'ID' ) %>%
      dplyr::left_join( re_ID[['re_lmer_poly']], by = 'ID' )      


    get_in_range_x_values <- 
      function(ZZ){
      ## local function to determine which predicted values are
      ## within the ID-specific range of the x-values used for modeling
      ## with the given subject.
                  xr <- (ZZ %>%
                         dplyr::filter( model_x_value ) %>%
                         dplyr::select(x) %>%
                         unlist %>%
                         range
                         );
                  in_range <- ZZ$x >= min(xr) &
                              ZZ$x <= max(xr) &
                              ZZ$in_pred_x_set
                  return( in_range )
    } # end of 'function(ZZ){...'
    
    
    clean_DF_pred <-
    dplyr::left_join( x = full_newdata_DF, y = re_data_frame, by = 'ID' ) %>%
    unique %>%
    dplyr::mutate(
         ## Construct the unconditional (i.e., no random effects) & conditional (i.e., with random effects) predicted
         ## values from the linear mixed model.  Fill in NA if the model was not fit.
         pred_lin    = ifelse( rep( class( md4_lmer ) == "lmerMod", nrow( . ) ) & in_pred_x_set,
                            c( predict( md4_lmer,  na.action = na.exclude, re.form =~ 0, newdata = full_newdata_DF ) ), # use 'newdata = .' instead of 'newdata = newdata_gam'?
                               rep( NA, nrow( . ) )
                              ),
         pred_lin_ID = pred_lin + intercept_re_lmer  + slope_re_lmer * x,
         ##
         ## Construct the unconditional (i.e., no random effects) & conditional (i.e., with random effects) predicted
         ## values from the generalized additive mixed model (GAMM).  Fill in NA if the model was not fit.
         pred_gam    = ifelse( rep( class( md4_gamm4$gam ) == "gam", nrow( . ) ) & in_pred_x_set,
                               predict( md4_gamm4$gam, na.action = na.exclude, newdata = full_newdata_DF  ), # use 'newdata = .' instead of 'newdata = newdata_gam'?
                               rep( NA, nrow( . ) )
                             ),
         pred_gam_ID = pred_gam + intercept_re_gamm4 + slope_re_gamm4 * x,
         ##
         ## Construct the unconditional (i.e., no random effects) & conditional (i.e., with random effects) predicted
         ## values from the piecewise-linear mixed model.  Fill in NA if the model was not fit.
         pred_pwl    = ifelse( rep( class( md4_lmer_pwl ) == "lmerMod", nrow( . ) ) & in_pred_x_set,
                            c(
                              (full_newdata_DF %>%
                               dplyr::select( group, x ) %>%
                               construct_design_matrix_from_basis( 'tent', group_name = 'group', x_name = 'x' )) %*% lme4::fixef( md4_lmer_pwl ) # "X %*% Beta"
                              ), # end of 'c(...'
                              rep( NA, nrow( . ) )
                              ),
         pred_pwl_ID = pred_pwl + intercept_re_lmer_pwl  + slope_re_lmer_pwl * x,
         ##
         ## Construct the unconditional (i.e., no random effects) & conditional (i.e., with random effects) predicted
         ## values from the polynomial basis mixed model.  Fill in NA if the model was not fit.
         pred_poly    = ifelse( rep( class( md4_lmer_poly ) == "lmerMod", nrow( . ) ) & in_pred_x_set,
                             c(
                               (full_newdata_DF %>%
                                dplyr::select( group, x ) %>%
                                construct_design_matrix_from_basis( 'poly', group_name = 'group', x_name = 'x', use_poly_coefs = TRUE )) %*% lme4::fixef( md4_lmer_poly ) # "X %*% Beta"
                              ), # end of 'c(...'
                               rep( NA, nrow( . ) )
                              ),
         pred_poly_ID = pred_poly + intercept_re_lmer_poly  + slope_re_lmer_poly * x
         ##
    ) ## end of 'dplyr::mutate(...' to define "clean_DF_pred"


      
    ## Construct versions that are "normalized to baseline" for
    ##  (1) the raw data
    ##  (2) the group-average linear predictions
    ##  (3) the group-average spline predictions
    ## By default, normalizing to baseline here means that we
    ## divide all ordinate 'y' values by the average ordinate at the first time point,
    ## then subtract '1', then multiply by 100. See function for details.
    clean_DF_pred %<>%  
    plyr::ddply( .var = 'ID',
                 .fun = function(XX){
                              data.frame(XX,
                                     y_norm = normalize_to_first_timepoint(XX, y_name =        'y'),  # by-ID, normed to 1st observation
                              pred_lin_norm = normalize_to_first_timepoint(XX, y_name = 'pred_lin'),  # by-ID, normed to 1st observation
                              pred_gam_norm = normalize_to_first_timepoint(XX, y_name = 'pred_gam'),  # by-ID, normed to 1st observation
                              pred_pwl_norm = normalize_to_first_timepoint(XX, y_name = 'pred_pwl'),  # by-ID, normed to 1st observation
                             pred_poly_norm = normalize_to_first_timepoint(XX, y_name = 'pred_poly'), # by-ID, normed to 1st observation
                           in_model_x_range = get_in_range_x_values(XX) # logical for which predicted values are in-range for the non-truncated "modeled" x-values
                                         )
                            }
                ) %>% ## end of 'plyr::ddply( .var = 'ID', ...'
    dplyr::arrange( group, ID, x ) %>%
    maeve::freeze_factor_levels()
    
    ## Impute missing covarate values from the full_join() of interpolated predictions and actual observations:
    clean_DF_pred %<>% impute_NA_covariate( 'group', group_name ) # harmonize internal 'group' with group_name moniker.
    clean_DF_pred %<>% impute_NA_covariate(    'ID', subject_ID ) # harmonize internal 'ID'    with subject_ID moniker.
    clean_DF_pred %<>% impute_NA_covariate(     'x',     x_name ) # harmonize internal 'x'     with x_name moniker.

      
    ## merge in derivatives of growth curves evaluated at the grid points:
    if( progress ){
        cat('Interpolating estimated derivatives and estimating control baseline growth rate.\n')
    }

    if( 'Xp' %in% names( gamm4_leia_deriv1 ) ){ ## typically, this is executed if "!is.null(gamm4_leia_deriv1) == TRUE"
      clean_DF_pred <-
      plyr::dlply( .data = gamm4_leia_deriv1$Xp,
                   .var  = 'group',
                   .fun  = function(XX){ spline(    x = XX$x,
                                                    y = XX$pred_gam_deriv1,
                                                 xout = unique( clean_DF_pred$x )
                                                )
                                       }
                  ) %>%
      plyr::ldply( .fun = function(XX){ data.frame( x = XX[[1]], pred_gam_deriv1 = XX[[2]] ) } ) %>%
      round_numerics() %>%
      dplyr::left_join( clean_DF_pred %>% round_numerics(), . , by = c( 'group', 'x' ) ) 

 
    } else{ ## alternative to 'if( 'Xp' %in% names( gamm4_leia_deriv1 ) ){...'

      clean_DF_pred <- data.frame( clean_DF_pred, pred_gam_deriv1 = NA )
   
    }

      
    ## Need to add in some code to extract the fitted values from
    ## the baseline reference group so that they can be plotted in
    ## the longitudinal figures.
    base_DF_pred <-
      clean_DF_pred %>%
      dplyr::filter( group == reference_Dunnett ) %>%
      dplyr::select( x, pred_lin, pred_gam, pred_gam_deriv1 ) %>%
      dplyr::rename( base_pred_lin = pred_lin, base_pred_gam = pred_gam, base_pred_gam_deriv1 = pred_gam_deriv1 ) %>%
      unique %>%
      dplyr::arrange( x ) %>%
      maeve::freeze_factor_levels()

    clean_DF_pred <-
      dplyr::left_join(
                       clean_DF_pred %>% round_numerics(),
                        base_DF_pred %>% round_numerics(),
                       ## join these by the x-value column. This should repeat the 'base_DF_pred' values for each distinct group.
                       by = c( 'x' )
                       )


    if( truncate_fit ){

      ## Define a local function to truncate the fitted values to the observed 
      ## vertical range of responses, only when maeve_options('truncate_fit') is TRUE.
      ## This gets called near the end of the function, since we will need all the 
      ## fitted values for some interim summary measures (e.g., RMSE by ID).
      ##    
      truncate_to_y_range <-
        function( yp, y ){ # locally defined function
          in_range_vec <-
            yp <= max( y, na.rm = TRUE ) &
            yp >= min( y, na.rm = TRUE )
         ## Now, keep the predicted value 'yp' when in_range_vec is TRUE,
         ## but return NA for entries for which in_range_vec is FALSE.
         ifelse( in_range_vec, yp, NA )
      }

      ## Define a local function to truncate the fitted values to the x-range
      ## defined by the truncation criterion (determined below) when
      ## maeve_options('truncate_fit') is TRUE.
      ## This gets called near the end of the function, since we will need all the 
      ## fitted values for some interim summary measures (e.g., RMSE by ID).
      ##    
      truncate_to_x_range <-
        function( yp, in_model_x_range ){ # locally defined function
            ifelse( in_model_x_range, yp, NA )
      }

      
      ## If maeve_options('truncate_fit') == TRUE, then automatically set the
      ## fitted values that fall outside the range of the observed response
      ## values (either higher or lower) to NA.
      clean_DF_pred %<>%
        dplyr::group_by(group) %>%
        dplyr::mutate( pred_lin     = truncate_to_y_range( pred_lin,     y ),
                       pred_gam     = truncate_to_y_range( pred_gam,     y ),
                       pred_pwl     = truncate_to_y_range( pred_pwl,     y ),                
                       pred_poly    = truncate_to_y_range( pred_poly,    y ),
                       ##
                       pred_lin_ID  = truncate_to_y_range( pred_lin_ID,  y ),
                       pred_gam_ID  = truncate_to_y_range( pred_gam_ID,  y ),
                       pred_pwl_ID  = truncate_to_y_range( pred_pwl_ID,  y ),
                       pred_poly_ID = truncate_to_y_range( pred_poly_ID, y ),
                       ##
                       pred_lin_norm  = truncate_to_y_range( pred_lin_norm,  y_norm ),
                       pred_gam_norm  = truncate_to_y_range( pred_gam_norm,  y_norm ),
                       pred_pwl_norm  = truncate_to_y_range( pred_pwl_norm,  y_norm ),
                       pred_poly_norm = truncate_to_y_range( pred_poly_norm, y_norm )
        ) ## end of 'dplyr::mutate( pred_lin     = truncate_to_y_range( pred_lin,     y ),...'

        
      clean_DF_pred %<>%
        plyr::ddply( .var = 'group',
                     .fun = function( XX ){
                            XX %<>%
                            dplyr::mutate(
                                      pred_lin       = truncate_to_x_range( pred_lin,  in_model_x_range ),
                                      pred_gam       = truncate_to_x_range( pred_gam,  in_model_x_range ),
                                      pred_pwl       = truncate_to_x_range( pred_pwl,  in_model_x_range ),
                                      pred_poly      = truncate_to_x_range( pred_poly, in_model_x_range ),
                                      ##
                                      pred_lin_ID    = truncate_to_x_range( pred_lin_ID,  in_model_x_range ),
                                      pred_gam_ID    = truncate_to_x_range( pred_gam_ID,  in_model_x_range ),
                                      pred_pwl_ID    = truncate_to_x_range( pred_pwl_ID,  in_model_x_range ),
                                      pred_poly_ID   = truncate_to_x_range( pred_poly_ID, in_model_x_range ),
                                      ##
                                      pred_lin_norm  = truncate_to_x_range( pred_lin_norm,  in_model_x_range ),
                                      pred_gam_norm  = truncate_to_x_range( pred_gam_norm,  in_model_x_range ),
                                      pred_pwl_norm  = truncate_to_x_range( pred_pwl_norm,  in_model_x_range ),
                                      pred_poly_norm = truncate_to_x_range( pred_poly_norm, in_model_x_range )
                                    ) ## end of 'dplyr::mutate(...'
                            } ## end of '.fun = function( XX ){...'
                    ) # end of "plyr::ddply( .var = 'group',..."
    } # end of 'if( truncate_fit ){...'



    ## Need to relevel clean_DF_pred factors named by the maeve_options() variables
    ## maeve_options("group_name") and maeve_options("subject_ID").  I could not figure out
    ## a way to do this gracefully in-line below, where I did a similar thing for the hard-coded
    ## names "group" and "ID" using "forcats".  Future improvement to find a cleaner solution.    
    clean_DF_pred[,group_name] <- factor( clean_DF_pred[,group_name], levels = levels( clean_DF_full_xrange[,group_name] ) )
    clean_DF_pred[,subject_ID] <- factor( clean_DF_pred[,subject_ID], levels = levels( clean_DF_full_xrange[,subject_ID] ) )

    ## This will have all the predicted values re-combined with the raw data that
    ## was NOT used for predictions.  This will be the primary output.    
    clean_DF_pred_full_xrange <-
      clean_DF_pred    %>%
      ## Need to ensure that "clean_DF_pred" has the same factor levels as "clean_DF_full_xrange" for 'group' & 'ID'.
      ## If some data are truncated, then the levels of 'group' & 'ID' in "clean_DF_pred" may have become a proper
      ## subset of the levels for 'group' & 'ID' in "clean_DF_full_xrange". In that case, this should make them match
      ## again:
      dplyr::mutate( group = group %>%
                             forcats::fct_expand(  levels(clean_DF_full_xrange[,'group']) ) %>%
                             forcats::fct_relevel( levels(clean_DF_full_xrange[,'group']) ),
                        ID = ID    %>%
                             forcats::fct_expand(  levels(clean_DF_full_xrange[,   'ID']) ) %>%
                             forcats::fct_relevel( levels(clean_DF_full_xrange[,   'ID']) )
                    ) %>%
      maeve::round_numerics() %>%
      dplyr::full_join( x = clean_DF_full_xrange %>% round_numerics(),
                        y = .,
                        by = unique( c( group_name, 'group', subject_ID, 'ID', x_name, 'x', endpoint_name, 'y', 'y_orig' ) ) # these should all be in common.
                       ) %>%
      (base::unique) %>% # duplicate rows seem to get induced by joining (?). Not clear why.
      dplyr::arrange( group, ID, x ) %>%
      ## Resolve the (I believe inevitable) two columns of 'model_x_value':
      impute_NA_covariate( 'model_x_value.x', 'model_x_value.y' ) %>% # this step should make 'model_x_value.x' & 'model_x_value.y' identical.
      dplyr::rename( model_x_value = model_x_value.x ) %>% # rename the 1st to the now-common-name.
      dplyr::select( -model_x_value.y ) %>%                # drop the 2nd.
      ## 'NA' values for 'in_pred_x_set' should have been taken from 'clean_DF_full_xrange', and so should be FALSE.
      ##
      ## _*QUESTION *_: What to do if there are { group_name, subject_ID, x_name } triples that
      ## are not in *both* the predicted set and the clean_DF_full_xrange set?  This could happen
      ## since the user can request custom predicted ranges.
      ##
      ## Current thinking: These rows would have 'FALSE' for 'model_x_value' but 'TRUE' for 'in_pred_x_set'
      ##      
      dplyr::mutate( in_pred_x_set    = ifelse( is.na( in_pred_x_set ),    FALSE, in_pred_x_set ),
                     in_model_x_range = ifelse( is.na( in_model_x_range ), FALSE, in_model_x_range )
                    ) %>%
      maeve::freeze_factor_levels()


    ## Ensure that the logical column 'in_model_x_range' is adjacent to the two other
    ## logical columns 'model_x_value' and 'in_pred_x_set':
    lead_columns <-
      c( eval( group_name ), eval( subject_ID ), eval( x_name ), eval( endpoint_name ),
         'group', 'ID', 'x', 'y_orig', 'y',
         'model_x_value', 'in_pred_x_set', 'in_model_x_range'
        )

    stopifnot( # check that all these columns are actually present.  They should be.
               all( lead_columns %in% colnames( clean_DF_pred )             ) &
               all( lead_columns %in% colnames( clean_DF_pred_full_xrange ) )
              )

    ## Put the "lead_columns" on the left columns of the data frames:
    clean_DF_pred             %<>% (function(XX){ cbind( dplyr::select( XX, lead_columns ), dplyr::select( XX, -dplyr::one_of(lead_columns) ) ) })
    clean_DF_pred_full_xrange %<>% (function(XX){ cbind( dplyr::select( XX, lead_columns ), dplyr::select( XX, -dplyr::one_of(lead_columns) ) ) })
    
    
    if( return_list ){
    ## This section bracketed by "if( return_list ){...}" computes additional
    ## output, and therefore is needed only if return_list == TRUE.

      ## Make data frame with the random effects estimates from each estimation method.
      ## We merge this with the two subsequent noise estimates.
      re_DF <-
        re_ID[["re_lmer"]] %>%
        dplyr::left_join( y = re_ID[["re_gamm4"]],     by = 'ID' ) %>%
        dplyr::left_join( y = re_ID[["re_lmer_pwl"]],  by = 'ID' ) %>%
        dplyr::left_join( y = re_ID[["re_lmer_poly"]], by = 'ID' )
 
      ## Compute standard deviations of conditional residuals (i.e., residuals from ID-specific fitted curves,
      ## including their random effects).
      SD_by_ID <-
        clean_DF_pred %>%
        dplyr::group_by( group, ID ) %>%
        dplyr::summarise( SD_lin_resid_ID  = sd( y - pred_lin_ID,  na.rm = TRUE ),
                          SD_gam_resid_ID  = sd( y - pred_gam_ID,  na.rm = TRUE ),
                          SD_pwl_resid_ID  = sd( y - pred_pwl_ID,  na.rm = TRUE ),
                          SD_poly_resid_ID = sd( y - pred_poly_ID, na.rm = TRUE )
                         ) %>%
        dplyr::left_join( re_DF, by = 'ID' ) %>%
        dplyr::arrange( group, ID ) %>%
        maeve::freeze_factor_levels() %>%
        data.frame()

      ## Compute root-mean-square errors (RMSEs) of conditional residuals (i.e., residuals from ID-specific fitted curves,
      ## including their random effects).
      RMSE_by_ID <-
        clean_DF_pred %>%
        dplyr::group_by( group, ID ) %>%
        dplyr::summarise( RMSE_lin_resid_ID  = ( y - pred_lin_ID  )^2  %>% mean( na.rm = TRUE ) %>% sqrt,
                          RMSE_gam_resid_ID  = ( y - pred_gam_ID  )^2  %>% mean( na.rm = TRUE ) %>% sqrt,
                          RMSE_pwl_resid_ID  = ( y - pred_pwl_ID  )^2  %>% mean( na.rm = TRUE ) %>% sqrt,
                          RMSE_poly_resid_ID = ( y - pred_poly_ID )^2  %>% mean( na.rm = TRUE ) %>% sqrt
                         ) %>%
        dplyr::left_join( re_DF, by = 'ID' ) %>%
        dplyr::arrange( group, ID ) %>%
        maeve::freeze_factor_levels() %>%
        data.frame()

      data_list = list( clean_DF_pred = clean_DF_pred,
                        clean_DF_pred_full_xrange = clean_DF_pred_full_xrange,
                        re_ID = re_ID,
                        SD_by_ID = SD_by_ID,
                        RMSE_by_ID = RMSE_by_ID,
                        func_list = func_list, ### transformation function and its inverse
                        func_char_list = list( trans_func = trans_func_char, inv_func = inv_func_char, add_to_endpoint = add_to_endpoint ) ### character names of transform & inverse
                       )

    } else{ # alternative to 'if( return_list ){...'

      data_list <- NULL
        
    }

      
    if( return_list ){
          
        return( data_list ) ## primary data.frame plus many extras.
        
    } else{
        
        return( clean_DF_pred_full_xrange ) ## one row for each predicted value + one row for each observation NOT used in modeling.
        
    }
    
} ## end of "predict_study <- function(...){..."
