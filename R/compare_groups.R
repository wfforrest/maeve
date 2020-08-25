#' Compare groups from fitted models and return confidence intervals & figures
#'
#' @author Bill Forrest <forrest@@gene.com>
#'
#' @param model_list R list with model objects from maeve::model_study().
#' @param xmin numeric lower bound of definite integral
#' @param xmax numeric upper bound of definite integral
#' @param custom_contrast numeric matrix of contrasts with number of columns equal to number of treatment groups and number of rows equal to desired number of contrasts.  Column names must equal treatment group names.
#' @param subtract_starting_value logical: Subtract from all computed AUCs the spline fit at xmin for that group.
#' @param conf_level numeric value in (0,1) to determine the level of confidence intervals returned.
#' @param adjustment_method  character string specifying a method for p-value adjustment.  Setting to 'none' will return unadjusted p-values and confidence intervals.  Setting to anything else (the default is 'single_step') will implement a particular p-value adjustment from multcomp.  Although the p-value adjustment method varies for different options of this parameter, all the options other than 'none' will return the same FWER- or FDR-adjusted confidence intervals implemented via setting "calpha = multcomp:::adjusted_calpha()" in "multcomp:::confint.glht()". Setting adjustment_method = 'none' will return confidence intervals with "calpha = multcomp:::univariate_calpha()" in "multcomp:::confint.glht()".
#' @param extended_output logical determining whether to compute extra output to return to a DIVOS summary table.
#' @param draw_figures logical determining whether to draw figures.  If FALSE, then NULL is returned for each figure.
#' @param facet_wrap_scales character string passed to "ggplot::facet_wrap( ..., scales = ...)" giving scales parameter. Usually 'fixed' or 'free_y' here.
#' @param study_ID_value character string with five digit DIVOS study ID, OR a DivoStudy object.
#' @param reference_Dunnett      character. See ?maeve_options() for documentation.
#' @param modeling_data_frame   data.frame. See ?maeve_options().
#' @param metric                 character. See ?maeve_options() for documentation.
#' @param break_points           numeric vector. See ?maeve_options().
#' @param poly_degree            numeric.   See ?maeve_options().
#' @param progress               logical.   See ?maeve_options() for documentation.
#' @param contrast               character. See ?maeve_options() for documentation.
#' @param xrange_norm_method     character. See ?maeve_options() for documentation.
#' @param N_integration_grid     numeric.   See ?maeve_options() for documentation.
#' @param truncated_group_levels character. See ?maeve_options() for documentation.
#' @param geom_na_rm             logical.   See ?maeve_options() for documentation.
#' @param axis_text_x_size       numeric.   See ?maeve_options() for documentation.
#' @param axis_text_x_angle      numeric.   See ?maeve_options() for documentation.
#' @param axis_text_x_hjust      numeric.   See ?maeve_options() for documentation.
#' @param axis_text_x_vjust      numeric.   See ?maeve_options() for documentation.
#' @param axis_text_y_size       numeric.   See ?maeve_options() for documentation.
#' @param strip_text_size        numeric.   See ?maeve_options() for documentation.
#' @param legend_text_size       numeric.   See ?maeve_options() for documentation.
#' @param legend_position_char   character. See ?maeve_options() for documentation.
#'
#' @return An R list with named output.
#'
#' @examples
#'  ### cat('Currently no running example for compare_groups(). See "\dontrun()" text.')
#'  \dontrun{
#'   data( vismodegib )
#'   vismo21 <- dplyr::filter( vismodegib, DAY_OF_STUDY <= 21 )
#'   cg_out  <- vismo21 %>% maeve::model_study() %>% maeve::compare_groups()
#' }
#'
#' @author Bill Forrest \email{forrest@@gene.com}
#'
#' @references \url{www.r-project.org}
#'
#' @keywords data.frame
#' @export
#'
compare_groups <-
  function( model_list,
            xmin              = NULL,
            xmax              = NULL,
            custom_contrast   = NULL,
            subtract_starting_value = TRUE,
            conf_level        = 0.95,
            adjustment_method = c( "single-step", "Shaffer", "Westfall", "free", "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none" ),
            extended_output   = FALSE,
            draw_figures      = TRUE, # Set to FALSE to avoid drawing figures in results.
            facet_wrap_scales = c('fixed', 'free_y', 'free_x', 'free'),
            study_ID_value    = '',
            ##
            reference_Dunnett      = maeve_options("reference_Dunnett"),
            modeling_data_frame    = maeve_options("modeling_data_frame"),
            metric                 = maeve_options('metric'),
            break_points           = maeve_options("break_points"),
            poly_degree            = maeve_options("poly_degree"),
            progress               = maeve_options('progress'),
            contrast               = maeve_options('contrast'),
            xrange_norm_method     = maeve_options('xrange_norm_method'),
            N_integration_grid     = maeve_options('N_integration_grid'),
            truncated_group_levels = maeve_options('truncated_group_levels'),
            geom_na_rm             = maeve_options('geom_na_rm'),
            axis_text_x_size       = maeve_options('axis_text_x_size'),
            axis_text_x_angle      = maeve_options('axis_text_x_angle'),
            axis_text_x_hjust      = maeve_options('axis_text_x_hjust'),
            axis_text_x_vjust      = maeve_options('axis_text_x_vjust'),
            axis_text_y_size       = maeve_options('axis_text_y_size'),
            strip_text_size        = maeve_options('strip_text_size'),
            legend_text_size       = maeve_options('legend_text_size'),
            legend_position_char   = maeve_options('legend_position_char')
           ){

    facet_wrap_scales = match.arg( facet_wrap_scales ) # used only here right now -- not putting it in maeve_options().

    if( 'linear' %in% metric ){

      if( progress ){
          cat(paste0('Performing lme4 comparison of ', study_ID_value, '.\n' ) )
      }

      if( !is.null( model_list[['md4_lmer']] ) ){
    
        group_levels <- levels( attributes( model_list[['md4_lmer']] )[['frame']][['group']] )
    
        if( is.null( group_levels ) ){
          linfct_lmer <- get_lmer_linfct( model_list[['md4_lmer']], contrast, grpnames = 'group',      custom_contrast = custom_contrast, slope_ind_char = 'x' ) # one-group case.
        } else{
          linfct_lmer <- get_lmer_linfct( model_list[['md4_lmer']], contrast, grpnames = group_levels, custom_contrast = custom_contrast, reference_Dunnett = reference_Dunnett )
        } 
      
        glht_lmer <- multcomp::glht( model_list[['md4_lmer']], linfct_lmer )
      
      } ## end of 'if( !is.null( model_list[['md4_lmer']] ) ){...'
  
      if( is.null( model_list[['md4_lmer']] ) ){
           glht_lmer <- NULL
      }

    } else{ ## alternative to 'if( 'linear' %in% metric ){...'

      if( progress ){
          cat(paste0('Skipping lme4 linear comparison.\n' ) )
      }

      glht_lmer <- NULL 

    } # end of 'if( 'linear' %in% metric ){...} else{...}' block





    ## If *NEITHER* of c('AUC', 'ITGR') is in the metrics requested,
    ## then we need to fill in NULL values for both these. Do it now,
    ## then add something non-NULL if we have a prompt and a model
    ## from which to do so.    

    glht_gamm4_leia <- glht_gamm4_leia_deriv1 <- NULL # default values.
    
    ## "gamm_AUC"
    ## The AUC of the the growth curve, by group.
    if( progress ){
        cat( paste0('Starting leia comparison for AUC and/or ITGR.\n') )
    }

    if( 'AUC' %in% metric & !is.null( model_list[['md4_gamm4']] ) ){

    glht_gamm4_leia <-
    maeve::leia( model_list[['md4_gamm4']]$gam,
                      derivative = 0,
                      return_list = TRUE,
                      contrast = contrast,
                      custom_contrast = custom_contrast,
                      reference_Dunnett = reference_Dunnett,
                      N = N_integration_grid, # value around 25 is usually fine; set in maeve_options('N_integration_grid').
                      xname = 'x',
                      xmin = xmin,
                      xmax = xmax,
                      ##  NB: Recommended setting for 'xrange_norm_method':
                      ## "slope_equivalent" <--> 'half_xrange_squared' option in maeve::leia() for "derivative = 0".
                      ## "slope_equivalent" <--> 'xrange'              option in maeve::leia() for "derivative = 1".
                      xrange_norm_method = switch( xrange_norm_method,
                                                  'none'             = 'none',
                                                  'xrange'           = 'xrange',
                                                  'slope_equivalent' = 'half_xrange_squared',
                                                   ## If none of the first three above are a match, stop:
                                                   stop('error in maeve::compare_groups() call to leia(): "xrange_norm_method" must be one of ',
                                                        'c("none", "xrange", "slope_equivalent")'
                                                        )
                                                 ),
                      subtract_starting_value = subtract_starting_value,
                      provide_warnings = TRUE
                     )

    } ## end of 'if( 'AUC' %in% metric & !is.null( model_list[['md4_gamm4']] ) ){...'

    
    ## "gamm_derivative", ('ITGR')
    ## The AUC of the the first derivatives of the growth curves, by group.
    ## Note that, per the Fundamental Theorem of Calculus, this is just the
    ## difference in a group's growth curve values at the endpoints of the
    ## time interval, but the "average growth rate" is in part what makes
    ## this a compelling measure, IMO, and this facilitates by-sub-interval
    ## importance weighting should we want to implement that in future versions
    ## (and leia() was already written!).

    if( 'ITGR' %in% metric & !is.null( model_list[['md4_gamm4']] ) ){

    glht_gamm4_leia_deriv1 <-
          maeve::leia( model_list[['md4_gamm4']]$gam,
                      derivative = 1,
                      return_list = TRUE,
                      contrast = contrast,
                      custom_contrast = custom_contrast,
                      reference_Dunnett = reference_Dunnett,
                      N = N_integration_grid,
                      xname = 'x',
                      xmin  = xmin,
                      xmax  = xmax,
                      ##  NB: Recommended setting for 'xrange_norm_method':
                      ## "slope_equivalent" <--> 'half_xrange_squared' option in maeve::leia() for "derivative = 0".
                      ## "slope_equivalent" <--> 'xrange'              option in maeve::leia() for "derivative = 1".
                      xrange_norm_method = switch( xrange_norm_method, 'none' = 'none', 'xrange' = 'xrange', 'slope_equivalent' = 'xrange' ),
                      subtract_starting_value = subtract_starting_value
                      )
        
    } ## end of 'if( 'ITGR' %in% metric & !is.null( model_list[['md4_gamm4']] ) ){...'




    

    ## Below are four blocks of code to summarise models for
    ##
    ## (1)  AUC for a piecewise  linear lmer() model <-->  'AUC_pwl'.
    ## (2) ITGR for a piecewise  linear lmer() model <--> 'ITGR_pwl'.
    ## (3)  AUC for a polynomial linear lmer() model <-->  'AUC_poly'.
    ## (4) ITGR for a polynomial linear lmer() model <--> 'ITGR_poly'.
    ##
    ## These all call the function maeve:::pia() with different
    ## arguments. For now, we leave them as four separate blocks,
    ## but they can (and perhaps should) be consolidated into
    ## one block called multiple times at some stage, since they
    ## use _most_ of the same parameter values and it could make
    ## maintenance down the road easier.    


    ## (1) AUC for a piecewise  linear lmer() model <-->  'AUC_pwl'  
    if( 'AUC_pwl' %in% metric ){ 
      ## "piecewise linear mixed model"
      ## The AUC of the the piecewise linear growth curve, by group.
        
      if( progress ){
            cat( paste0('Starting maeve:::pia comparison of ', study_ID_value, '.\n') )
      }

      if( is.null( model_list[['md4_lmer_pwl']] ) ){
          glht_lmer_AUC_pwl <- NULL
      }

      if( !is.null( model_list[['md4_lmer_pwl']] ) ){
      ## This is the section where we find AUC for fixed effects from a piecewise linear mixed model.
      ## 
      ## Strategy: Make a fine-grid design matrix based on the currently used 'group_name' and 'x_name'
      ## (i.e., time) variables, and the current break_points.  All these are going to be taken from
      ## maeve_options(), so they need either to be already stored there or passed directly to
      ## maeve::compare_groups().

      if( is.null( modeling_data_frame ) ){
        stop( 'error in maeve::compare_groups(): ',
              'if "AUC_pwl" is in the metric list, then maeve_options("modeling_data_frame")\n ',
              'must be non-NULL; his can be done either directly or by running maeve::model_study() ',
              'with "autoset_modeling_data = TRUE" in the option list.\n\n'
             )
      } ## end of 'if( is.null( modeling_data_frame ) ){...'
   
      glht_lmer_AUC_pwl <-   
          pia( model_list[['md4_lmer_pwl']],
               basis_choice = 'tent',     
               break_points = break_points,   
               degree  = poly_degree,
               N = N_integration_grid,
               local_modeling_data_frame = modeling_data_frame,                     
               derivative = 0,
               return_list = TRUE,
               contrast = contrast,
               custom_contrast = custom_contrast,
               reference_Dunnett = reference_Dunnett,
               ##
               groupname = 'group',
               xname     = 'x',
               ##
               xmin = xmin,
               xmax = xmax,
               ##  NB: Recommended setting for 'xrange_norm_method':
               ## "slope_equivalent" <--> 'half_xrange_squared' option in maeve::leia() for "derivative = 0".
               ## "slope_equivalent" <--> 'xrange'              option in maeve::leia() for "derivative = 1".
               xrange_norm_method = switch( xrange_norm_method,
                                           'none'             = 'none',
                                           'xrange'           = 'xrange',
                                           'slope_equivalent' = 'half_xrange_squared',
                                            ## If none of the first three above are a match, stop:
                                            stop( 'error in maeve::compare_groups() call to maeve:::pia(): "xrange_norm_method" must be one of ',
                                                  'c("none", "xrange", "slope_equivalent")'
                                                 )
                                          ),
               subtract_starting_value = subtract_starting_value,
               provide_warnings = TRUE
              ) ## end of call 'pia( model_list[['md4_lmer_pwl']], ...'

      } ## end of 'if( !is.null( model_list[['md4_lmer_pwl']] ) ){...'
 
    } else{ ## alternative to 'if( 'AUC_pwl' %in% metric ){ ... '

      if( progress ){
          cat(paste0('Skipping AUC piecewise linear comparison.\n' ) )
      }

      glht_lmer_AUC_pwl <- NULL

    } # # end of 'if( 'AUC_pwl' %in% metric ){ ...} else{...}' block




    
    ## (2) ITGR for a piecewise  linear lmer() model <--> 'ITGR_pwl'
    if( 'ITGR_pwl' %in% metric ){ 

      ## "piecewise linear mixed model"
      ## The ITGR of the the piecewise linear growth curve, by group.
      if( progress ){
          cat( paste0('Starting ITGR maeve:::pia comparison of ', study_ID_value, '.\n') )
      }

      if(  is.null( model_list[['md4_lmer_pwl']] ) ){
          glht_lmer_ITGR_pwl <- NULL
      }

      if( !is.null( model_list[['md4_lmer_pwl']] ) ){
        ## This is the section where we find ITGR for fixed effects from a piecewise linear mixed model.
        ## 
        ## Strategy: Make a fine-grid design matrix based on the currently used 'group_name' and 'x_name'
        ## (i.e., time) variables, and the current break_points.  All these are going to be taken from
        ## maeve_options(), so they need either to be already stored there or passed directly to
        ## maeve::compare_groups().

        if( is.null( modeling_data_frame ) ){
          stop( 'error in maeve::compare_groups(): ',
                'if "ITGR_pwl" is in the metric list, then maeve_options("modeling_data_frame")\n ',
                'must be non-NULL.  This can be done either directly or by running maeve::model_study() ',
                'with "autoset_modeling_data = TRUE" in the option list.\n\n'
               )
        }
   
        glht_lmer_ITGR_pwl <-   
          pia( model_list[['md4_lmer_pwl']],
               basis_choice = 'tent',     
               break_points = break_points,   
               degree  = poly_degree,
               N = N_integration_grid,
               local_modeling_data_frame = modeling_data_frame,                     
               derivative = 1,
               return_list = TRUE,
               contrast = contrast,
               custom_contrast = custom_contrast,
               reference_Dunnett = reference_Dunnett,
               ##
               groupname = 'group',
                   xname = 'x',
               ##
               xmin = xmin,
               xmax = xmax,
               ##  NB: Recommended setting for 'xrange_norm_method':
               ## "slope_equivalent" <--> 'half_xrange_squared' option in maeve::leia() for "derivative = 0".
               ## "slope_equivalent" <--> 'xrange'              option in maeve::leia() for "derivative = 1".
               xrange_norm_method = switch( xrange_norm_method,
                                           'none'             = 'none',
                                           'xrange'           = 'xrange',
                                           'slope_equivalent' = 'xrange', # need 'xrange' rather than 'half_xrange_squared' for derivative == 1.
                                            ### If none of the first three above are a match, stop:
                                            stop( 'error in maeve::compare_groups() call to pia(): "xrange_norm_method" must be one of ',
                                                  'c("none", "xrange", "slope_equivalent")'
                                                 )
                                          ),
               subtract_starting_value = subtract_starting_value,
               provide_warnings = TRUE
              ) # end of call 'pia( model_list[['md4_lmer_pwl']], ...'

      } ## end of 'if( !is.null( model_list[['md4_lmer_pwl']] ) ){...'
 
    } else{ ## This executes if ('ITGR_pwl' %in% metric) is FALSE.

      if( progress ){
          cat(paste0('Skipping ITGR piecewise linear comparison.\n' ) )
      }

      glht_lmer_ITGR_pwl <- NULL

    } ## end of 'if( 'ITGR_pwl' %in% metric ){ ...} else{...}' block



    ## (3) AUC for a polynomial linear lmer() model <-->  'AUC_poly'
    if( 'AUC_poly' %in% metric ){ 

      ## "polynomial basis linear mixed model"
      ## The AUC of the the polynomial basis linear mixed model curve, by group.
      if( progress ){
          cat( paste0('Starting maeve:::pia comparison of ', study_ID_value, ' for "AUC_poly".\n') )
      }

      if( is.null( model_list[['md4_lmer_poly']] ) ){
          glht_lmer_AUC_poly <- NULL
      }


      if( !is.null( model_list[['md4_lmer_poly']] ) ){
        ## This is the section where we find AUC for fixed effects from a polynomial linear mixed model.
        ## 
        ## Strategy: Make a fine-grid design matrix based on the currently used 'group_name' and 'x_name'
        ## (i.e., time) variables, and the current break_points.  All these are going to be taken from
        ## maeve_options(), so they need either to be already stored there or passed directly to
        ## maeve::compare_groups().

        if( is.null( modeling_data_frame ) ){
          stop( 'error in maeve::compare_groups(): ',
                'If "AUC_poly" is in the metric list, then maeve_options("modeling_data_frame")\n ',
                'must be non-NULL.  This can be done either directly or by running maeve::model_study() ',
                'with "autoset_modeling_data = TRUE" in the option list.\n\n'
              )
        }
   
        glht_lmer_AUC_poly <-   
          pia( model_list[['md4_lmer_poly']],
               basis_choice = 'poly',     
               degree  = poly_degree,
               N = N_integration_grid,
               local_modeling_data_frame = modeling_data_frame,                     
               derivative = 0,
               return_list = TRUE,
               contrast = contrast,
               custom_contrast = custom_contrast,
               reference_Dunnett = reference_Dunnett,
               ##
               groupname = 'group',
                   xname = 'x',
               ##
               xmin = xmin,
               xmax = xmax,
               ##  NB: Recommended setting for 'xrange_norm_method':
               ## "slope_equivalent" <--> 'half_xrange_squared' option in pia() for "derivative = 0".
               ## "slope_equivalent" <--> 'xrange'              option in pia() for "derivative = 1".
               xrange_norm_method = switch( xrange_norm_method,
                                           'none'             = 'none',
                                           'xrange'           = 'xrange',
                                           'slope_equivalent' = 'half_xrange_squared',
                                            ### If none of the first three above are a match, stop:
                                            stop( 'error in maeve::compare_groups() call to pia(): "xrange_norm_method" must be one of ',
                                                  'c("none", "xrange", "slope_equivalent")'
                                                 )
                                          ),
               subtract_starting_value = subtract_starting_value,
               provide_warnings = TRUE
              ) # end of call 'pia( model_list[['md4_lmer_poly']], ...'

     } ## end of 'if( !is.null( model_list[['md4_lmer_poly']] ) ){...'


    } else{ ### This executes if ('AUC_poly' %in% metric) is FALSE.

      if( progress ){
          cat(paste0('Skipping AUC polynomial comparison.\n' ) )
      }

      glht_lmer_AUC_poly <- NULL

    } ## end of "if( 'AUC_poly' %in% metric ){ ...} else{...}" block





    ## (4) ITGR for a polynomial linear lmer() model <--> 'ITGR_poly'  
    if( 'ITGR_poly' %in% metric ){ 

      ## "polynomial basis linear mixed model"
      ## The ITGR of the the polynomial basis linear mixed model curve, by group.

      if( progress ){
          cat( paste0('Starting pia comparison of ', study_ID_value, ' for "ITGR_poly".\n') )
      }

      if( is.null( model_list[['md4_lmer_poly']] ) ){
        glht_lmer_ITGR_poly <- NULL
      }

      if( !is.null( model_list[['md4_lmer_poly']] ) ){
        ## This is the section where we find ITGR for fixed effects from a polynomial mixed model.

        if( is.null( modeling_data_frame ) ){
          stop( 'error in maeve::compare_groups(): ',
                'if "ITGR_poly" is in the metric list, then maeve_options("modeling_data_frame") ',
                'must be non-NULL; this can be done either directly or by running maeve::model_study() ',
                'with "autoset_modeling_data = TRUE" in the option list.\n\n'
               )
        } ## end of 'if( !is.null( model_list[['md4_lmer_poly']] ) ){...
          
   
        glht_lmer_ITGR_poly <-   
          pia( model_list[['md4_lmer_poly']],
               basis_choice = 'poly',     
               degree  = poly_degree,
               N = N_integration_grid,
               local_modeling_data_frame = modeling_data_frame,                     
               derivative = 1,
               return_list = TRUE,
               contrast = contrast,
               custom_contrast = custom_contrast,
               reference_Dunnett = reference_Dunnett,
               ##
               groupname = 'group',
                   xname = 'x',
               ##
               xmin = xmin,
               xmax = xmax,
               ##  NB: Recommended setting for 'xrange_norm_method':
               ## "slope_equivalent" <--> 'half_xrange_squared' option in maeve::leia() for "derivative = 0".
               ## "slope_equivalent" <--> 'xrange'              option in maeve::leia() for "derivative = 1".
               xrange_norm_method = switch( xrange_norm_method,
                                           'none'             = 'none',
                                           'xrange'           = 'xrange',
                                           'slope_equivalent' = 'xrange', # need 'xrange' rather than 'half_xrange_squared' for derivative == 1.
                                            ### If none of the first three above are a match, stop:
                                            stop( 'error in maeve::compare_groups() call to pia(): "xrange_norm_method" must be one of ',
                                                  'c("none", "xrange", "slope_equivalent")'
                                                 )
                                          ),
               subtract_starting_value = subtract_starting_value,
               provide_warnings = TRUE
              ) # end of call 'pia( model_list[['md4_lmer_poly']], ...'

      } ## end of 'if( !is.null( model_list[['md4_lmer_poly']] ) ){...'


    }  else{ ## This executes if ('ITGR_poly' %in% metric) is FALSE.

       if( progress ){
           cat(paste0('Skipping ITGR polynomial comparison.\n' ) )
       }

       glht_lmer_ITGR_poly <- NULL

    } # # end of "if( 'ITGR_poly' %in% metric ){ ...} else{...}" block


    
    
    ## put all the glht objects into a list.
    glht_obj_list <- list(
                         ## summary for lme4::lmer() linear fit
                         linear    = glht_lmer, 
                         ## summaries for gamm4() fit on spline basis (thin-plate splines by default)
                         ITGR      = glht_gamm4_leia_deriv1$glht_out,
                         AUC       = glht_gamm4_leia$glht_out,
                         ## summaries for lme4::lmer() fit with piecewise linear basis
                         ITGR_pwl  = glht_lmer_ITGR_pwl$glht_out,
                         AUC_pwl   = glht_lmer_AUC_pwl$glht_out,
                         ## summaries for lme4::lmer() fit with simple polynomial basis
                         ITGR_poly = glht_lmer_ITGR_poly$glht_out,
                         AUC_poly  = glht_lmer_AUC_poly$glht_out
                        )

    if( progress ){
        cat('Estimating confidence intervals for desired contrasts.\n')
    }

      
    contrast_levels <- unique(
                              c(
                                ##
                                names(coef( glht_obj_list[['linear']]    )),
                                ##
                                names(coef( glht_obj_list[['ITGR']]      )),
                                names(coef( glht_obj_list[['AUC']]       )), 
                                ##
                                names(coef( glht_obj_list[['ITGR_pwl']]  )),
                                names(coef( glht_obj_list[['AUC_pwl']]   )),
                                ##
                                names(coef( glht_obj_list[['ITGR_poly']] )),
                                names(coef( glht_obj_list[['AUC_poly']]  ))
                              )
                            )
      
    effectDF <-
      ## This was an in-between point for a bug stemming from the
      ## R 4.0.0 change to options( 'stringsAsFactors' ) == FALSE.
      ## I added changes in the underlying (and unexported) function
      ## "make_confint()" to explicitly make the 'metric' and 'contrast'
      ## columns in a data.frame there into factors rather than characters.
      ## Returning as characters caused a cascading stream of bugs that
      ## crashed the "maeve::generate_summary_table()" function when it
      ## called "maeve::compare_groups()", which in turn called
      ## "maeve:::make_confint()."
      plyr::ldply( as.list( names( glht_obj_list ) ),
                   .fun = make_confint,
                         glht_obj_list = glht_obj_list,     # 2nd argument to make_confint()
                            conf_level = conf_level,        # 3rd argument to make_confint()
                     adjustment_method = adjustment_method, # 4th argument to make_confint()
                       extended_output = extended_output    # 5th argument to make_confint()
                  ) %>%
      dplyr::mutate( contrast = factor( contrast, levels = contrast_levels ) )

    

    if( length( truncated_group_levels ) > 0 ){ # one or more groups completely truncated from modeling

      ## NB: 'model_group_levels' has only groups that were used in modeling.
      ## If no groups were entirely truncated, 'model_group_levels' will be
      ## identical to 'all_group_levels':
      model_group_levels <- levels(    modeling_data_frame[, group_name ] ) 
        all_group_levels <- levels( full_study_data_frame[ , group_name ] )
      ## The union of "model_group_levels" with "truncated_group_levels" should be 'all_group_levels'
      ## Sanity check of this fact:
      stopifnot( all( match( union( model_group_levels, truncated_group_levels ), all_group_levels ) ) )
      
      ## Check on value of reference group for when it's needed in Dunnett contrasts:
      if( is.null( reference_Dunnett ) ){
          reference_char <- model_group_levels[1]
      } else{
          reference_char <- reference_Dunnett
      }
         
      if( ! reference_char %in% model_group_levels ){
            stop_msg_string <- paste( model_group_levels, collapse = '\n' )
            stop( 'error in maeve::compare_group(): reference group value\n',
                   reference_char,
                  'not found in non-truncated groups used in modeling:\n',
                   stop_msg_string,
                  '\n\n',
                  'change parameter "reference_Dunnett" to a non-truncated group.'
                 )
      } # end of 'if( ! reference_char %in% model_group_levels ){...'
      

      truncated_group_DF <-
          expand.grid( metric   = levels( effectDF$metric ),
                       contrast = switch( contrast,
                                          ## With 'Dunnett' contrast, attach the reference group in the name.
                                          Dunnett = paste( truncated_group_levels, '-', reference_char ),
                                          ## With 'Identity', etc., just return the names as they are:
                                          truncated_group_levels # in maeve_options("truncated_group_levels")
                                         )
                      ) ## end of 'expand.grid( metric   = levels( effectDF$metric ),...'
      
      truncated_group_NA <-
          vector( "list", length = ncol( effectDF ) - 2 ) %>% # " - 2 " to exclude 'metric' and 'contrast' from the column count.
          setNames( setdiff( colnames( effectDF ), c( 'metric', 'contrast' ) ) ) %>% # all column names except c('metric', 'contrast').
          lapply( function( x, L = nrow( truncated_group_DF ) ) rep( NA, L ) ) %>% # fill in 'NA' for each {'metric', 'contrast'} pair.
          data.frame # bind the lists together into a data.frame.

      truncated_group_DF %<>% cbind( truncated_group_NA ) # affix the 'NA' values to the right of the {'metric', 'contrast'} combinations.

      effectDF %<>% rbind( truncated_group_DF ) # put these on the tail end of the "effectDF" data.frame.

      if( "Identity" == contrast ){
          effectDF %<>%
          dplyr::mutate( contrast = .data$contrast %>%
                                     forcats::fct_expand(  levels( full_study_data_frame[ , group_name ] ) ) %>%
                                     forcats::fct_relevel( levels( full_study_data_frame[ , group_name ] ) )
                         ) %>%
          dplyr::arrange( .data$metric, .data$contrast )
      } # end of 'if( "Identity" == contrast ){...'

     
      if( "Dunnett" == contrast ){
        ## With Dunnett contrasts, the contrast name will be something like, e.g., "group_03 - group_01",
        ## where in this case "group_01" is the "reference_char" value we just defined.  In that case,
        ## we make a list of all the groups with the " - group_01" part of the string taken out:
        contrast_levels_to_match <- gsub( paste( " -", reference_char ), '', levels( effectDF[ , 'contrast' ] ), fixed = TRUE )
         
        level_dist_mat <- utils::adist( contrast_levels_to_match, all_group_levels[ all_group_levels != reference_char ] ) # edit / Levenshtein distances.
         
        rownames( level_dist_mat ) <- contrast_levels_to_match
        colnames( level_dist_mat ) <- all_group_levels[ all_group_levels != reference_char ]

        ## This should order the table properly:
        Dunnett_match_order <- apply( level_dist_mat, 1, function(x) which( x == min(x) )  ) # assumes shortest edit distance is the right match.
        effectDF <- effectDF[ order( Dunnett_match_order ), ] %>% maeve::freeze_factor_levels()

      } # end of 'if( "Dunnett" == contrast ){...'
     
    } # end of 'if( length( truncated_group_levels ) > 0 ){...'



    
    ## Make figures for output.  Lots of figures.

    if( draw_figures ){

      if( progress ){
          cat( paste0( 'Drawing figures for summaries.\n' ) )
      }

      figCI <-
       ggplot2::ggplot( effectDF, ggplot2::aes( x = contrast, y = Estimate, group = metric ) ) +
       ggplot2::geom_errorbar( ggplot2::aes( ymin = lwr, ymax = upr, width = 0.1), na.rm = geom_na_rm ) +
       ggplot2::geom_point( ggplot2::aes( colour = contrast ), size = 5, na.rm = geom_na_rm ) +
       ggplot2::theme( axis.text.x = ggplot2::element_text(  size = axis_text_x_size,
                                                            angle = axis_text_x_angle,
                                                            hjust = axis_text_x_hjust,
                                                            vjust = axis_text_x_vjust
                                                           ),
                       axis.text.y = ggplot2::element_text( size = axis_text_y_size ),
                       strip.text  = ggplot2::element_text( size =  strip_text_size ), # set font size in facet panel titles
                       legend.text = ggplot2::element_text( size = legend_text_size ),
                       legend.position = legend_position_char
                      ) +
       ggplot2::facet_wrap( ~ metric, scales = facet_wrap_scales ) +
       ggplot2::ggtitle( study_ID_value )


      figCI_list <- vector('list', nlevels( effectDF$metric ) )
      names( figCI_list ) <- levels( effectDF$metric )
      for( name in levels( effectDF$metric ) ){
        figCI_list[[name]] <-
          dplyr::filter( effectDF, metric == name ) %>%
          ggplot2::ggplot( ggplot2::aes( x = contrast, y = Estimate, group = metric ) ) +
          ggplot2::geom_errorbar( ggplot2::aes( ymin = lwr, ymax = upr, width = 0.1 ), na.rm = geom_na_rm ) +
          ggplot2::geom_point( ggplot2::aes( colour = contrast ), size = 5, na.rm = geom_na_rm  ) +
          ggplot2::theme(  axis.text.x = ggplot2::element_text( size = axis_text_x_size, angle = axis_text_x_angle, hjust = 1 ),
                           axis.text.y = ggplot2::element_text( size = axis_text_y_size ),
                           strip.text  = ggplot2::element_text( size =  strip_text_size ), # set font size in facet panel titles
                           legend.text = ggplot2::element_text( size = legend_text_size ),
                       legend.position = legend_position_char
                         ) +
          ggplot2::ggtitle( paste0(study_ID_value,": ", name) )
      } ## end of 'for( name in levels( effectDF$metric ) ){...'

      fig_list <- list( figCI = figCI )

    } ### end of "if( draw_figures ){..."

    if( ! draw_figures ){
      ## Just return NULL for each figure:

      figCI <- NULL

      figCI_list <- vector('list', nlevels( effectDF$metric ) ) # makes a list of NULL values.
      names( figCI_list ) <- levels( effectDF$metric )

    } ### end of "if( ! draw_figures ){..."


    return(
        list(
             models = list( glht_obj_list = glht_obj_list ),
               data = list( effectDF = effectDF ),
            figures = list( figCI = figCI, figCI_list = figCI_list )
             )
         )

  } # end 'compare_groups <- function(...'
