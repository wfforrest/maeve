#' Fit longitudinal models to and return a list with models, data frames, and miscellaneous.
#'
#' @author Bill Forrest <forrest@@gene.com>
#'
#' @param study_data_frame a well-prepared data.frame with the study data and appropriate columns.
#' @param drop_truncated_levels logical: whether to drop from factors levels that have been truncated away in the modeling data.
#' @param return_all logical: return a whole bunch of output in a list.
#' @param record_truncated_group_levels logical: whether to record level(s) of any groups that are entirely lost during data truncation in maeve_options("truncated_group_levels").
#' @param metric                  character.      See ?maeve_options().
#' @param weight_lmer_option      character.      See ?maeve_options().
#' @param group_name              character.      See ?maeve_options().
#' @param subject_ID              character.      See ?maeve_options().
#' @param x_name                  character.      See ?maeve_options().
#' @param endpoint_name           character.      See ?maeve_options().
#' @param add_to_endpoint         numeric.        See ?maeve_options().
#' @param trans_func_char         character.      See ?maeve_options(). 
#' @param inv_func_char           character.      See ?maeve_options().
#' @param test_func_x             numeric.        See ?maeve_options().
#' @param progress                logical.        See ?maeve_options().
#' @param abbreviate_n            numeric.        See ?maeve_options().
#' @param reference_Dunnett       character.      See ?maeve_options().
#' @param restrict_x              logical.        See ?maeve_options().
#' @param overall_x_min           numeric.        See ?maeve_options().
#' @param overall_x_max           numeric.        See ?maeve_options().
#' @param min_n_in_group          numeric.        See ?maeve_options().
#' @param min_frac_in_group       numeric.        See ?maeve_options().
#' @param min_frac_in_study       numeric.        See ?maeve_options().    
#' @param number_basis_vecs       numeric.        See ?maeve_options().
#' @param min_basis_vecs          numeric.        See ?maeve_options().
#' @param max_basis_vecs          numeric.        See ?maeve_options().
#' @param autoset_full_study_data logical.        See ?maeve_options().
#' @param autoset_modeling_data   logical.        See ?maeve_options().
#' 
#' @return An R list with named output.
#'
#' @examples
#'  cat('Example for model_study() in dontrun code block.')
#'  \dontrun{
#'  data( vismodegib )
#'  vismo21 <- dplyr::filter( vismodegib, DAY_OF_STUDY <= 21 )
#'  model_list <- model_study( vismo21 )
#' }
#'
#' @author Bill Forrest \email{forrest@@gene.com}
#'
#' @references \url{www.r-project.org}
#'
#' @keywords data.frame
#' @export
#'
model_study <-
  function( study_data_frame,
            drop_truncated_levels         = FALSE,
            return_all                    = FALSE, # return additional data frames in a list of lists.
            record_truncated_group_levels = TRUE,
            ##
            metric                = maeve_options("metric"),
            weight_lmer_option    = maeve_options("weight_lmer_option"),
            ##
            group_name            = maeve_options("group_name"),
            subject_ID            = maeve_options("subject_ID"),
            x_name                = maeve_options("x_name"),
            endpoint_name         = maeve_options("endpoint_name"),
            ##
            add_to_endpoint       = maeve_options("add_to_endpoint"),
            trans_func_char       = maeve_options("trans_func_char"),
            inv_func_char         = maeve_options("inv_func_char"),
            test_func_x           = maeve_options("test_func_x"),
            ##
            progress              = maeve_options("progress"),
            abbreviate_n          = maeve_options("abbreviate_n"),
            reference_Dunnett     = maeve_options("reference_Dunnett"),
            restrict_x            = maeve_options("restrict_x"),
            overall_x_min         = maeve_options("overall_x_min"),
            overall_x_max         = maeve_options("overall_x_max"),
            ##
            ##
            min_n_in_group        = maeve_options("min_n_in_group"),
            min_frac_in_group     = maeve_options("min_frac_in_group"),
            min_frac_in_study     = maeve_options("min_frac_in_study"),
            ##
            number_basis_vecs     = maeve_options("number_basis_vecs"),
            min_basis_vecs        = maeve_options("min_basis_vecs"),
            max_basis_vecs        = maeve_options("max_basis_vecs"),
            ##
            autoset_full_study_data = maeve_options("autoset_full_study_data"),
            autoset_modeling_data   = maeve_options("autoset_modeling_data")
            ##
           ){


  metric = match.arg( metric, choices = maeve_options('metrics_supported'), several.ok = TRUE ) # how to summarize longitudinal data?    

  weight_lmer_option = match.arg( weight_lmer_option, choices = c('uniform', 'overweight_baseline') )

  ## Check the transformation function & assign it & its inverse:
  func_list  <- check_trans_func( trans_func_char, inv_func_char, test_func_x )
  trans_func <- func_list$trans_func;
    inv_func <- func_list$inv_func

  ## check that the study data.frame is "OK" -- see maeve::check_study_data_frame() for details.
  clean_DF_full_xrange <-
  maeve::check_study_data_frame(
                                 input_data_frame = study_data_frame,
                                 ###
                                 group_name    = group_name,
                                 subject_ID    = subject_ID,
                                 x_name        = x_name,
                                 endpoint_name = endpoint_name,
                                 ###
                                 study_id_name      = NULL, ### do not include this in the data
                                 studyProtocol_name = NULL, ### do not include this in the data
                                 ###
                                 progress           = FALSE,
                                 abbreviate_n       = abbreviate_n,
                                 reference_Dunnett  = reference_Dunnett,
                                 return_unique      = TRUE
                                )

  clean_DF_full_xrange %<>%
      ## Define names used internally in model formulae
      ## 
      dplyr::mutate(  'group'  = !!dplyr::sym( group_name ),
                      'ID'     = !!dplyr::sym( subject_ID ),
                      'x'      = !!dplyr::sym( x_name ),
                      'y_orig' = !!dplyr::sym( endpoint_name )
                     ) %>%
      dplyr::mutate ( y = trans_func( y_orig + add_to_endpoint ) ) %>%
      swap_columns( 'y_orig', 'y' )

  ## 20181116: if reference_Dunnett is NULL, then set it to the first level of the group_name factor:
  ##
  group_name_levels <- levels( study_data_frame[, group_name ] )
  
  if( is.null( reference_Dunnett ) ){
      reference_index   <- 1
      reference_Dunnett <- group_name_levels[reference_index] # use first level if no other guidance is given.
  } # end of ' if( is.null( reference_Dunnett ) ){...'

  if( ! is.null( reference_Dunnett ) ){
      
      reference_index <- match( reference_Dunnett, group_name_levels ) # numeric position of "reference_Dunnett" in "group_name_levels".

      if( is.na( reference_index ) ){
        ## This should catch the case in which 'reference_Dunnett' does *NOT* match one of the group_name_levels.
        stop_msg_string <- paste( group_name_levels, collapse = '\n' )  
        stop( 'error in maeve::model_study(): ',
               reference_Dunnett,
              ' not found in the group_name factor levels:\n\n',
               stop_msg_string,
              '\n'
             )
      } # end of 'if( is.na( reference_index ) ){...'

  } # end of 'if( ! is.null( reference_Dunnett ) ){...'
    

      
  if( restrict_x ){
      xrange <- get_xrange( clean_DF_full_xrange, x_name = 'x', group_name = 'group', reference_Dunnett = reference_Dunnett )
  } else{
      xrange <- range( clean_DF_full_xrange[,x_name], na.rm = TRUE )
  }

      

  ## In the next section we are incorporating data truncation via "maeve::truncate_study()"
  ## into the modeling.  The underlying issue is that we want to use only the
  ## truncated data for modeling (splines, lines, piecewise linear, naive polynomials),
  ## but we want to retain the *full* data set (including x-values that get truncated
  ## and their endpoint values) in the returned data frame used by predict_study().


  ## First step:  Get the full list of truncated results from
  ## maeve::truncate_study().
  ## Truncation is imposed by the values of the maeve_options() values in
  ## {"min_n_in_group", "min_frac_in_group", "min_frac_in_study"},
  ## while the "xrange" values inform the overall min & max.
  clean_DF_post_truncation_list <- 
     clean_DF_full_xrange %>%
     maeve::truncate_study( group_name  = group_name,
                            subject_ID  = subject_ID,
                            x_name      = x_name,
                            ##
                            ## If "restrict_x == FALSE", the next step with 'xrange' should not make any changes,
                            ## since all the x values will fall within xrange.     
                            overall_x_min = max( c( overall_x_min, min( xrange ) ) ),
                            overall_x_max = min( c( overall_x_max, max( xrange ) ) ),
                            ##
                            min_n_in_group    = min_n_in_group,    # all times in a group are truncated when less than this number of subjects are left in the group.
                            min_frac_in_group = min_frac_in_group, # all times in a group are truncated when less than this fraction of subjects are left in the group.
                            ##
                            min_frac_in_study = min_frac_in_study, # all times in a study are truncated when less than this fraction of subjects are left in the entire study.
                            #
                            drop_singletons        = TRUE, # drop *all* observations from a group if, post-truncation, it has only 1 observation per remaining subject.
                            drop_singleton_IDs     = FALSE, # do NOT drop an individual subject if it has 'singleton_number' of values, but is in a group with other subjects that have more than 'singleton_number' observations per subject.
                            singleton_number       = 1,     # If all IDs in a group have "singleton_number" or few observations and drop_singletons == TRUE, the group will get dropped.            
                            truncation_return_type = 'list'
                           )

  model_x_value <- clean_DF_post_truncation_list$include_x # just a more descriptive name for the external data.frame.    

  clean_DF_full_xrange %<>% cbind( model_x_value )
  clean_DF_restricted <- clean_DF_full_xrange[ model_x_value, ]

  if( drop_truncated_levels ){
      clean_DF_restricted %<>% droplevels # drop levels of factors that have been completely truncated.
  }

  if( record_truncated_group_levels ){
          
      truncated_levels <- setdiff( levels( clean_DF_full_xrange[,group_name] ),
                                   levels( clean_DF_restricted[, group_name] )
                                  )
      
      if( length( truncated_levels ) > 0 ){
         maeve_options("truncated_group_levels" = truncated_levels )
      } # end of 'if( length( truncated_levels ) > 0 ){...'
      
  } # end of 'if( record_truncated_group_levels ){...'


      
  ## Fit the linear and / or spline and / or piecewise linear models with 2+ groups:
  if( nlevels( clean_DF_restricted[,'group'] ) >= 2 ){
      model_list = models_in_list( clean_DF_restricted, metric, study_ID_value = 'current study', progress,
                                   number_basis_vecs, min_basis_vecs, max_basis_vecs, weight_lmer_option
                                   )
  } # end of 'if( nlevels( clean_DF_restricted[,'group'] ) >= 2 ){...'

      
  ## Fit the linear and / or spline and / or piecewise linear models, but with only 1 group:
  if( nlevels( clean_DF_restricted[,'group'] ) == 1 ){
      model_list = models_in_list( clean_DF_restricted, metric, study_ID_value = 'current study', progress,
                                   number_basis_vecs, min_basis_vecs, max_basis_vecs, weight_lmer_option,
                                   lme4_formula  = formula( 'y ~ 1 + x + ( 1 + x | ID )' ), # including "group:x" would over-parameterize the model.
                                   gam_formula_char_list = list( prefix = 'y ~ s( x, bs = "tp", k =', suffix = ')' ) # including "group" would over-parameterize the model.
                                   )
  } # end of 'if( nlevels( clean_DF_restricted[,'group'] ) == 1 ){...'


  if( autoset_full_study_data ){ # 'autoset_full_study_data' is a logical parameter in maeve_options().
      maeve_options( 'full_study_data_frame' = clean_DF_full_xrange ) # 'full_study_data_frame' is a parameter in maeve_options().
  }
    
  if( autoset_modeling_data ){ # 'autoset_modeling_data' is a logical parameter in maeve_options().
      maeve_options( 'modeling_data_frame' = clean_DF_restricted ) # 'modeling_data_frame' is a parameter in maeve_options().
  }

    
  model_study_return_list <-
    list(
          models = model_list, ## three components, but one or two may be NULL if their respective metrics were not requested.
            data = list( clean_DF_full_xrange = clean_DF_full_xrange, ## cleaned data frame across all observations for each group.
                         clean_DF_restricted  = clean_DF_restricted,  ## data actually used in fitting -- should just be clean data restricted to xrange of baseline group.
                             study_data_frame = study_data_frame      ## original data frame provided.
                       ),
          ## 'extra' below is a miscellaneous list of parameters and small derived values as used inside this function.
          ## They are recorded here to make it easy to track them and pass them as needed to other
          ## functions for, e.g., prediction and plotting.
          extra = list(
                          metric = metric,
                          xrange = xrange,
               reference_Dunnett = reference_Dunnett,
                             ##
                      group_name = group_name,
                      subject_ID = subject_ID,
                          x_name = x_name,
                   endpoint_name = endpoint_name,
                            ##
                       func_list = func_list,
                 trans_func_char = trans_func_char,
                   inv_func_char = inv_func_char
                        )
        
         )

  if( return_all ){
      return( model_study_return_list  )
  } else{ # alternative to 'if( return_all ){...'
      return( model_list )
  }

      
} ## end of function definition for maeve::model_study().

