###
### Set options global to the package namespace
MAEVE_OPTIONS <-
  settings::options_manager(
            group_name            = 'group_name',
            subject_ID            = 'animalID',
            trace_ID              = 'animalID', ### Used for grid_by_trace() when there are, e.g., multiple longitudinal imaging traces per subject.
            x_name                = 'DAY_OF_STUDY',
            endpoint_name         = 'TUMOR_VOLUME',
            ##
            EOS_CR_minval         = 0,   # an "end-of-study complete response" (EOS_CR) is counted if a subject's last observed value is at or below this value.
            PR_threshold          = 0.5, # a "partial response" (PR) is counted if a subject has 1+ values at or below this fraction of its baseline, and is *NOT* an end-of-study complete response (EOS_CR).
            summary_first_day     = NULL, # if not NULL, specifies the first day used for calculating certain summary statistics (PR, EOS_CR, TTP, Effect Duration)
            ##
            full_study_data_frame   = NULL,  # data.frame: If out <- model_study(...);, modeling_data_frame = out$data$clean_DF_full_xrange.
            modeling_data_frame     = NULL,  # data.frame: If out <- model_study(...);, modeling_data_frame = out$data$clean_DF_restricted.
            autoset_full_study_data = TRUE, # automatically set modeling_data_frame when model_study() is called?
            autoset_modeling_data   = TRUE, # automatically set modeling_data_frame when model_study() is called?
            truncate_fit            = FALSE, # truncate predicted values to the range of observed responses.
            ##
            truncation_return_type = 'data.frame', # what type of data structure to return from "truncate_study()"?
            truncated_group_levels = character(0), # character vector with all group levels that are entirely excluded during truncation step within model_study().
            min_n_in_group         = 0, # all times in a group are truncated when less than this number of subjects are left in the group.
            min_frac_in_group      = 0, # all times in a group are truncated when less than this fraction of subjects are left in the group.
            min_frac_in_study      = 0, # all times in a study are truncated when less than this fraction of subjects are left in the entire study.
            overall_x_min          = -Inf, # all times in a study are truncated below this value.
            overall_x_max          =  Inf, # all times in a study are truncated above this value.
            ##
            abbreviate_n          = 1e6,  # Allow no more than this number of characters in a group name.
            reference_Dunnett     = NULL, # character string with group level to serve as designated reference in many-to-one contrasts.  If NULL, the first group level should get used by the local function.
                                          # If it's a vector, the 1st element is used for Dunnett's contrast          
            add_to_endpoint       = 1,    # constant added to response
            ##
            restrict_x            = FALSE, # Include only x-values in baseline group range of x-values.
            ##
            number_basis_vecs     = -1, # "-1" will invoke the thin-plate default spline number.
                                        # number_basis_vecs can also usually be set to a reasonable positive integer (e.g., "7"?),
                                        # but must be smaller than the distinct number of 'x' values.
            min_basis_vecs        =  3,
            max_basis_vecs        = 15,            
            ##
            trans_func_char       = "log",
              inv_func_char       = "exp",
                test_func_x       = 1:10,
            ##
            weight_lmer_option    = "uniform", # "uniform" weighting across time points
            ##
            metric                = c('linear', 'ITGR', 'AUC'),
            metrics_supported     = c('linear', 'ITGR', 'AUC', 'ITGR_pwl', 'AUC_pwl', 'ITGR_poly', 'AUC_poly'),
            N_integration_grid    = 25, # number of equally-spaced points to use in Simpson's Rule numerical integration.
            contrast              = 'Identity',
            xrange_norm_method    = 'slope_equivalent',
            progress              =  FALSE,
            ##
            number_break_points   = 3,    # used only when piecewise linear regression is requested (e.g., metric = 'AUC_pwl')
            break_points          = NULL, # used only when piecewise linear regression is requested.
            adjust_break_points   = FALSE, # if TRUE, explicitly specified break points are adjusted to the actual time range.
            ##
            poly_degree           = 2,    # degree of simply polynomial (NOT the spline) fit.  Typically used for 3 time points.
            poly_object           = NULL, # object returned from stats::poly() call in maeve:::make_basissis_matrix().  Typically used for 3 time points.
            ##
            mismatch_action       = 'stop', # what to do if a function is passed an optional argument not found in maeve::options()?
            ##
            ## prediction options:
            ##
            x_pred_type          = c( 'observed', 'grid', 'union_observed_and_grid', 'custom', 'union_observed_and_custom', 'union_custom_and_grid' )[1],
            x_pred_vec           = NULL,
            x_pred_spacing       = 1,
            x_pred_interior_grid = TRUE,
            include_newdata_ID   = TRUE,
            ## ggplot2 options:
            title_label           = '',
            y_label               = c('log( tumor volume + 1 )', 'response')[1],
            x_label               = c('day', 'time on study')[1],            
            legend_position_char  = 'none', # passed to ggplot2::theme()
            axis_text_x_angle     =  0,
            axis_text_x_size      =  9,
            axis_text_x_hjust     =  0.5,
            axis_text_x_vjust     =  0.5,
            axis_text_y_size      =  9,
            legend_text_size      =  9,
            strip_text_size       =  9,
            ##
            linear_predictor      = 'pred_lin',
            linear_color          = 'red',
            linear_lwd            = 1.5,
            ##
            spline_predictor      = 'pred_gam',
            spline_color          = 'black',
            spline_lwd            = 1.25,
            ##
            piecewise_predictor   = 'pred_pwl',
            piecewise_color       = 'blue',
            piecewise_lwd         = 1.75,
            ##
            poly_predictor        = 'pred_poly',
            poly_color            = 'green',
            poly_lwd              = 2.00,
            ##
            geom_na_rm            =  TRUE, ## silently remove NA values in the geom_point() & geom_line() calls?
            geom_point_size       =  1,    ## "geom_point( size = ... )"
            nrow_value            =  NULL, # passed to ggplot2::facet_wrap(...) below.
            ncol_value            =  NULL, # passed to ggplot2::facet_wrap(...) below.                         
            alpha_value           =  1, # passed to ggplot2::facet_wrap(...) below.                         
            ##
          .allowed = list(
           autoset_full_study_data = settings::inlist( FALSE, TRUE ), # whether to auto-set maeve_options('full_study_data_frame') from within maeve::model_study()?
             autoset_modeling_data = settings::inlist( FALSE, TRUE ), # whether to auto-set maeve_options('modeling_data_frame') from within maeve::model_study()?
                      truncate_fit = settings::inlist( FALSE, TRUE ), # whether to truncate predicted values to the range of observed responses within maeve:predict_study()?
            truncation_return_type = settings::inlist( 'data.frame', 'list', 'logical' ), # what data type to return from truncate_study()?
                    min_n_in_group = settings::inrange( 0, 1e9 ), # all times in a group are truncated when less than this number of subjects are left in the group.
                 min_frac_in_group = settings::inrange( 0, 1 ),   # all times in a group are truncated when less than this fraction of subjects are left in the group.
                 min_frac_in_study = settings::inrange( 0, 1 ),   # all times in a study are truncated when less than this fraction of subjects are left in the entire study.
                     overall_x_min = settings::inrange( -Inf, Inf ), # data with x-values below this in a study are truncated.
                     overall_x_max = settings::inrange( -Inf, Inf ), # data with x-values above this in a study are truncated.
                weight_lmer_option = settings::inlist( 'uniform', 'overweight_baseline' ),
                   mismatch_action = settings::inlist( 'none', 'warn', 'stop' ), # what to do when a parameter passed does not match a name in maeve_options()?
                          contrast = settings::inlist( 'Identity', 'Dunnett', 'Tukey', 'Sequen', 'custom' ),
                xrange_norm_method = settings::inlist( 'slope_equivalent', 'xrange', 'none' ),
               number_break_points = settings::inrange( 3, 10000 ),
                N_integration_grid = settings::inrange( 3, 10000 ), # need minimum of 3 points for Simpson's Rule.
                       x_pred_type = settings::inlist( 'observed', 'grid', 'union_observed_and_grid', 'custom', 'union_observed_and_custom', 'union_custom_and_grid' ),
                    x_pred_spacing = settings::inrange( 1e-8, 1e9 ),
              legend_position_char = settings::inlist( 'none','right','left','bottom' ),            
                 axis_text_x_angle = settings::inrange( -360, 360 ),
                 axis_text_x_size  = settings::inrange( 0, 10000 ),
                 axis_text_x_hjust = settings::inrange( 0, 1 ),
                 axis_text_x_vjust = settings::inrange( 0, 1 ),
                 axis_text_y_size  = settings::inrange( 0, 10000 ),
                  legend_text_size = settings::inrange( 0, 10000 ),
                   strip_text_size = settings::inrange( 0, 10000 ),
                       alpha_value = settings::inrange( 0, 1 )

###### 
###### metric = settings::inlist( 'linear', 'ITGR', 'AUC', 'ITGR_pwl', 'AUC_pwl', 'ITGR_poly', 'AUC_poly' ) # what are supported summary metrics?
######
###### NB: We do NOT check for the values of 'metric' because the "settings::inlist()" function appears not to support checks
######     for parameters with 2+ default values.  Our default for 'metric' is c('linear', 'AUC', 'ITGR'), which seems to cause an error.
######            
             ) # end of '.allowed = list(...'

 ) ### end of 'MAEVE_OPTIONS <- settings::options_manager(...'



 

# User function that gets exported:

#' Set or get options for maeve
#'
#' @param ... Option names to retrieve option values or \code{[key]=[value]} pairs to set options.
#'
#' @section Supported options:
#' The following options are supported
#' \itemize{
#'  \item{\code{group_name}}              {(\code{character};              'group_name') The column name for the group name factor}
#'  \item{\code{subject_ID}}              {(\code{character};                'animalID') The column name for the subject name factor.}
#'  \item{\code{trace_ID}}                {(\code{character};                'animalID') The column name for the trace identifier name factor for maeve::grid_by_trace().}
#'  \item{\code{x_name}}                  {(\code{character};            'DAY_OF_STUDY') The column name for the x-axis / time field }
#'  \item{\code{endpoint_name}}           {(\code{character};            'TUMOR_VOLUME') The column name for the y-axis / response field }
#'  \item{\code{EOS_CR_minval}}           {(\code{numeric};                           0) numeric threshold to count an end-of-study complete response if a subject_ID has its last endpoint_name value at or below this value. }
#'  \item{\code{PR_threshold}}            {(\code{numeric};                         0.5) numeric threshold to count a partial response if a subject_ID has any observations at or below this fraction of its baseline endpoint_name value and is NOT an end-of-study complete response (EOS_CR). }
#'  \item{\code{full_study_data_frame}}   {(\code{data.frame};                     NULL) data.frame with untruncated study data.  Typically returned from (model_study(...))[['data']][['clean_DF_full_study']] }
#'  \item{\code{modeling_data_frame}}     {(\code{data.frame};                     NULL) data.frame with internal names used for predictions.  Typically returned from (model_study(...))[['data']][['clean_DF_restricted']] }

#'  \item{\code{autoset_full_study_data}} {(\code{logical};                        TRUE) Whether to automatically assign (model_study(...))[['data']][['clean_DF_full_study']] to maeve_options('full_study_data_frame' = full_study_data_frame) }

#'  \item{\code{autoset_modeling_data}}   {(\code{logical};                        TRUE) Whether to automatically assign (model_study(...))[['data']][['clean_DF_restricted']] to maeve_options('modeling_data_frame' = modeling_data_frame) }
#'  \item{\code{truncate_fit}}            {(\code{logical};                       FALSE) Whether to truncate predicted values to the range of observed responses within maeve:predict_study()? }
#'  \item{\code{truncation_return_type}}  {(\code{character};              'data.frame') What data type to return from "truncate_study()"? }
#'  \item{\code{truncated_group_levels}}  {(\code{character};              character(0)) Which group levels are entirely lost when "truncate_study()" is run within model_study()? }
#'  \item{\code{min_n_in_group}}          {(\code{numeric};                         0) all times in a group are truncated when less than this number   of subjects are left in the group. }
#'  \item{\code{min_frac_in_group}}       {(\code{numeric};                         0) all times in a group are truncated when less than this fraction of subjects are left in the group. }
#'  \item{\code{min_frac_in_study}}       {(\code{numeric};                         0) all times in a study are truncated when less than this fraction of subjects are left in the entire study. }
#'  \item{\code{overall_x_min}}           {(\code{numeric};                      -Inf) data with x-axis values below this number are truncated. }
#'  \item{\code{overall_x_max}}           {(\code{numeric};                       Inf) data with x-axis values above this number are truncated. }
#'  \item{\code{abbreviate_n}}            {(\code{numeric};                         1e6) The maximum number of characters in a group name }
#'  \item{\code{reference_Dunnett}}       {(\code{character};                      NULL) Character string with the group factor level to use as Dunnett reference.  If NULL, the first level should be used.}
#'  \item{\code{add_to_endpoint}}         {(\code{numeric};                           1) The offset value added to the endpoint before transformation }
#'  \item{\code{restrict_x}}              {(\code{logical};                       FALSE) Restrict to the x-values found in the specified reference group }
#'  \item{\code{number_basis_vecs}}       {(\code{numeric};                          -1) Default number of basis_vecs to try in fitting a spline. }
#'  \item{\code{min_basis_vecs}}          {(\code{numeric};                           3) Minimum number of basis_vecs to try in fitting a spline. }
#'  \item{\code{max_basis_vecs}}          {(\code{numeric};                          15) Maximum number of basis_vecs to try in fitting a spline. } 
#'  \item{\code{trans_func_char}}         {(\code{character};                     'log') Function for transformation of (endpoint_name + add_to_endpoint) }
#'  \item{\code{inv_func_char}}           {(\code{character};                     'exp') Inverse function for transformation of (endpoint_name + add_to_endpoint) }
#'  \item{\code{test_func_x}}             {(\code{numeric};                       1:10 ) Values with which to test that trans_func and inv_func are inverse functions. }
#'  \item{\code{weight_lmer_option}}      {(\code{character};                'uniform' ) Select how x-values should be weighted in lme4 linear mixed model regression. }
#'  \item{\code{metric}}                  {(\code{character}; c('linear','ITGR','AUC') ) Summarization method(s) for longitudinal response function. }
#'  \item{\code{metrics_supported}}       {(\code{character}; c('linear','ITGR','AUC', 'ITGR_pwl', 'AUC_pwl', 'ITGR_poly', 'AUC_poly') ) Supported summarization methods for longitudinal response function. }
#'  \item{\code{N_integration_grid}}      {(\code{numeric};                          25) Number of equally-spaced points to use in the Simpson Rule numerical integration.}
#'  \item{\code{contrast}}                {(\code{character};                'Identity') Contrast for group summary comparisons. }
#'  \item{\code{xrange_norm_method}}      {(\code{character};        'slope_equivalent') Method for normalization of spline summary statistics. }
#'  \item{\code{progress}}                {(\code{logical};                       FALSE) Print out various progress messages from the functions. }
#'  \item{\code{number_break_points}}     {(\code{numeric};                           3) Number of break points (including 2 end points) in piecewise linear regression when metric = 'AUC_pwl'. Typically used for 3 time points.}
#'  \item{\code{break_points}}            {(\code{numeric};                        NULL) Break points (including 2 end points) in piecewise linear regression when metric = 'AUC_pwl'. Typically used for 3 time points.}
#'  \item{\code{poly_degree}}             {(\code{numeric};                           2) Degree of simply polynomial (NOT the spline) fit, when metric = 'AUC_poly'. Typically used for 3 time points.}
#'  \item{\code{poly_object}}             {(\code{poly,matrix};                    NULL) Object returned from stats::poly() call in maeve:::make_basissis_matrix() when metric = 'AUC_poly'. Typically used for 3 time points.}
#'  \item{\code{mismatch_action}}         {(\code{character};                    'stop') Action when a function is passed arguments that do not match either the function or an element of maeve_options(). }
#'  \item{\code{x_pred_type}}             {(\code{character};                'observed') One of c( 'observed', 'grid', 'union_observed_and_grid', 'custom' ) determining options for what the x-axis values for predictions should be.  If "custom", then the vector 'x_pred_vec' will be used.  Otherwise, 'x_pred_vec' is ignored. }
#'  \item{\code{x_pred_vec}}              {(\code{numeric};                        NULL) numeric vector of values to use for x-axis prediction points when x_pred_type = 'custom'. }
#'  \item{\code{x_pred_spacing}}          {(\code{numeric};                           1) distance between x-values in a prediction grid. }
#'  \item{\code{x_pred_interior_grid}}    {(\code{logical};                        TRUE) whether to truncate the grid endpoints to be the most widely spaced integers within the range. }
#'  \item{\code{include_newdata_ID}}      {(\code{logical};                        TRUE) whether to include subject_ID values in the data.frame. }
#'  \item{\code{title_label    }}         {(\code{character};                        '') ggplot2::labs( title = title_label ) }
#'  \item{\code{y_label}}                 {(\code{character}; 'log( tumor volume + 1 )') ggplot2::labs(     y =     y_label ) }
#'  \item{\code{x_label}}                 {(\code{character};                    'day' ) ggplot2::labs(     x =     x_label ) }
#'  \item{\code{legend_position_char}}    {(\code{character};                    'none') ggplot2::theme( legend.position = legend_position_char ) }
#'  \item{\code{axis_text_x_angle}}       {(\code{numeric};                           0) ggplot2::theme( axis.text.x = element_text( angle = axis_text_x_angle ) ) }
#'  \item{\code{axis_text_x_size}}        {(\code{numeric};                           9) ggplot2::theme( axis.text.x = element_text( size = axis_text_x_size ) ) }
#'  \item{\code{axis_text_x_hjust}}       {(\code{numeric};                         0.5) ggplot2::theme( axis.text.x = element_text( hjust = axis_text_x_hjust ) ) }
#'  \item{\code{axis_text_x_vjust}}       {(\code{numeric};                         0.5) ggplot2::theme( axis.text.x = element_text( vjust = axis_text_x_vjust ) ) }
#'  \item{\code{axis_text_y_size}}        {(\code{numeric};                           9) ggplot2::theme( axis.text.y = element_text( size = axis_text_y_size ) ) }
#'  \item{\code{legend_text_size}}        {(\code{numeric};                           9) ggplot2::theme( legend.text = element_text( size = legend_text_size ) ) }
#'  \item{\code{strip_text_size}}         {(\code{numeric};                           9) ggplot2::theme(  strip.text = element_text( size = strip_text_size ) ) }
#'  \item{\code{linear_predictor}}        {(\code{character};                'pred_lin') column name of linear predictor in data.frame of predicted output. }
#'  \item{\code{linear_color}}            {(\code{character};                     'red') color of linear predictor. }
#'  \item{\code{linear_lwd}}              {(\code{numeric};                        1.50) line width parameter for linear predictor, passed to geom_line(...) }
#'  \item{\code{spline_predictor}}        {(\code{character};                'pred_gam') column name of spline predictor in data.frame of predicted output. }
#'  \item{\code{spline_color}}            {(\code{character};                   'black') color of spline predictor. }
#'  \item{\code{spline_lwd}}              {(\code{numeric};                        1.25) line width parameter for spline predictor, passed to geom_line(...) }
#'  \item{\code{piecewise_predictor}}     {(\code{character};                'pred_pwl') column name of piecewise linear predictor in data.frame of predicted output. }
#'  \item{\code{piecewise_color}}         {(\code{character};                    'blue') color of piecewise linear predictor. }
#'  \item{\code{piecewise_lwd}}           {(\code{numeric};                        1.75) line width parameter for piecewise linear predictor, passed to geom_line(...) }
#'  \item{\code{poly_predictor}}          {(\code{character};               'pred_poly') column name of polynomial predictor in data.frame of predicted output. }
#'  \item{\code{poly_color}}              {(\code{character};                   'green') color of polynomial predictor. }
#'  \item{\code{poly_lwd}}                {(\code{numeric};                        2.00) line width parameter for polynomial predictor, passed to geom_line(...) }
#'  \item{\code{geom_na_rm}}              {(\code{logical};                       TRUE ) silently remove NA values in the geom_point() & geom_line() calls? }
#'  \item{\code{geom_point_size}}         {(\code{numeric};                          2 ) ggplot2::geom_point( size = geom_point_size ) }
#'  \item{\code{nrow_value}}              {(\code{numeric};                       NULL ) passed to ggplot2::facet_wrap(...) }
#'  \item{\code{ncol_value}}              {(\code{numeric};                       NULL ) passed to ggplot2::facet_wrap(...) }
#' }
#'
#' @examples
#'  cat('maeve::maeve_options() allows reset of parameters called in functions within maeve.')
#'  \dontrun{ maeve_options( 'spline_color' );
#'            maeve_options( 'spline_color' = 'orange');
#'            maeve_options( 'spline_color' );
#'            maeve_reset()
#'            maeve_options( 'spline_color' );
#'           }
#'
#'
#' @return a function as from settings::options_manager() 
#'
#' @author Bill Forrest \email{forrest@@gene.com}
#'
#' @keywords function
#'
#' @export
maeve_options <- function(...){
# protect against the use of reserved words.
# settings::stop_if_reserved(...) 
  MAEVE_OPTIONS(...)
}
### 20180430: Including the "settings::stop_if_reserved(...) " above line causes an error.
### Not *exactly* sure why, but skipping the optional error check, since I can't see any
### reserved word here (there are only two).


#' Reset global options for maeve
#'
#' @examples
#'  cat('maeve::maeve_options() allows reset of parameters called in functions within maeve.')
#'  \dontrun{ maeve_options( 'spline_color' );
#'            maeve_options( 'spline_color' = 'orange');
#'            maeve_options( 'spline_color' );
#'            maeve_reset()
#'            maeve_options( 'spline_color' );
#'           }
#'
#' @return resets the MAEVE_OPTIONS() internal function to its package defaults.
#'
#' @author Bill Forrest \email{forrest@@gene.com}
#'
#' @export
maeve_reset <- function(){
  settings::reset( MAEVE_OPTIONS )
}



#' Return default valus of global options for maeve
#'
#' The function accepts either comma-spaced option names as character strings,
#' a concatenated character vector of such names, or integer positions in the
#' option list. It returns the default values corresponding to the input, without
#' resetting the existing option values.
#'
#' @param ... either comma-spaced option names as character strings, a concatenated character vector of such character strings, or integer positions in the option list.
#'
#' @examples
#'  cat( paste( 'maeve::maeve_defaults() returns default parameter',
#'              'values without resetting the current options.'
#'             ) 
#'     )
#'  \dontrun{ maeve_defaults( 'spline_color' );
#'            maeve_defaults( 'spline_color', 'alpha_value' );
#'            maeve_defaults( c( 'spline_color', 'alpha_value' ) );
#'            maeve_defaults( 1:3 ); # return defaults for first 3 components of maeve_options().
#'            ##
#'            maeve_options( alpha_value = 0.5 ) # change something.
#'            maeve_defaults( 'alpha_value' ); #
#'            ## Observe that the newly set value persists until maeve_reset() is called.
#'            maeve_options( 'alpha_value' )
#'           }
#'
#' @return A list with maeve package defaults values for named parameters, but without resetting any option values.  If no names are specified, then the entire list of defaults is returned.
#'
#' @author Bill Forrest \email{forrest@@gene.com}
#'
#' @export
maeve_defaults <- function(...){

   requested_default_names <- c( ... ) # collapse names to a vector, if they are not already.

   if( length( requested_default_names ) >= 1 ){
           ### Return only the option values requested.
           return( settings::defaults( maeve_options )[ requested_default_names ] )
           ###
   }
   
   if( length( requested_default_names ) == 0 ){
           ### No names passed, so just return all the defaults:
           return( settings::defaults( maeve_options ) )
           ###
   }

   stop( 'error in maeve::maeve_defaults().' ) # should not ever get to this.

} # end of 'maeve_defaults <- function(...){...'   
