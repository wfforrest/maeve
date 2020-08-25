#' Create a ggplot2 object with root-mean-square prediction error from the conditional residuals for each subject in a study.
#'
#' @author Bill Forrest <forrest@@gene.com>
#'
#' @param rmse data frame with factors for group and ID, and numerics for RMSE-by-ID from linear fit and spline fit.  These four columns *MUST* be names {"group", "ID", "RMSE_lin_resid_ID", "RMSE_gam_resid_ID" }, respectively.  Typically, this data.frame is the data.frame called 'RMSE_by_ID' returned from maeve::predict_study() by setting "return_list = TRUE" in the predict_study() call.
#' @param facet_by character vector to determine whether to facet by group_name (default) or fit method.
#' @param exclude_NA logical whether to drop missing RMSE values.  Most often, this is because one of the two methods ('linear' or 'spline') simply was not fit.
#' @param ncol_value         numeric.  See ?maeve_options().
#' @param nrow_value         numeric.  See ?maeve_options().
#' @param geom_na_rm         logical.  See ?maeve_options().
#' @param geom_point_size    numeric.  See ?maeve_options().
#' @param alpha_value        numeric.  See ?maeve_options().
#' @param axis_text_x_size   numeric.  See ?maeve_options().
#' @param axis_text_x_angle  numeric.  See ?maeve_options().
#' @param axis_text_x_hjust  numeric.  See ?maeve_options().
#' @param axis_text_x_vjust  numeric.  See ?maeve_options().
#' @param axis_text_y_size   numeric.  See ?maeve_options().
#' @param strip_text_size    numeric.  See ?maeve_options().
#' @param title_label        numeric.  See ?maeve_options().
#'
#' @return A ggplot2 object
#'
#' @examples
#'  cat('Example for draw_noise() in dontrun block.')
#'  \dontrun{
#'  data( vismodegib )
#'  vismo21 <- dplyr::filter( vismodegib, DAY_OF_STUDY <= 21 )
#'  pred_list = vismo21 %>% model_study() %>% predict_study( return_list = TRUE )
#'  rmse = pred_list[[ 'RMSE_by_ID' ]]
#'  figure_of_noise <- draw_noise( rmse )
#' }
#' 
#' @author Bill Forrest \email{forrest@@gene.com}
#'
#' @references \url{www.r-project.org}
#'
#' @keywords ggplot2
#' @export
#'
draw_noise <-
  function( rmse, ### Usually pred_list[['RMSE_by_ID']] when pred_list = maeve::predict_study( ..., return_list = TRUE ).
            facet_by = c('group_name','fit'),
            exclude_NA = FALSE,
            ##
            ncol_value         =  maeve_options("ncol_value"),
            nrow_value         =  maeve_options("nrow_value"),
            geom_na_rm         =  maeve_options("geom_na_rm"),
            geom_point_size    =  maeve_options("geom_point_size"),
            alpha_value        =  maeve_options("alpha_value"),
            axis_text_x_size   =  maeve_options("axis_text_x_size"),
            axis_text_x_angle  =  maeve_options("axis_text_x_angle"),
            axis_text_x_hjust  =  maeve_options("axis_text_x_hjust"),
            axis_text_x_vjust  =  maeve_options("axis_text_x_vjust"),
            axis_text_y_size   =  maeve_options("axis_text_y_size"),
            strip_text_size    =  maeve_options("strip_text_size"),
            title_label        =  maeve_options("title_label")
           ){

   facet_by = match.arg( facet_by )

   ## 'rmse' should be the 'RMSE_by_ID' data.frame returned from
   ## "maeve::predict_study()" when "return_list = TRUE".
   ##
   ## cursory check that we got the right thing:
   stopifnot( is.data.frame( rmse ) &&
              all( c( "group", "ID", "RMSE_lin_resid_ID", "RMSE_gam_resid_ID", "RMSE_pwl_resid_ID", "RMSE_poly_resid_ID" ) %in% colnames( rmse ) )
             )

   rmse %<>%
   dplyr::select( group, ID, RMSE_lin_resid_ID, RMSE_gam_resid_ID, RMSE_pwl_resid_ID, RMSE_poly_resid_ID ) %>%
   dplyr::rename(    linear = RMSE_lin_resid_ID,
                     spline = RMSE_gam_resid_ID,
                  piecewise = RMSE_pwl_resid_ID,
                       poly = RMSE_poly_resid_ID
                 ) %>%
   tidyr::gather( key = 'fit', value = 'RMSE', linear, spline, piecewise, poly  ) %>%
   dplyr::mutate( fit = factor( fit, levels = c('linear', 'spline', 'piecewise', 'poly') ) ) %>%
   dplyr::arrange( group, ID, fit ) %>%
   maeve::freeze_factor_levels() %>%
   dplyr::mutate( color_order = maeve::splice_color_order( group, ID ) )

   if( exclude_NA ){ rmse %<>% dplyr::filter( !is.na( RMSE ) ) %>% droplevels() }    

   if( facet_by == 'group_name' ){ facet_variable <- 'group'; x_variable <-   'fit' }
   if( facet_by ==        'fit' ){ facet_variable <-   'fit'; x_variable <- 'group' }
    
   fig <-
    ggplot2::ggplot( data = rmse, ggplot2::aes_string( x = eval( x_variable ), y = 'RMSE', colour = 'color_order' ) ) +
    ggplot2::facet_wrap( stats::reformulate( facet_variable ), ncol = ncol_value, nrow = nrow_value ) +
    ggplot2::geom_boxplot( colour = 'gray50', outlier.size = 0, na.rm = geom_na_rm ) +
    ggplot2::geom_point( size = geom_point_size, alpha = alpha_value, position = ggplot2::position_jitter( width = .10 ), na.rm = geom_na_rm ) +
    ggplot2::theme_bw( ) +
    ggplot2::theme( legend.position = 'none',
                    axis.text.x = ggplot2::element_text(  size  = axis_text_x_size,
                                                          angle = axis_text_x_angle,
                                                          hjust = axis_text_x_hjust,
                                                          vjust = axis_text_x_vjust
                                                       ),
                    axis.text.y = ggplot2::element_text( size = axis_text_y_size ),
                    strip.text = ggplot2::element_text( size = strip_text_size )
                  ) +
    ggplot2::labs( title = title_label )

  return( fig )    

  }
