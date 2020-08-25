#' Create a ggplot2 object with model overlay fits from a maeve study.
#'
#' @author Bill Forrest <forrest@@gene.com>
#'
#' @param data_frame data frame with a factor for group, numerics for time on study and response, and fitted numeric response curves.
#' @param fit character vector with names of fitted values to include.
#' @param group_name           character.   See ?maeve_options().
#' @param x_name               character.   See ?maeve_options().
#' @param endpoint_name        character.   See ?maeve_options().
#' @param linear_predictor     character.   See ?maeve_options().
#' @param spline_predictor     character.   See ?maeve_options().
#' @param piecewise_predictor  character.   See ?maeve_options().
#' @param poly_predictor       character.   See ?maeve_options().
#' @param axis_text_x_size     numeric.     See ?maeve_options().
#' @param axis_text_x_angle    numeric.     See ?maeve_options().
#' @param axis_text_y_size     numeric.     See ?maeve_options().
#' @param strip_text_size      numeric.     See ?maeve_options().
#' @param legend_text_size     numeric.     See ?maeve_options().
#' @param legend_position_char character.   See ?maeve_options().
#' @param title_label          character.   See ?maeve_options().
#' @param x_label              character.   See ?maeve_options().
#' @param y_label              character.   See ?maeve_options().
#' @param geom_na_rm           logical.     See ?maeve_options().
#'
#' @return A ggplot2 object
#'
#' @examples
#'  cat("Currently no running example for draw_overlay(). See next block.")
#'  \dontrun{
#'  data( vismodegib )
#'  vismo21 <- dplyr::filter( vismodegib, DAY_OF_STUDY <= 21 )
#'  pred_data_frame = vismo21 %>% model_study() %>% predict_study()
#'  figure_overlay <- draw_overlay( pred_data_frame )
#' }
#'
#' @author Bill Forrest \email{forrest@@gene.com}
#'
#' @references \url{www.r-project.org}
#'
#' @keywords data.frame
#' @export
#'
draw_overlay <-
  function( data_frame,
            fit = c( 'spline', 'linear', 'piecewise', 'poly', 'none' ), # what fit to add to picture?
            ##
            group_name           = maeve_options("group_name"),
            x_name               = maeve_options("x_name"),
            endpoint_name        = maeve_options("endpoint_name"),
            ##
            linear_predictor     = maeve_options("linear_predictor"),
            spline_predictor     = maeve_options("spline_predictor"),
            piecewise_predictor  = maeve_options("piecewise_predictor"),
            poly_predictor       = maeve_options("poly_predictor"),
            ##
            axis_text_x_size     = maeve_options("axis_text_x_size"),
            axis_text_x_angle    = maeve_options("axis_text_x_angle"),
            axis_text_y_size     = maeve_options("axis_text_y_size"),
            strip_text_size      = maeve_options("strip_text_size"),
            legend_text_size     = maeve_options("legend_text_size"),
            legend_position_char = maeve_options("legend_position_char"),
            title_label          = maeve_options("title_label"),
            x_label              = maeve_options("x_label"),
            y_label              = maeve_options("y_label"),
            geom_na_rm           = maeve_options("geom_na_rm")
           ){

  fit = match.arg( fit )

  ## Check that columns for group,time, and response are in the data frame.
  ## Note: 'subject_ID' is not used here since this is for group overlays.
  stopifnot( sapply( c(group_name, x_name, endpoint_name), as.character ) %in% colnames( data_frame ) )

  ## Commandeer columns for group, ID, time, and response to pass to ggplot() below.
  data_frame %<>%
      dplyr::mutate( 'group' = !!dplyr::sym( group_name ),
                     'x'     = !!dplyr::sym( x_name ),
                     'y'     = !!dplyr::sym( endpoint_name )
                    )
    

  ## Make sure predictor(s) are in the data.frame, but only if needed:
  if( 'linear' %in% fit ){
    stopifnot( eval( linear_predictor ) %in% colnames( data_frame ) )
    data_frame %<>%
    dplyr::mutate( 'pred_lin' = !!dplyr::sym( linear_predictor ) )
  }

  if( 'spline' %in% fit ){
    stopifnot( eval( spline_predictor ) %in% colnames( data_frame ) )
    data_frame %<>%
    dplyr::mutate( 'pred_gam' = !!dplyr::sym( spline_predictor ) )
  }

  if( 'piecewise' %in% fit ){
    stopifnot( eval( piecewise_predictor ) %in% colnames( data_frame ) )
    data_frame %<>%
    dplyr::mutate( 'pred_pwl' = !!dplyr::sym( piecewise_predictor ) )
  }

  if( 'poly' %in% fit ){
    stopifnot( eval( poly_predictor ) %in% colnames( data_frame ) )
    data_frame %<>% 
    dplyr::mutate( 'pred_poly' = !!dplyr::sym( poly_predictor ) )
  }

    
  fig_multi_group_template <-
   data_frame %>%
   ggplot2::ggplot( ggplot2::aes( x = x, y = y, group = group, colour = group ) ) +
   ggplot2::theme_bw( ) +
   ggplot2::theme( axis.text.x = ggplot2::element_text( size = axis_text_x_size, angle = axis_text_x_angle ),
                   axis.text.y = ggplot2::element_text( size = axis_text_y_size ),
                   strip.text  = ggplot2::element_text( size =  strip_text_size ), # set font size in facet panel titles
                   legend.text = ggplot2::element_text( size = legend_text_size ),
                   legend.position = legend_position_char
                  ) +
   ggplot2::labs( title = title_label, x = x_label , y = y_label )

    
  fig_multi_group <- fig_multi_group_template

  if( 'linear' %in% fit ){ # add fixed-effects fit from linear mixed model
  fig_multi_group <-
   fig_multi_group +
   (data_frame %>% dplyr::filter( !is.na( pred_lin ) ) %>%      
       ggplot2::geom_line( ggplot2::aes( y = pred_lin ), data = ., lwd = 2.0, na.rm = geom_na_rm )
    )
    }

  if( 'spline' %in% fit ){ # add fixed-effects fit from generalized additive mixed model
  fig_multi_group <-
   fig_multi_group +
    (data_frame %>% dplyr::filter( !is.na( pred_gam ) ) %>%      
     ggplot2::geom_line( ggplot2::aes( y = pred_gam ), data = ., lwd = 1.5, na.rm = geom_na_rm )
     )
  }

  if( 'piecewise' %in% fit ){ # add fixed-effects fit from piecewise linear mixed model
  fig_multi_group <-
   fig_multi_group +
    (data_frame %>% dplyr::filter( !is.na( pred_pwl ) ) %>%      
     ggplot2::geom_line( ggplot2::aes( y = pred_pwl ), data = .,  lwd = 1.5, na.rm = geom_na_rm )
     )
  }

  if( 'poly' %in% fit ){ # add fixed-effects fit from simple polynomial basis linear mixed model
  fig_multi_group <-
   fig_multi_group +
    (data_frame %>% dplyr::filter( !is.na( pred_pwl ) ) %>%      
     ggplot2::geom_line( ggplot2::aes( y = pred_poly ), data = .,  lwd = 1.5, na.rm = geom_na_rm )
     )
  }
    
 return( fig_multi_group )

} ## end of "draw_overlay <- function(...){..."
