#' Create a ggplot2 object with raw longitudinal data and optional model fits.
#'
#' @author Bill Forrest <forrest@@gene.com>
#'
#' @param data_frame data frame with factors for group and ID, numerics for time on study and response.
#' @param fit character vector with names of fitted values to include.
#' @param facet logical.  Whether to facet plots by group.  Defaults to TRUE.
#' @param group_name           character.   See ?maeve_options().
#' @param subject_ID           character.   See ?maeve_options().
#' @param x_name               character.   See ?maeve_options().
#' @param endpoint_name        character.   See ?maeve_options().
#' @param linear_predictor     character.   See ?maeve_options().
#' @param spline_predictor     character.   See ?maeve_options().
#' @param piecewise_predictor  character.   See ?maeve_options().
#' @param poly_predictor       character.   See ?maeve_options().
#' @param linear_color         character.   See ?maeve_options().
#' @param spline_color         character.   See ?maeve_options().
#' @param piecewise_color      character.   See ?maeve_options().
#' @param poly_color           character.   See ?maeve_options().
#' @param linear_lwd           character.   See ?maeve_options().
#' @param spline_lwd           character.   See ?maeve_options().
#' @param piecewise_lwd        character.   See ?maeve_options().
#' @param poly_lwd             character.   See ?maeve_options().
#' @param nrow_value           numeric.     See ?maeve_options().
#' @param ncol_value           numeric.     See ?maeve_options().
#' @param axis_text_x_size     numeric.     See ?maeve_options().
#' @param axis_text_x_angle    numeric.     See ?maeve_options().
#' @param axis_text_x_hjust    numeric.     See ?maeve_options().
#' @param axis_text_x_vjust    numeric.     See ?maeve_options().
#' @param axis_text_y_size     numeric.     See ?maeve_options().
#' @param strip_text_size      numeric.     See ?maeve_options().
#' @param legend_text_size     numeric.     See ?maeve_options().
#' @param legend_position_char character.   See ?maeve_options().
#' @param title_label          character.   See ?maeve_options().
#' @param x_label              character.   See ?maeve_options().
#' @param y_label              character.   See ?maeve_options().
#' @param geom_na_rm           logical.     See ?maeve_options().
#' @param geom_point_size      numeric.     See ?maeve_options().
#' @param alpha_value          numeric.     See ?maeve_options().
#'
#' @return A ggplot2 object
#'
#' @examples
#'  cat('Example for draw_study() in dontrun code block.')
#'  \dontrun{
#'  data( vismodegib )
#'  vismo21 <- dplyr::filter( vismodegib, DAY_OF_STUDY <= 21 )
#'  pred_data_frame = vismo21 %>% model_study() %>% predict_study()
#'  figure_by_group <- draw_study( pred_data_frame, endpoint_name = 'y',
#'                                 fit = 'spline', ncol_value = 5 )
#' }
#'
#' @author Bill Forrest \email{forrest@@gene.com}
#'
#' @references \url{www.r-project.org}
#'
#' @keywords data.frame
#' @export
#'
draw_study <-
  function( data_frame,      # predict_study_list$data[['clean_DF_pred']],
            fit = c( 'none', 'linear', 'spline', 'piecewise', 'poly' )[1], # what fit to add to picture?
            facet = TRUE,
            ##
            group_name           = maeve_options("group_name"),
            subject_ID           = maeve_options("subject_ID"),
            x_name               = maeve_options("x_name"),
            endpoint_name        = maeve_options("endpoint_name"),
            ##
            linear_predictor     = maeve_options("linear_predictor"),
            spline_predictor     = maeve_options("spline_predictor"),
            piecewise_predictor  = maeve_options("piecewise_predictor"),
            poly_predictor       = maeve_options("poly_predictor"),
            ##
            linear_color         = maeve_options("linear_color"),
            spline_color         = maeve_options("spline_color"),
            piecewise_color      = maeve_options("piecewise_color"),
            poly_color           = maeve_options("poly_color"),
            ##
            linear_lwd           = maeve_options("linear_lwd"),
            spline_lwd           = maeve_options("spline_lwd"),
            piecewise_lwd        = maeve_options("piecewise_lwd"),
            poly_lwd             = maeve_options("poly_lwd"),
            ##
            nrow_value           = maeve_options("nrow_value"),
            ncol_value           = maeve_options("ncol_value"),
            ##
            axis_text_x_size     = maeve_options("axis_text_x_size"),
            axis_text_x_angle    = maeve_options("axis_text_x_angle"),
            axis_text_x_hjust    = maeve_options("axis_text_x_hjust"),
            axis_text_x_vjust    = maeve_options("axis_text_x_vjust"),
            axis_text_y_size     = maeve_options("axis_text_y_size"),
            strip_text_size      = maeve_options("strip_text_size"),
            legend_text_size     = maeve_options("legend_text_size"),
            legend_position_char = maeve_options("legend_position_char"),
            title_label          = maeve_options("title_label"),
            x_label              = maeve_options("x_label"),
            y_label              = maeve_options("y_label"),
            geom_na_rm           = maeve_options("geom_na_rm"),
            geom_point_size      = maeve_options("geom_point_size"),
            alpha_value          = maeve_options("alpha_value")
            ##
           ){

  ## This next bit is so that by default there is one choice ('none'),
  ## but that multiple fits will be acceptable if entered.
  fit = match.arg( fit, choices = c( 'none', 'linear', 'spline', 'piecewise', 'poly' ), several.ok = TRUE )

  ## Check that columns for group, ID, time, and response are in the data frame.
  stopifnot( sapply( c( group_name, subject_ID, x_name, endpoint_name), as.character ) %in% colnames( data_frame ) )

  ## Commandeer columns for group, ID, time, and response to pass to ggplot() below.
  data_frame %<>%
    dplyr::mutate( 'group' = !!dplyr::sym( group_name ),
                   'ID'    = !!dplyr::sym( subject_ID ),
                   'x'     = !!dplyr::sym( x_name ),
                   'y'     = !!dplyr::sym( endpoint_name )
                  )

  ## Filter out missing values up front if geom_na_rm == TRUE.
  if( FALSE & geom_na_rm ){ ### Disabled 20180813.  Added in-line removal below.
    data_frame %<>% dplyr::filter( !is.na( y ) )
  }

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

    
  ## Add a color-by-ID factor that looks more diverse when faceting by group:
  data_frame %<>% dplyr::mutate( color_order = maeve::splice_color_order( group, ID ) )
  
  fig_multi_group_template <-
   data_frame %>%   
   ggplot2::ggplot( ggplot2::aes( x = x, y = y, group = ID, colour = color_order ) ) +
   ggplot2::theme_bw( ) +
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
   ggplot2::labs( title = title_label, x = x_label , y = y_label )

  if( facet ){
     ## whether to facet by group.
     ## Defaults to TRUE.  Set to FALSE to make so-called 'spider plots'.
     fig_multi_group_template <-
     fig_multi_group_template +
     ggplot2::facet_wrap ( ~ group, nrow = nrow_value, ncol = ncol_value )
  } else{
     stop('error in maeve::draw_study(): option "facet = FALSE" is not yet implemented; please leave it at TRUE.')
  }
     
 fig_multi_group <-
   fig_multi_group_template +
   ggplot2::geom_point( alpha = alpha_value, na.rm = geom_na_rm,  size = geom_point_size ) +
   ## Filter out any NA points in order to linearly interpolate across any missing values.
   ## This is because the point of this step is just to visually clarify which points come from
   ## the same 'ID' grouping, not make any functional claim about unobserved values in between.     
   ggplot2::geom_line(  data = data_frame %>% dplyr::filter( !is.na( y ) ), alpha = alpha_value, lty = 1 ) 

    
  if( 'linear' %in% fit ){ # add fixed-effects fit from linear mixed model
  fig_multi_group <-
    fig_multi_group +
    (data_frame %>% dplyr::filter( !is.na( pred_lin ) ) %>%      
     ggplot2::geom_line( ggplot2::aes( y = pred_lin ), data = ., colour = eval( linear_color ), lwd = linear_lwd, na.rm = geom_na_rm )
    )
  }

  if( 'spline' %in% fit ){ # add fixed-effects fit from generalized additive mixed model
  fig_multi_group <-
    fig_multi_group +
    (data_frame %>% dplyr::filter( !is.na( pred_gam ) ) %>%      
     ggplot2::geom_line( ggplot2::aes( y = pred_gam ), data = ., colour = eval( spline_color ) , lwd = spline_lwd, na.rm = geom_na_rm )
    )
  }

  if( 'piecewise' %in% fit ){ # add fixed-effects fit from piecewise linear mixed model
  fig_multi_group <-
    fig_multi_group +
    (data_frame %>% dplyr::filter( !is.na( pred_pwl ) ) %>%      
     ggplot2::geom_line( ggplot2::aes( y = pred_pwl ), data = ., colour = eval( piecewise_color ), lwd = piecewise_lwd, na.rm = geom_na_rm )
    )
  }
    
  if( 'poly' %in% fit ){ # add fixed-effects fit from piecewise linear mixed model
  fig_multi_group <-
    fig_multi_group +
    (data_frame %>% dplyr::filter( !is.na( pred_pwl ) ) %>%      
     ggplot2::geom_line( ggplot2::aes( y = pred_poly ), data = ., colour = eval( poly_color ), lwd = poly_lwd, na.rm = geom_na_rm )
    )
  }
    
 return( fig_multi_group )
    
} ## end of "draw_study <- function(...){..."
