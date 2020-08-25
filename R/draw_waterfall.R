#' Create a ggplot2 object with a waterfall plot from a maeve study.
#'
#' @author Bill Forrest <forrest@@gene.com>
#'
#' @param data_frame data frame with factors for group and ID, numerics for time on study and response.
#' @param bar         character vector with description of how to determine bar range for each subject_ID.
#' @param ordering    character vector with description of how to order the bars from left to right (within panel, if facet_char != 'none')
#' @param facet_char  character vector with description of how to facet:  "facet_wrap( ~ facet_char, ...)"
#' @param scales_char character vector with description of how to set x- and y-axes within facets.
#' @param vertical_x_label logical whether to rotate the x-axis labels by default.  Usually desirable behavior in the waterfall plot.
#' @param return_list logical whether to return multiple results in a list.  If FALSE (default), just a ggplot is returned.
#' @param group_name           character.   See ?maeve_options().
#' @param subject_ID           character.   See ?maeve_options().
#' @param x_name               character.   See ?maeve_options().
#' @param endpoint_name        character.   See ?maeve_options().
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
#'
#' @return A ggplot2 object
#'
#' @examples
#'  cat('Example for draw_waterfall() in dontrun code block.')
#'  \dontrun{
#'  data( vismodegib )
#'  vismo21 <- dplyr::filter( vismodegib, DAY_OF_STUDY <= 21 )
#'  vismo21 <- dplyr::mutate( vismo21, y = log( 1 + TUMOR_VOLUME ) ) # add log-scale endpoint.
#'  figure_waterfall <- draw_waterfall( vismo21, endpoint_name = 'y', facet_char = 'group_name',
#'                                      ncol_value = 5 )
#' }
#'
#' @author Bill Forrest \email{forrest@@gene.com}
#'
#' @references \url{www.r-project.org}
#'
#' @keywords data.frame
#' @export
#'
draw_waterfall <-
  function( data_frame, # Needs group_name, subject_ID, x_name, endpoint_name; no model required.
            bar         = c( 'both', 'max', 'min', 'extreme', 'last' ), # how to choose height of boxes?
            ordering    = c(         'extreme', 'max', 'min', 'last' ), # left-to-right order to arrange waterfall.
            facet_char  = c( 'none', 'group_name' ),
            scales_char = c( 'free_x','fixed', 'free_y', 'free' ),
            vertical_x_label = TRUE,
            return_list = FALSE,
            ##
            group_name           = maeve_options("group_name"),
            subject_ID           = maeve_options("subject_ID"),
            x_name               = maeve_options("x_name"),
            endpoint_name        = maeve_options("endpoint_name"),
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
            geom_na_rm           = maeve_options("geom_na_rm")
            ##           
           ){

      
  bar         = match.arg( bar )

  ordering    = match.arg( ordering )

  facet_char  = match.arg( facet_char )

  scales_char = match.arg( scales_char )
    
  ## Check that columns for group, ID, time, and response are in the data frame.
  stopifnot( sapply( c(group_name, subject_ID, x_name, endpoint_name), as.character ) %in% colnames( data_frame ) )

  ## Commandeer columns for group, ID, time, and response to pass to ggplot() below.
  data_frame %<>%
    dplyr::mutate( 'group' = !!dplyr::sym( group_name ),
                   'ID'    = !!dplyr::sym( subject_ID ),
                   'x'     = !!dplyr::sym( x_name ),
                   'y'     = !!dplyr::sym( endpoint_name )
                  )

  ## Filter out missing values up front if geom_na_rm == TRUE.
  if( geom_na_rm ){
    data_frame %<>% dplyr::filter( !is.na( y ) & !is.na( x ) )
  }

  ## Usually want vertical x-axis labels. If vertical_x_label == FALSE, just use value of axis_text_x_angle.
  if( vertical_x_label ){
    axis_text_x_angle = 90 # overrides maeve_options( 'axis_text_x_angle' ) value.
  }
    
  waterfall <-
    data_frame %>%
    dplyr::group_by( group, ID ) %>%
    dplyr::summarise( logFC_max = max( y ) - unique( y[ x == min(x) ] ),
                      logFC_min = min( y ) - unique( y[ x == min(x) ] ),
                      logFC_last = unique( y[ x == max(x) ] ) - unique( y[ x == min(x) ] )                    
                     ) %>%
    dplyr::mutate( logFC_extreme = ifelse( abs(logFC_max) >= abs(logFC_min), logFC_max, logFC_min ) )

  ## Sort by one of the three summaries, as determined by the 'ordering' parameter.
  if( ordering == 'max'     ){ waterfall %<>% dplyr::arrange( desc( logFC_max     ) ) }
  if( ordering == 'min'     ){ waterfall %<>% dplyr::arrange( desc( logFC_min     ) ) }
  if( ordering == 'extreme' ){ waterfall %<>% dplyr::arrange( desc( logFC_extreme ) ) }
  if( ordering == 'last'    ){ waterfall %<>% dplyr::arrange( desc( logFC_last    ) ) }
    
  waterfall %<>% data.frame %>% maeve::freeze_factor_levels( no_freeze = 'group' )

  fig_template <-
     waterfall %>%
     ggplot2::ggplot( aes( x = ID, colour = group, fill = group ) ) +
     ggplot2::theme( axis.text.x = ggplot2::element_text(  size = axis_text_x_size,
                                                          angle = axis_text_x_angle,
                                                          hjust = axis_text_x_hjust,
                                                          vjust = axis_text_x_vjust
                                                         ),
                     axis.text.y = ggplot2::element_text( size = axis_text_y_size ),
                     strip.text  = ggplot2::element_text( size =  strip_text_size ),
                     legend.text = ggplot2::element_text( size = legend_text_size ),
                     legend.position = legend_position_char                    
                    ) +
     ggplot2::labs( title = title_label, x = x_label , y = y_label )

  if( bar == 'max' ){
        fig <- fig_template + ggplot2::geom_bar( aes( y = logFC_max ), stat = 'identity' )
  }

  if( bar == 'min' ){
        fig <- fig_template + ggplot2::geom_bar( aes( y = logFC_min ), stat = 'identity' )
  }

  if( bar == 'both' ){
        fig <- fig_template + ggplot2::geom_bar( aes( y = logFC_max     ), stat = 'identity' ) +
                              ggplot2::geom_bar( aes( y = logFC_min     ), stat = 'identity' )
  }

  if( bar == 'extreme' ){
        fig <- fig_template + ggplot2::geom_bar( aes( y = logFC_extreme ), stat = 'identity' )
  }

  if( bar == 'last' ){
        fig <- fig_template + ggplot2::geom_bar( aes( y = logFC_last    ), stat = 'identity' )
  }
          
  ## Should we facet_wrap() the plot?
  if( facet_char == 'group_name' ){
      fig <- fig +
             ggplot2::facet_wrap( ~ group, scales = scales_char, nrow = nrow_value, ncol = ncol_value )
  }

  if( return_list ){
      return( list( waterfall_plot = fig, waterfall_data_frame = waterfall ) )
  } else{
      return( fig )
  }

} ## end of "draw_waterfall <- function(...){..."

