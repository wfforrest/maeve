#' Draw spline basis vector values evaluated at abcissa values of a gam model.
#'
#' This function uses the extract_spline_basis() output to sketch the thin
#' plate regression spline basis vectors that are being fit in a GAMM.
#' Both extract_spline_basis() and draw_spline_basis() are experimental and
#' still require some work to extract & order the spline bases you want, so
#' use with care.
#' 
#' @param spline_basis_data_frame   data frame with spline basis information, usually obtained from maeve::extract_spline_basis().
#' @param sort_rows                 logical whether to apply sorting criteria to spline basis values.
#' @param tolerance                 numeric spline coefficients below this in absolute value will be removed (set, e.g., to "-1" to avoid filtering entirely).
#' @param number_facet_rows         numeric number of rows in ggplot figure (passed to "facet_wrap( ..., nrow = number_facet_rows, ... )").
#' @param x_text                    character string with x-axis label for figure
#' @param y_text                    character string with y-axis label for figure
#' @param title_text                character string with title  label for figure
#' @param legend_position           character string with "theme( legend.position = legend_position )" value.
#' @param strip.text.size           numeric controls size of facet_wrap() strip labels: "theme( strip.text.x = element_text( size = strip.text.size ) )".
#' @param include_coefs             logical whether to include estimated spline coefficients in panels of faceted plot.
#' @param x_loc_frac                numeric horizontal fraction for placing geom_text() of coefficient value.
#' @param y_loc_frac                numeric   vertical fraction for placing geom_text() of coefficient value.
#'
#' @return a data.frame with values of basis vectors at different x-values across different groups.
#'
#' @examples
#'  cat('Example in dontrun block.')
#'  \dontrun{
#'  data( vismodegib )
#'  vismo21 <- dplyr::filter( vismodegib, DAY_OF_STUDY <= 21 )
#'  model_list = model_study( vismo21, metric = 'AUC' )
#'  basis_spline_DF <-
#'  extract_spline_basis( gam_model = model_list$md4_gamm4[['gam']],
#'                        current_DF = vismo21,
#'                        sort_by_tag = TRUE,
#'                        include_spline_coefficients = TRUE,
#'                        x_name_char = 'DAY_OF_STUDY', # x-variable name in vismodegib
#'                        spacing = .50
#'                       )
#'  ##
#'  main <- 'thin plate regression spline basis for three groups from vismodegib over 21 days'
#'  basis_spline_DF_3groups <-
#'      dplyr::filter( basis_spline_DF, group %in% c('dose_1.0', 'dose_6.0', 'dose_100') )
#'  ##
#'  ind_1.0 <- grepl( 'groupdose_1.0', basis_spline_DF_3groups$coef_name_pretty, fixed = TRUE )
#'  ind_6.0 <- grepl( 'groupdose_6.0', basis_spline_DF_3groups$coef_name_pretty, fixed = TRUE )
#'  ind_100 <- grepl( 'groupdose_100', basis_spline_DF_3groups$coef_name_pretty, fixed = TRUE )
#'  ##
#'  basis_spline_DF_3groups <-
#'      droplevels( subset( basis_spline_DF_3groups, ( ind_1.0 | ind_6.0 | ind_100 ) ) )
#'  basis_spline_DF_3groups <- droplevels( subset( basis_spline_DF_3groups, !is.na( group ) ) )
#'  basis_spline_DF_3groups <- subset( basis_spline_DF_3groups, abs( Ey ) > 1e-12 )
#'  basis_spline_DF_3groups %<>%
#'      dplyr::mutate( group = factor( group, levels = c('dose_1.0', 'dose_6.0', 'dose_100') ) )
#'  basis_spline_DF_3groups <-
#'      dplyr::mutate( basis_spline_DF_3groups, coef_name_pretty = factor( coef_name_pretty ) )
#'  basis_spline_DF_3groups %<>% dplyr::arrange( group, coef_name_pretty ) %>% freeze_factor_levels()
#'  ##
#'  figure_vismo_basis <- draw_spline_basis( basis_spline_DF_3groups, title_text = main )
#'  }
#'
#' @author Bill Forrest \email{forrest@@gene.com}
#'
#' @keywords character
#'
#' @seealso \code{\link{data.frame}}
#' @export
#'
draw_spline_basis <-
  function( spline_basis_data_frame, ### data frame with spline values, usually from "extract_spline_basis()".
            ##
              sort_rows = TRUE,
              tolerance = 10^( -12 ),
            ##         
      number_facet_rows = nlevels( spline_basis_data_frame$group ),
            ##
                 x_text = 'day of study',
                 y_text = 'spline function value',
             title_text = 'spline basis functions',
            ##
        legend_position = c( 'bottom', 'none', 'left', 'top', 'right' ),
        strip.text.size = 7,
            ##
          include_coefs = TRUE,
             x_loc_frac = 0.2,
             y_loc_frac = 0.1
            ##
           ){

  legend_position = match.arg( legend_position )
    
  if( sort_rows ) {
    spline_basis_data_frame %<>%
       dplyr::arrange( group, sort_integer ) %>%
        maeve::freeze_factor_levels()
  }
    
  figure <- 
  spline_basis_data_frame %>%  
  dplyr::filter( abs( Ey ) > tolerance ) %>% # exclude zero-ed out terms.
  ##
  ggplot( aes( x = x, y = Ey, colour = group, group = group ) ) +
  geom_point( size = 0.25, alpha = 0.67, position = position_jitter( width = 0.0001, height = 0.0001 ) ) +
  geom_line( ) +
  facet_wrap( ~ basis_spline_name, nrow = number_facet_rows, dir = 'h' ) +
  ##      
  theme( legend.position = legend_position, strip.text.x = element_text( size = strip.text.size ) ) +
  labs( title = title_text, y = y_text, x = x_text )

    
  if( include_coefs ){
  figure <-
   figure +      
   geom_text( aes(     x = min( x )  + x_loc_frac * diff( range(  x, na.rm = TRUE ) ),
                       y = min( Ey ) + y_loc_frac * diff( range( Ey, na.rm = TRUE ) ),
                  colour = group,
                   label = format( coef_numeric, digits = 1 )
                  ),
             colour = 'gray50',
             size = 3
             )
  }    

  return(figure)
  
 } # end of function definition for 'maeve::draw_spline_basis()'
