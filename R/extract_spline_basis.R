#' Extract spline basis vector values evaluated at abcissa values of a gam model and return them in a data frame.
#'
#' This function uses extracts the plate regression spline basis vectors 
#' that are being fit in a GAMM.  They can be visualized with draw_spline_basis().
#' Both extract_spline_basis() and draw_spline_basis() are experimental and
#' still require some work to extract & order the spline bases you want, so
#' use with care.
#'
#' @param gam_model                 object of class "gam" returned from a maeve:: call to mgcv::gam()
#' @param current_DF                data.frame with group and x-value information for the fitted object.
#' @param spacing                   numeric how finely spaced should points be for grid evaluation?
#' @param tag_sort_vector           character vector with distinct character string "tags" by which to partially sort model basis vector names
#' @param sort_by_tag               logical whether to partially sort basis vector names by components of tag_sort_vector.
#' @param include_spline_coefficients logical whether to merge in the actual numeric coefficient of each spline basis in each group.
#' @param restrict_to_x_by_group    logical whether to exclude from a group x-values beyond the range of those observed in that group.
#' @param group_name_char           character name of the group-level identifier in the current_DF data.frame.
#' @param x_name_char               character name of the x-value (usually, e.g., time or concentration) identifier in the current_DF data.frame.
#' @param restrict_global_intercept logical whether to restrict global intercept values to only the reference group (i.e., the only group to get its intercept solely from the global intercept).
#' @param global_intercept_char     character name of global intercept column.
#' @param return_list               logical whether to return additional information, with also the design matrix included.
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
#'  ## Need to extract features of interest.
#'  ## Example:
#'  ##
#'  basis_spline_DF_2groups <- dplyr::filter( basis_spline_DF, group %in% c('dose_1.0', 'dose_100') )
#'  ##
#'  ind_1.0 <- grepl( 'groupdose_1.0', basis_spline_DF_2groups$coef_name_pretty, fixed = TRUE )
#'  ind_100 <- grepl( 'groupdose_100', basis_spline_DF_2groups$coef_name_pretty, fixed = TRUE )
#'  ##
#'  basis_spline_DF_2groups <-
#'      droplevels( subset( basis_spline_DF_2groups, (ind_1.0 | ind_100 ) & !is.na( group ) ) )
#'  basis_spline_DF_2groups <- subset( basis_spline_DF_2groups, abs( Ey ) > 1e-12 )
#'  basis_spline_DF_2groups <-
#'      dplyr::mutate( basis_spline_DF_2groups, coef_name_pretty = factor( coef_name_pretty ) )
#'  basis_spline_DF_2groups %<>% dplyr::arrange( group, coef_name_pretty ) %>% freeze_factor_levels()
#'  }
#'
#' @author Bill Forrest \email{forrest@@gene.com}
#'
#' @keywords character
#'
#' @seealso \code{\link{data.frame}}
#' @export
#'
extract_spline_basis <-
  function( gam_model,
            current_DF,
            spacing = .25,
            tag_sort_vector = c( "X.Intercept.", "group_01", "group_02", "group_03", "group_04" ),
            sort_by_tag = FALSE, # add a column of integers to sub-sort by presence of the tag vector.
            include_spline_coefficients = FALSE,
            restrict_to_x_by_group = FALSE,
            group_name_char = maeve_options('group_name'), # usually 'group_name' or 'group'
                x_name_char = 'x',
            restrict_global_intercept = FALSE,
            global_intercept_char = 'X.Intercept.',
            return_list = FALSE
           ){
    ##
    my_newdata <-
      with( current_DF,
            expand.grid( group = factor( levels( get( group_name_char ) ) ), ###factor( levels( group_name ) ),
                             x = seq( min( get( x_name_char ), na.rm = TRUE ),
                                      max( get( x_name_char ), na.rm = TRUE ),
                                      by = spacing
                                     )
                       )
           ) %>%
             dplyr::arrange( group, x )
    ##
    if( restrict_to_x_by_group ){
    ## For each group separately, remove rows with x-values that fall outside the range
    ## of x-values observed in the real data.  This will restrict a group's evaluated
    ## spline to the x-range for that group.
      my_newdata %<>%
      plyr::ddply( .var = 'group',
                   .fun = function( XX, CURRENT_DF = current_DF, Group_Name_Char = group_name_char ){
                          ## Restrict our copy of the real data frame to the single group in 'XX':                   
                          CURRENT_DF %<>% dplyr::filter( get(Group_Name_Char) == unique( as.character( XX$group ) ) )
                          ## Find the range of x-values observed in the restricted real data.
                          x_range = with( CURRENT_DF, range( x, na.rm = TRUE ) )
                          ## Find the design matrix restricted to the real-data range for this single group:
                          XX %<>% dplyr::filter( x >= min( x_range ) & x <= max( x_range ) )
                          ##
                          return(XX)
                  } # end '.fun = function( XX, CURRENT_DF ){...'
                  ) # end 'plyr::ddply( .var = 'group',...'
      
    } # end 'if( restrict_to_x_by_group ){...'
    ##
    design_DF <- my_newdata %>% data.frame( predict( gam_model, type = 'lpmatrix', newdata = my_newdata ) )
    
    ##
    basis_spline_DF <-
      design_DF %>%
      tidyr::gather( key = basis_spline_name, value = Ey, -x, -group ) %>%
      dplyr::mutate( basis_spline_name = factor( basis_spline_name, levels = unique( basis_spline_name ) ) )
    ##
    
    if( restrict_global_intercept ){
    ## excludes rows with non-reference group from the global intercept
    basis_spline_DF %<>%
      dplyr::filter( ! (group != levels( group )[1] & basis_spline_name == eval( global_intercept_char ) ) ) 
    } # end of 'if( restrict_global_intercept ){...'  
    ##
    
    basis_spline_DF$sort_integer <- NA
    
    if( sort_by_tag ){
      
      for( ii in 1:length( tag_sort_vector ) ){
        basis_spline_DF$sort_integer[ grepl( tag_sort_vector[ii], basis_spline_DF$basis_spline_name ) ] <- ii
      }
      
    } # end of 'if( sort_by_tag ){...'


    if( include_spline_coefficients ){
      
       PRETTY   <- names( coef( gam_model ) )
       ASCII    <- levels( basis_spline_DF$basis_spline_name )
       dist_mat <- utils::adist( PRETTY, ASCII )
       ##
       rownames( dist_mat ) <- PRETTY
       colnames( dist_mat ) <- ASCII
    ## Taking a quick look will show that one distance in every row (or every column)
    ## is lower than all the others.  This is the presumed match.

    match_order <- apply( dist_mat, 2, function(x) which( x == min(x) )  )

    stopifnot( is.numeric( match_order ) )
        
    ##
    coef_name_key <-
       data.frame(
            coef_name_pretty = rownames( dist_mat )[match_order],
             coef_name_ASCII = factor( colnames( dist_mat ), levels = levels(basis_spline_DF$basis_spline_name) )
                  )
       
    coef_DF <-
     data.frame( coef_name_pretty = PRETTY, coef_numeric = coef( gam_model ) ) %>%
     dplyr::left_join( coef_name_key, ., by = "coef_name_pretty" ) %>%
     maeve::round_numerics();
       
    rownames( coef_DF ) <- NULL

    basis_spline_DF %<>% dplyr::left_join( coef_DF, by = c( "basis_spline_name" = "coef_name_ASCII" ) )
       
    } # end of 'if( include_spline_coefficients ){...'


    if( return_list ){
        return( list( basis_spline_DF = basis_spline_DF, design_DF = design_DF ) )
    } else{
        return( basis_spline_DF )
    }

      
  } # end of 'extract_spline_basis <- function( gam_model, current_DF, spacing = .25, return_list = FALSE ){...'
