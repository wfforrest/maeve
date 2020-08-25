#' Make a design matrix from a grouping factor and numeric time vector from a user-selected basis type.
#'
#' Takes a data frame with a grouping factor and a vector of time 'x'-values and a
#' user choice of basis function type (currently 'tent' or 'poly') and returns
#' a design matrix for fitting a piecewise linear mixed effects model.
#' Tent basis vectors are created from the so-called "tent-functions".  See, e.g., Simon Wood (2017)
#' GAM 2nd edition text, section 4.2, page 164-166.  Polynomial basis vectors (not to be
#' confused with the gam spline approach) use the stats::poly() function.  This is
#' intended to be used in fitting linear mixed effects models to three (maybe four?) time points.
#' It can in principal be used for more than that, but in that circumstance the spline approach
#' will typically be better and is recommended.
#'
#' @param study_data_frame data.frame with time ('x') vector and grouping vector
#' @param basis_choice character describing which basis function type to use in making a design matrix.
#' @param format_digits how many digits to retain in column names?
#' @param use_poly_coefs logical whether to use "attr( maeve_options('poly_object'), 'coef')" to scale poly basis matrix.
#' @param group_name          character.      See ?maeve_options().
#' @param x_name              character.      See ?maeve_options().
#' @param break_points        numeric vector. See ?maeve_options().
#' @param number_break_points numeric.        See ?maeve_options().
#' @param poly_degree         numeric.        See ?maeve_options().
#'
#' @return a design matrix, including intercept(s), that can be used with lme4::lmer().
#'
#' @examples
#'    ### Make a basis for piecewise linear regression over 21 days from the vismodegib data:
#'       require( magrittr ) # want pipe operator "%>%"
#'       vismo_pwl_basis <-
#'         vismodegib %>%
#'         dplyr::filter( DAY_OF_STUDY <= 21 ) %>%
#'         maeve:::construct_design_matrix_from_basis( basis_choice = 'tent', 
#'                                                     break_points = c( 0, 7, 14, 21 )
#'                                                    )
#'
#' @author Bill Forrest \email{forrest@@gene.com}
#'
#' @keywords matrix
#'
#' @seealso \code{\link{matrix}}
#'
construct_design_matrix_from_basis <-
   function( study_data_frame,
             basis_choice = c( 'tent', 'poly' ),
             format_digits = 3,
             use_poly_coefs = FALSE,
             ##
             group_name          = maeve_options("group_name"),
             x_name              = maeve_options("x_name"),
             break_points        = maeve_options("break_points"),
             number_break_points = maeve_options("number_break_points"),
             poly_degree         = maeve_options("poly_degree")
             ){

        basis_choice = match.arg( basis_choice )
       
        x     <- study_data_frame[,     x_name]
        group <- study_data_frame[, group_name]

       
        if( basis_choice == 'tent' ){
        
          if( is.null( break_points ) ){
              ## By default, give evenly-spaced break points based on a number of break_points requested.
              break_points  <- quantile( sort(unique(x)), probs = seq( 0, 1, length = number_break_points ), names = FALSE )
          }


          if( !( min(x) >= min(break_points) & max(x) <= max( break_points ) ) ){
            stop(  'error in maeve::construct_design_matrix_from_basis(): ',
                   'x-values must fall within closed range of break points.'
                 )
          } # end of 'if( !( min(x) >= min(break_points) & max(x) <= max( break_points ) ) ){...'
            

          X <- make_basis_matrix( x, basis = basis_choice, break_points = break_points )

          if( nlevels( group ) > 1 ){
              mf <- formula( ' ~ (-1) + group:X' )          
          } else{
              mf <- formula( ' ~ (-1) + X' ) # if nlevels( group ) == 1, then the model.matrix with "group:X" is not identifiable.
          }

          XX <- model.matrix( mf, data = data.frame( group = group ) )
        
        } # end of 'if( basis_choice == 'tent' ){...'

        

        if( basis_choice == 'poly' ){
          
          X <- make_basis_matrix( x, basis = basis_choice, degree = poly_degree, use_poly_coefs = use_poly_coefs )

          if( !use_poly_coefs ){
            ## Do NOT reset if we're using the polynomial coefficients for scaling, since that indicates that we already
            ## have an object stored.
            maeve_options( 'poly_object' = X ) # store this so we can extract scaling coefficients for numerical integration in compare_groups().
          }
        
          if( nlevels( group ) > 1 ){
              mf <- formula( ' ~ group + group:X' )          
          } else{
              X <- cbind( `(Intercept)` =  1 / sqrt( nrow( X ) ), X )            
              mf <- formula( ' ~ (-1) + X' )
          }

          XX <- model.matrix( mf, data = data.frame( group = group ) )

        } ## end of 'if( basis_choice == 'poly' ){...'
        
            
        return( XX )

}

