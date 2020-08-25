#' Approximate the first derivative of a spline passing through a set of (x,y) pairs.
#'
#' @author Bill Forrest <forrest@@gene.com>
#'
#' @param x numeric abcissa values for the spline.  Should be 
#' @param y numeric ordinae values for the spline.
#' @param relative_tolerance numeric tolerance parameter for how close to uniformly spaced the x-values must be.
#' @param dx_frac numeric relative value for how far out to step from given x-values in approximating first derivative.
#' @param extended_output logical. Whether to return a data frame with all the components leading to the first derivative.
#'
#' @return A numeric vector the same length as x & y with the first derivative of a spline through the points, or a data.frame if extended_output == TRUE.
#'
#' @examples
#'  x <- seq(0, 2*pi, length = 101); 
#'  y <- sin(x) 
#'  dy.dx <- maeve:::approximate_first_derivative(x,y); 
#'  results <- cbind( x, y, cos(x), dy.dx ) # 3rd & 4th columns should match closely
#'
#' @author Bill Forrest \email{forrest@@gene.com}
#'
#' @references \url{www.r-project.org}
#'
#' @keywords list
#'
approximate_first_derivative <- function( x, y, relative_tolerance = .01, dx_frac = .01, extended_output = FALSE ){
      ## use a cubic spline interpolator to estimate the derivative of a smooth function through the given values.

      stopifnot( is.numeric( x ) & is.numeric( y ) )
  
      if( any( is.na( x ) ) ){
        stop('error in approximate_first_derivative(): x grid cannot contain NA values.')
      }

      if(  max( diff( x ) ) / min( diff( x ) ) - 1 > relative_tolerance ){
          stop( 'error in approximate_first_derivative(): x grid is not uniformly spaced in ascending order; ',
                'increase "relative_tolerance" value to use with these data '
               )
      }

      dx <- median( diff( sort( x ) ) ) * dx_frac # small positive "delta" for finite difference
     
      ## We need two cases to deal with the left boundary and right boundary
       left_endpoint <- x == min( x )
      right_endpoint <- x == max( x )

      x0 <- x1 <- x # set up placeholder matrices to find a finite difference
 
      x0[ ! left_endpoint  ] <- x0[ ! left_endpoint  ] - dx
      x1[ ! right_endpoint ] <- x1[ ! right_endpoint ] + dx     

      y0 <- spline( x, y, xout = x0 )$y
      y1 <- spline( x, y, xout = x1 )$y

      first_derivative <- ( y1 - y0 ) / ( x1 - x0 )

      if( !extended_output ){
        return( first_derivative )
      } else{
        return( data.frame( x, y, x0, y0, x1, y1, first_derivative ) )
      }
        
   } # end of 'approximate_first_derivative <- function( x, y, relative_tolerance = .01, dx_frac = .01, extended_output = FALSE ){...
