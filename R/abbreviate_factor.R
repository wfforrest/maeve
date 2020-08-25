#' Abbreviate a factor.
#' 
#' Calls the base::abbreviate() function on a factor, and returns
#' a new factor with abbreviated level values.
#'
#' @param x factor
#' @param n numeric positive integer passed to abbreviate().
#'
#' @return a factor
#'
#' @examples
#'    x <- factor( c( 'catttt', 'dogggg', 'catttt' ) )
#'    out <- maeve:::abbreviate_factor( x, n = 3 )
#'
#' @author Bill Forrest \email{forrest@@gene.com}
#'
#' @keywords factor abbreviate
#'
#' @seealso \code{\link{factor}}
#'
#'
abbreviate_factor <- function( x, n = 1e9 ){
  
      if( !is.factor( x ) ){
          stop('error in abbreviate_factor(): "x" must be a factor')
      }

      interim <- abbreviate( as.character(x), n )
      names( interim ) <- NULL
      
      factor( interim, levels = abbreviate( levels( x ), n ) )
  
 }
