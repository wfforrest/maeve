#' Truncate values in a numeric vector, logically or numerically.
#'
#' Takes a numeric vector and returns a vector of equal length, either a logical or a numeric with values truncated within provided ranges.
#'
#' @param x numeric vector to be truncated
#' @param lower numeric lower end or allowable range.
#' @param upper numeric upper end or allowable range.
#' @param type character indicating whether to return a "logical" or "numeric" vector.
#'
#' @return a vector the same length as the input vector, with either logical or numeric entries.
#'
#' @examples
#'    dat <- data.frame( x = 0:10, y = rnorm( 11 ), letter = letters[1:11] )
#'    truncate_numeric( dat$x, 3, 8 ) # logical vector by default
#'    subset( dat, truncate_numeric( dat$x, 3, 8 ) ) # subset a data frame.
#'    #
#'    truncate_numeric( dat$x, 3, 8, 'numeric' ) # numeric vector
#'
#'
#' @author Bill Forrest \email{forrest@@gene.com}
#'
#' @keywords truncate
#'
#' @seealso \code{\link{data.frame}}
#' @export
#'
truncate_numeric <-
  function( x, lower = -Inf, upper = Inf, type = c('logical', 'numeric') ){

    type = match.arg( type )
    
    if( any( !is.numeric(x) ) |
        any( !is.numeric( lower )) |
        any( !is.numeric( upper ))
       ){
        stop('error in truncate_numeric(): input must be numeric')
    }

    if( length( lower ) > 1 | length( upper ) > 1 ){
        stop('error in truncate_numeric(): range limits must each be of length 1')
    }
    
    xt <- x # "xt" = 'x-truncated'
    
    if( type == 'numeric' ){
    ## truncate numeric values into range:      
     xt <- ifelse( xt < lower, lower, xt )
     xt <- ifelse( xt > upper, upper, xt )
    }

    if( type == 'logical' ){
    ## make a vector that's TRUE when x is in range, FALSE otherwise.
     xt <- (lower <= xt) & (xt <= upper)
    }
    
    return( xt )
    
  }
