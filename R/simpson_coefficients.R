#' Find coefficients used in Simpson's approximation for numerical integration
#'
#' Takes an increasing, equally spaced numeric vector 'x' and returns 
#' Simpson's Rule cofficients used in numerical integration.
#'
#' @author Bill Forrest <forrest@@gene.com>
#'
#' @param x numeric vector of abcissa values in ascending order.
#' @param return_list logical whether to return computed coefficients in a vector or the separate components in a list.
#' @param diff_TOL numeric measure of how uneven the x-values can be (they should be evenly spaced).
#' @param suppress_warning logical for whether to return warning.
#'
#' @return vector of numeric cofficients if return_list is TRUE; list of integers coefficients and interval size "h" separately if return_list is FALSE
#'
#' @examples
#'  x <- seq(0, 1, by = .1); y <- 6 * x * ( 1 - x )
#'  w <- simpson_coefficients( x )
#'  print( sum( w * y ) )
#'
#'  simpson_coefficients( x, return_list = TRUE )
#'
#' @author Bill Forrest \email{forrest@@gene.com}
#'
#' @references \url{www.r-project.org}
#'
#' @keywords integration
#' @export
#'
 simpson_coefficients <-
  function( x, return_list = FALSE, diff_TOL = .001, suppress_warning = TRUE ){

    n <- length(x)
    
    if( n <= 2 ){ ### only allow cases with 3 or more abcissa values.
        stop("error in maeve::simpson_coefficients(): must have three or more { (x_i, y_i) } pairs to use Simpson's Rule")
    }
    
    diff_grid <- diff( x )
    
    if( any(diff_grid <= 0) ){
        stop('error in maeve::simpson_coefficients(): x-values must be in ascending order')
    }

    if( diff( range( diff_grid ) ) / min( diff_grid ) >= diff_TOL ){
        stop('error in maeve::simpson_coefficients(): x-values must be uniformly spaced')
    }

    h <- (stats::median)( diff_grid ) ## should all be about the same.
    
    simpson_fractions <- ## hard code the small-n cases; makes code below easier.
    switch( n,
            c( 1 ) * ( 1 / 1 ), ## n = 1       ## This case is not allowed currently.
            c( 1, 1 ) * ( 1 / 2 ), ## n = 2    ## This case is not allowed currently.
            c( 1, 4, 1 ) * ( 1 / 3 ), ## n = 3
            c( 1, 3, 3, 1 ) * ( 3 / 8 ), ## n = 4; Simpson's three-eighths rule
            c( 1, 4, 2, 4, 1 ) * ( 1 / 3 ), ## n = 5
           ### NB: Next line, 'n=6' uses (3/8) rule for first section, (1/3) rule 2nd section. *Two* nonzero coefficients at 4th abcissa.
            c( 1, 3, 3, 1, 0, 0 ) * (3 / 8) + c( 0, 0, 0, 1, 4, 1 )  * ( 1 / 3 ) # n = 6; 
           )
    
    if( n %% 2 & n >= 7 ){ ## "n" is odd and at least 7.
      ## This is the (standard) Simpson's approach for an odd number of { (x_i, y_i) } pairs.
      simpson_fractions <-
      ## These are the "{1, 4, 1}" overlapping
      ## triples from Simpson's approximation:
      ( 1 / 3 ) * ## include the ( 1 / 3 ) term, but not the interval size.
      (
       1 * (1:n) %in% seq( 1, n    , by = 2) +
       4 * (1:n) %in% seq( 2, n - 1, by = 2) +
       1 * (1:n) %in% seq( 3, n - 1, by = 2) ## this leaves out first & last entries.
      )
    }

    if( !(n %% 2) & n >= 8 ){ ## "n" is even and at least 8.
      
      if( !suppress_warning ){
        warning('warning in simpson_coefficients(): proceeding with an even number of {(x_i,y_i)} pairs.')
      }
      ## This is a workaround for an even number of { (x_i, y_i) } pairs.      
      ## Workaround:
      ## (1) Get Simpson's 3/8 rule coefficients for i in {1,2,3,4}.
      ## (2) Get Simpson's 1/3 rule coefficients for i in {4,...,n}. Note "i = 4" is in both.
      ## (3) Add the coefficients.
      simpson_fractions_1_3 <- simpson_fractions_3_8 <- rep( 0, n )
      simpson_fractions_3_8[1:4] <- ( 3 / 8 ) * c( 1, 3, 3, 1 )
      
      simpson_fractions_1_3 <- 
      # These are the "{1, 4, 1}" overlapping
      # triples from Simpson's approximation:
      ( 1 / 3 ) * # include the ( 1 / 3 ) term, but not the interval size.
      (
       1 * (1:n) %in% seq( 4, n    , by = 2) +
       4 * (1:n) %in% seq( 5, n - 1, by = 2) +
       1 * (1:n) %in% seq( 6, n - 1, by = 2) # this leaves out first & last entries.
      )
      
      simpson_fractions <- simpson_fractions_1_3 + simpson_fractions_3_8
      
    }
    
    if( ! return_list ){
      return( h * simpson_fractions ) ## 'sum( h * simpson_fractions * y )' approximates the definite integral
    } else{
      return( list( h = h, simpson_fractions = simpson_fractions ) )
    }
    
 }
