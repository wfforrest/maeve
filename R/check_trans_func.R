#' Check the transformation and inverse transformation functions.
#' 
#' @param trans_func_char character with transformation name.
#' @param inv_func_char character with inverse name.
#' @param test_func_x numeric values at which to check inverse.
#' @param TOLERANCE numeric tolerance for agreement of inverted values.
#'
#' @return a list with the transformation and inverse transformation functions.
#'
#' @examples
#'  out <- maeve:::check_trans_func( 'log', 'exp' )
#'  
#'  
#'
#' @author Bill Forrest \email{forrest@@gene.com}
#'
#' @keywords character
#'
#' @seealso \code{\link{character}}
#'
check_trans_func <-
  function( trans_func_char = c("log", "Identity", "sqrt",  "cube_root"), 
              inv_func_char = c("exp", "Identity", "square","cube"),
                test_func_x = 1:10,
                  TOLERANCE = 1e-6
           ){

### Set up the transformation function and its inverse:
  trans_func <- NULL
  trans_func_char <- trans_func_char[1]
  if( trans_func_char == 'Identity' ){
    trans_func <- function(x){x}
  } 
  if( trans_func_char == 'cube_root' ){
    trans_func <- function(x){x^(0.333333333333)}
  }  
  if( !is.function( trans_func ) ){
    trans_func <- try( get( trans_func_char ) )
  } 

  if( !is.function( trans_func ) )
   stop( 'error in check_trans_func(): cannot find function "', trans_func_char, '".' )

  
  inv_func <- NULL
  inv_func_char <- inv_func_char[1]  
  if( inv_func_char == 'Identity' ){
    inv_func <- function(x){x}
  } 
  if( inv_func_char == 'square' ){
    inv_func <- function(x){x*x}
  } 
  if( inv_func_char == 'cube' ){
    inv_func <- function(x){x*x*x}
  }
  if( !is.function( inv_func ) ){
    inv_func <- try( get( inv_func_char ) )
  } 

  if( !is.function( inv_func ) )
    stop( 'error in check_trans_func(): cannot find function "', inv_func_char, '".' )

  
  test_func_y = inv_func( trans_func( test_func_x ) ) # If all OK, then test_func_y == test_func_x
  inv_check <- max( abs( test_func_y - test_func_x ) )
  if( is.na( inv_check ) || (inv_check > TOLERANCE) )
    stop( 'error in check_trans_func(): ',
          'failed inversion check for functions ',
           trans_func_char,
           ' and ',
           inv_func_char,
           '.\n'
         )

  return( list( trans_func = trans_func, inv_func = inv_func ) )
  
}
