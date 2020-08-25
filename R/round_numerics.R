#' Round all numeric columns in a data frame to a given number of decimal places.
#'
#' Takes a data frame and looks at each column.  If the column is _not_ a numeric,
#' it will be returned unchanged.  If the column is a numeric, it will be replaced
#' with a numeric rounded to the number of decimal places specified in the argument.
#' The function rounds all numerics except those named in the "no_round" argument.
#' This is a adapted from the pre-existing function freeze_factor_levels().
#'
#' @param x a data frame.
#' @param digits numeric number of decimal places to which to round.
#' @param no_round character vector.  Columns of "x" named in this vector will be returned unchanged.
#'
#' @return data frame of the same size, names, and types as "x".  Numerics may have changed.
#'
#' @examples
#'  myDat <- data.frame(
#'              animal = factor( c('cat', 'dog', 'cat', 'bird', 'fish', 'dog', 'cat'),
#'                               levels = c('bird','cat','dog','fish')
#'                             ),
#'              covering = factor( c('hair','hair', 'hair', 'feathers', 'scales', 'hair', 'hair'),
#'                                 levels = c('feathers','hair','scales')
#'              ),
#'              size = c(4.12, 4.123, 4.1234, 4.12345, 4.123456, 4.1234567, 4.12345678 ),
#'              mass = c(9.12, 9.123, 9.1234, 9.12345, 9.123456, 9.1234567, 9.12345678 )
#'           ) # end of data.frame 'myDat'
#'
#' print( myDat )
#'
#' ### round numeric columns of a data.frame
#' myNewDat <- round_numerics( myDat, digits = 3 )
#' print( myNewDat )
#'
#' ### Round one but not the other.
#' myNewDat2 <- round_numerics( myDat, digits = 3, no_round = 'mass' )
#' print( myNewDat2 )
#'
#' ### Round to a different number of digits
#' myNewDat3 <- round_numerics( myDat, digits = 4, no_round = c('mass') )
#' print( myNewDat3 )
#'
#' @author Bill Forrest \email{forrest@@gene.com}
#'
#' @keywords numeric
#'
#' @seealso \code{\link{factor}}
#' @export
#'
round_numerics <- function(x, digits = 12, no_round = NULL){
      ## look at columns in a data.frame.  If the column is of class 'numeric', then
      ## round the column to 'digits' decimal places.
      ## Example of intended use: If we're joining two data.frames and one of the join
      ## columns is a numeric.  Extraneous precision (e.g., at the 15th decimal place)
      ## can result in extra rows to accomodate the "different" x-values, which are not
      ## actually (or intended to be) different.
  
      if( !is.data.frame(x) ){
        stop( 'error: maeve::round_numerics() requires a data frame as input' )
      }

      ## If you're willing to give up "no_round" entirely, the rest of the function can be accomplished by:
      ##
      ##   data.frame(lapply( x, function(y,d=digits){ if( is.numeric(y) ){ round( y, d ) } else{y}} ) )
      
      ## Make a list of the column names of the input data.frame "x".
      ## Over this list of column names, apply the following function:
      ##
      ## (1) If the column name references a column with a numeric in the data frame
      ##     and that column name is _NOT_ in the "no_round" character vector,
      ##     then round that column to "digits" decimal places.
      ##
      ## (2) Otherwise, the column name references a non-numeric _or_ a numeric named in
      ##     "no_round".  In either case, just return the column as it was.

      out <- # Store interim output; need to re-assign variable names after this step.
         lapply( as.list( names( x ) ),
                 function( y, XX = x, d = digits ){
                  if( is.numeric( XX[,y] ) & !( y %in% no_round ) ){
                       round( XX[,y], d ) # round the numeric to the function-specified # decimal places.
                   } else{
                       XX[,y] # no action -- return the column as it is.
                   }
                }
               ) # end of 'lapply(...'

     names( out ) <- colnames( x ) # "out" is a list at the moment, not a data.frame
      
     return( data.frame( out ) )

     }
