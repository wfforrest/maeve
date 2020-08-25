#' Add a leading character to equalize string lengths
#'
#' @param x          Vector of character, numeric, or factor values to pad.  Will be converted immediately to a character vector.
#' @param padChar    character to add to the front of values of 'x'.  Should be of length 1, or unexpected behavior may result.
#' @param padExtra   nonnegative integer of how many extra padChar characters to add to the front of values of 'x'.
#'
#' @return character string
#'
#' @examples
#' lead_char( 1:10 )
#' lead_char( 1:10, padChar = '_' )
#' data.frame( animal = lead_char( c('cat','alligator', 'heron'), padChar = '_' ) )
#'
#' @author Bill Forrest \email{forrest@@gene.com}
#'
#' @keywords character
#'
#' @seealso \code{\link{mapply}}
#' @export
#'
lead_char <- function( x, padChar = '0', padExtra = 0 ){
    
           charVec <- as.character( x ) # convert to a character vector
    
           n <- nchar(charVec) # number characters in each string of 'charVec'
    
           if( padExtra != abs(round(padExtra) ) ){
               stop('error in maeve::lead_char(): padExtra must be a nonnegative integer')
           }
           #
           mapply( function(a,b) # add 'padChar' leading characters to equalize lengths:
                     paste0( paste(rep(padChar, a), collapse=''), b ),
                   a = as.list( padExtra + max(n) - n ), # number of characters to add to i'th entry
                   b = as.list( charVec )     # i'th entry
                  )
}
