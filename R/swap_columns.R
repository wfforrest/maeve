#' Swap positions (so names and values both) for two columns named in a data.frame.
#' 
#' Takes a data frame and two character strings.  The character strings must
#' match exactly to the column names of two columns in the data frame. The
#' returned value is the same data frame, but with the column positions of 
#' the two columns named interchanged.
#' 
#' @param XX data.frame with two or more columns.
#' @param column_name_01 character string with the name of first column.
#' @param column_name_02 character string with the name of first column.
#'
#' @return a data.frame with the two named columns interchanged.
#'
#' @examples
#'   ### avoid a ':::'-related warning in package check by not running next lines:
#'    dat <- data.frame( A = letters[1:10], X = 1:10, Y = rnorm( 10 ) )
#'    dat_swap <- maeve:::swap_columns( dat, 'X', 'Y' )
#'
#' @author Bill Forrest \email{forrest@@gene.com}
#'
#' @keywords data.frame
#'
#' @seealso \code{\link{data.frame}}
#'
swap_columns <- function(XX, column_name_01, column_name_02){
  stopifnot( is.data.frame(XX)                 &
             is.character( column_name_01 )    &
             is.character( column_name_01 )    &
            (column_name_01 %in% colnames(XX)) &
            (column_name_02 %in% colnames(XX))
            ) 
  ## utility to interchange named columns.
  ind <- match( c( column_name_01, column_name_02 ), colnames(XX) )
  XX[,ind] <- XX[ rev( ind ) ]
  colnames( XX )[ ind ] <- colnames( XX )[ rev( ind ) ]
  return(XX)
 }
