#' Return x-range for a given group
#'
#' Takes a data frame with groups and a continuous abscissa, then returns the abscissa range for a given group.
#'
#' @param dat a data.frame including a numeric column and a factor column
#' @param x_name character string with name of numeric column of interest
#' @param group_name character string with name of factor column of interest
#' @param reference_Dunnett character string with exact match to an element of grpnames.  The exact match will be the reference group in Dunnett contrasts.
#'
#' @return a vector with two numbers giving the range of interest for dat[,x_name].
#'
#' @examples
#'  test_dat <- expand.grid( x = 0:3, group = c('group_1','group_2') )
#'  example_xrange <- maeve:::get_xrange( test_dat, 'x', 'group' )
#'
#' @author Bill Forrest \email{forrest@@gene.com}
#'
#' @keywords character
#'
#' @seealso \code{\link{data.frame}}
#'
get_xrange <- function( dat,
                        x_name            = maeve_options('x_name'),     ## 'DAY_OF_STUDY',
                        group_name        = maeve_options('group_name'), ## 'group_name',
                        reference_Dunnett = maeve_options('reference_Dunnett')
                       ){

   ind_row <- as.character( dat[,group_name] ) %in% reference_Dunnett
   ind_col <- colnames( dat ) == x_name

   return( range( dat[ ind_row, ind_col ] ) )

}
