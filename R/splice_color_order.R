#' splice color ordering factor to rotate across groups.
#'
#' Takes equal-length factors for group membership and ID value (nested within group membership)
#' and returns a new factor ordering more evenly across groups.  E.g., for "K" groups,
#' the first level is the first ID in group #1, second level is the first ID in group #2,
#' ..., the K'th level is the first ID in group #K, then the (K+1)st level is the second
#' ID in group #1, and so on.  The logic of the algorithm assumes that 
#'
#'    identical( dat, dat[ order( group, ID ),] )
#'
#' is TRUE, since this is the use case within which it was developed, 
#' and otherwise it's ambiguous what a user would expect. The function
#' will still run if this condition fails, but a warning is issued and 
#' results are unpredictable.
#'
#' @param group     factor denoting group membership.
#' @param ID        factor denoting identification. Levels must be nested within group (i.e., an ID cannot be listed in 2+ different groups).
#' @param as_factor logical return the result as a factor (the default) or, as an alternative, return the whole data frame.
#'
#' @return a factor
#'
#' @examples
#'  my_group <- factor( rep(paste('group',lead_char(1:3),sep = '_'),each = 4) )
#'  my_ID    <- factor( paste('ID',lead_char(1:12), sep = '_') )
#'  my_color_order <- splice_color_order ( my_group, my_ID )
#'
#' @author Bill Forrest \email{forrest@@gene.com}
#'
#' @keywords factor
#'
#' @seealso \code{\link{factor}}
#' @export
#'
splice_color_order <-
  function( group, ID, as_factor = TRUE ){

    stopifnot( is.factor( group ), is.factor( ID ), length( group ) == length( ID ), is.logical( as_factor ) )

    dat <- data.frame( group, ID )

    ## Check that each ID is nested within a unique group:
    if( nrow( unique( dat ) ) > nlevels( ID ) ){
        stop( 'error in maeve::splice_color_order(): nrow( unique( dat ) ) > nlevels( ID );',
              'probably one of the ID values is in two or more groups, which is not allowed'
             )
    }

    ## Check that groups & ID are already presented their heirarchical level orders.  If not, give a warning.
    ##        if( ! identical( dat, dplyr::arrange( dat, group, ID ) ) ){
    if( ! identical( dat, dat[ order( group, ID ),] ) ){
        warning( paste( 'WARNING in maeve::splice_color_order():\n',
                        'The "group" and "ID" factors were not heirarchically sorted when passed\n',
                        '(i.e., group in level-order, then ID in level-order, nested within group).\n',
                        'This can result in unpredictable color assignments.\n'
                      )
               )
    }

    ## Intent of next section:
    ##
    ## Within each group, generate  an equally-spaced grid of numbers on [0, 1].  So, e.g.,
    ##  "2" --> [0, 1].
    ##  "3" --> [0, 1/2, 1]
    ##  "4" --> [0, 1/3, 2/3, 1]
    ## and so on.
    ##
    ## By convention, we set
    ## "1" --> [ 0 ].
    dat_unique <-
        dat    %>%
        unique %>%
        plyr::ddply( .var = 'group',
                     .fun = function(X){
                             XX <- droplevels( X )
                             if( nlevels( XX$ID ) == 1 ){
                               return( cbind( XX, in_group_color_fraction = 0 ) )
                             } else {
                               return( cbind( XX, in_group_color_fraction = seq( 0, 1, length = nlevels( XX$ID )) ) )
                             }
                            } # end of '.fun = function(X){...'
                    ) # end of 'plyr::ddply( .var = 'ID',...'

      
    ## Find the rank order of 'in_group_color_fraction' values across all ID values across groups:
    dat_unique %<>% dplyr::mutate( color_order_n = factor( maeve::lead_char( rank( in_group_color_fraction, ties.method = 'first' ) ) ) )

    ## Make a copy of the ID factor, but with levels now in the order described by 'color_order_n':
    dat_unique %<>% dplyr::mutate( color_order = factor( as.character(ID), levels = levels( ID )[order(color_order_n)] ) )

    ## Merge these unique values by group & ID with the original data:
    dat_merge <- dplyr::left_join( dat, dat_unique, by = c('group', 'ID') ) %>% dplyr::arrange( group, ID )

    ## Return the 'color_order' factor only, by default:
    if( as_factor ){

       return( dat_merge$color_order ) # send back just the new factor.

    } else {

       return( dat_merge ) # send back the entire data.frame

    }

} ## end of 'splice_color_order <- function( group, ID, as_factor = TRUE ){...'

