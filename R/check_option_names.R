#' Check optional functional argument parameter names.
#'
#' Check a list of optional "..." functional argument parameter names to confirm that they are all present in maeve_options().  If any are not, it may indicate, e.g., typos.  Take action depending on the value of maeve_options('mismatch_action').
#'
#' @param ... optional parameters passed to a function.
#'
#' @return a vector the same length as the input vector, with either logical or numeric entries.
#'
#' @examples
#'    cat('no example for check_option_names().')
#'
#' @author Bill Forrest \email{forrest@@gene.com}
#'
#' @keywords setdiff
#'
#' @seealso \code{\link{setdiff}}
#'
check_option_names <- function( ... ){

    not_matched <- base::setdiff( names( list(...) ), names( maeve_options() ) )
  
    if( length( not_matched ) == 0 ){
        return(NULL)
      } else{ # at least one optional argument not found in maeve_options().  What to do?
        not_matched_message <- paste('Following optional parameters not matched in maeve_options():', paste( not_matched, collapse = '\n' ), '\n' )
        switch( maeve_options('mismatch_action'),
               'none' = { return(NULL) },
               'warn' = { base::warning( 'warning:',  not_matched_message ) },
               'stop' = { base::stop(    'stopping:', not_matched_message ) }
               ) 
      } # end of 'else{...'

} # end of 'check_option_names <- function( ... ){...'
