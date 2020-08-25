#' Remove any trailing ":" colon from each element of a character vector.
#' 
#' Strips out a trailing colon (if it is present) from each element of a
#' character string.  This is to deal with the apparent (but troublesome)
#' fact that glht results from "lmerMod" do not add such a colon, but results
#' from the same function call on a "gam" object do, which was causing errors
#' when they were combined.
#'
#' @param x character vector or NULL
#'
#' @return a character vector or NULL
#'
#' @examples
#'  strings_with_colons <- c("cat:", "dog",  ":")
#'  
#'  trimmed_strings <- maeve:::trim_final_colon( strings_with_colons )
#'
#' @author Bill Forrest \email{forrest@@gene.com}
#'
#' @keywords character
#'
#' @seealso \code{\link{character}}
#'
  trim_final_colon <- function(x){
    ## If x is a character string and its final character is a colon, then remove
    ## the colon and keep the rest.
    if( ! (is.character(x) | is.null(x)) ){
        stop('error in trim_final_colon(): input must be class "character" or NULL')
    }
    
    if( is.null( x ) ){
          return( NULL )
    }
    ##
    nchar_by_word <- unlist( lapply( as.list( x ), nchar ) )
    list_of_words_strsplit_by_char <- strsplit( x, '' )
    trimmed_vec <- rep( NA, length = length(x) )
    for( ii in 1:length( x ) ){
      if( list_of_words_strsplit_by_char[[ii]][ nchar_by_word[ii] ] == ':' ){
        trimmed_vec[ii] <-
          paste( list_of_words_strsplit_by_char[[ii]][-nchar_by_word[ii]], collapse = '' )
      } else{
        trimmed_vec[ii] <-
          paste( list_of_words_strsplit_by_char[[ii]],                     collapse = '' )
      }
    }
    return( trimmed_vec )
  }
