#' swap each occurrence of a factor with a level-specific number ("fraction")
#'
#' A factor is provided in the 1st argument.  By default (i.e., "frac =  NULL")
#' equally-spaced fractions descending from 1 to 0 are returned with the number
#' of distinct fractions equal to the number of levels of the factor (e.g., a
#' three-level factor will have distinct values {1, 0.5, 0} returned). If there
#' is only one level in "fact", a vector of "1" values is returned.  If a numeric
#' vector "frac" with length equal to the number of levels in "fact" is provided,
#' then the ordered values of "frac" are substituted in for the matching values
#' of "fact" (see the example).
#'
#' @author Bill Forrest <forrest@@gene.com>
#'
#' @param fact a factor
#' @param frac a numeric vector with length equal to the number of levels of fact. 
#'
#' @return a numeric vector with each instance of "fact" swapped out for its matching "frac" component.
#'
#' @examples
#'  animal <- factor(c('ant','bird','cat','dog','emu','cat','bird'))
#'  animal_score <- c(1, 0.6, 0.2, 0.1, 0)
#'  maeve:::fact_to_frac( animal )
#'  maeve:::fact_to_frac( animal, animal_score )
#'
#' @author Bill Forrest \email{forrest@@gene.com}
#'
#' @references \url{www.r-project.org}
#'
#' @keywords factor
#'
fact_to_frac <- function( fact, frac = NULL ){
   
    if(!is.factor(fact)){
      stop("error in maeve:::fact_to_frac(): input must be a factor")
    }

    if( !is.null(frac) && !is.numeric(frac) ){
      stop("error in maeve:::fact_to_frac(): User-entered fraction must be numeric")
    }
    
    if( !is.null(frac) && length( frac ) != nlevels( fact ) ){
      stop('error in maeve:::fact_to_frac(): length of "frac" must equal number of levels in "fact"')
    }
    
    if( nlevels( fact ) == 1 ){
      if( is.null( frac ) ){
         out <- frac
       } else{
         out <- rep( frac, length( fact ) )
       }
    }

    if( nlevels( fact ) > 1 ){
      if( is.null( frac ) ){
        out <-  ( nlevels( fact ) - as.numeric( fact ) ) / ( nlevels( fact ) - 1 ) 
      } else{
        out <- frac[ as.numeric( fact ) ]
      }
    }
    
    return( out )
 }
