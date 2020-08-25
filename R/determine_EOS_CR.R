#' Determine whether each identifier has a complete response (CR) at the end of study (EOS).
#'
#' @author Bill Forrest <forrest@@gene.com>
#'
#' @param YY data.frame with times and original-scale tumor burdens for each of several identifiers.
#' @param min_val numeric threshold below which a tumor size is classified as a Complete Response (CR) if it is at End of Study (EOS).
#'
#' @return A numeric sum total of Complete Response cases recorded at End of Study in the group.
#'
#' @examples
#'  cat('Currently no working example for unexported function determine_EOS_CR().')
#'
#' @author Bill Forrest \email{forrest@@gene.com}
#'
#' @references \url{www.r-project.org}
#'
#' @keywords data.frame
#'
    determine_EOS_CR <- function( YY, min_val = 0 ){

      ## optionally trim data to a specified first day
      first_day <- maeve_options( "summary_first_day" )
      if( !is.null( first_day ) ){
        YY %<>%
          dplyr::filter( x >= first_day )
      }
      
      tmp <- plyr::ddply(
                      YY,
                     .var = 'ID',
                     .fun = function(YYY){ 
                                           if (all(is.na(YYY$y_orig))){
                                             data.frame(ID = factor(YYY$ID[1]),
                                                        CR = FALSE
                                                        )
                                           } else {
                                             YYY <- YYY[!is.na(YYY$y_orig),]
                                             data.frame(ID = factor(unique(as.character(YYY$ID))),
                                                        CR =  as.logical( YYY$y_orig[YYY$x == max( YYY$x )] <= min_val )
                                                        )
                                           }
                                         }
                     )
         return( sum( tmp$CR ) )
      } # end of 'determine_EOS_CR <- function(...'
