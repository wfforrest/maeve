#' Normalize a time-indexed vector of values its first value.
#'
#' From a data.frame, extract a y-vector of responses and their x-vector of 
#' observation times.  Normalize the y-vector to its first (based on the
#' x-values) observation, subtract out the centering value from the entire
#' normalized vector, and return the normalized, centered vector.
#'
#' @author Bill Forrest <forrest@@gene.com>
#'
#' @param DATA_FRAME data.frame with x-values (i.e, time-values) and y-values (responses.
#' @param x_name character string with name of time value that order the responses.
#' @param y_name character string with name of responses to be normalized to their own temporal baseline value.
#' @param average_multiple_obs logical should we allow averaging of multiple baseline values, or call an error?
#' @param centering_value numeric value to subtract from the normalized vector.
#' @param scale_value numeric value by which to re-scale the normalized, centered vector.
#'
#' @return A numeric vector with normalized values.
#'
#' @examples
#' normalizing_test_data <- data.frame( x = seq(0,1,length = 11) )
#' normalizing_test_data$y <- 1 + 2 * exp(-normalizing_test_data$x)
#' normalized_output <-
#'   data.frame( normalizing_test_data,
#'               y_norm = maeve:::normalize_to_first_timepoint( normalizing_test_data )
#'              )
#'
#' @author Bill Forrest \email{forrest@@gene.com}
#'
#' @references \url{www.r-project.org}
#'
#' @keywords data.frame
#'
 normalize_to_first_timepoint <-
   function( DATA_FRAME,
             x_name = 'x',
             y_name = 'y',
             average_multiple_obs = TRUE,
             centering_value = 1,
             scale_value = 100
            ){
     y_values <- DATA_FRAME[ , y_name ]
     is_y_not_na <- !is.na(y_values)
     
     if (!any(is_y_not_na)){
       return(NA)
     }
     
     x_values <- DATA_FRAME[ , x_name ]
     
     ###
     y_baseline <- y_values[ x_values == min(x_values[is_y_not_na]) ]
     if( length( y_baseline ) > 1 && !average_multiple_obs ){
       print( DATA_FRAME )
       stop('error in norm_to_first_timepoint(): multiple baseline observations not allowed')
     } else{ y_baseline <- mean( y_baseline, na.rm = TRUE )  }

     y_normalized <- (  ( y_values / y_baseline ) - centering_value ) * scale_value

     return( y_normalized )
     
   } # end of "normalize_to_first_timepoint()"
