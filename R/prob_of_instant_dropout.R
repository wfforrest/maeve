#' Compute probability of dropout just after a time point based on current endpoint value.
#'
#' Takes an input value 'y' which is an endpoint at a given time.  Returns a probability
#' interpolated based on a spline interpolating paired { y_vector_i = endpoint_i, dropout_hazard_map_i = dropout_probability_i } pairs.
#' 
#' @author Bill Forrest <forrest@@gene.com>
#'
#' @param x_vector     numeric vector of time points at which endpoint values are observed.
#' @param y_vector     numeric vector of endpoint values.
#' @param dropout_hazard_map  data.frame with the first column a set of endpoint values spanning the relevant range of endpoints, the second column dropout probabilities corresponding to the endpoint values.
#' @param MIN_endpoint numeric  lowest value of endpoint to be evaluated.
#' @param MAX_endpoint numeric highest value of endpoint to be evaluated.
#'
#' @return a numeric vector the same length as y_vector.
#'
#' @examples
#' cat('no current example for unexported function prob_of_instant_dropout.')
#' \dontrun{
#' prob <- prob_of_instant_dropout( c( 1:5, 10, 12, 20 ), c( rep(100,5), 2001, 1500, 800 ) )
#'
#' 
#' }
#' 
#' @author Bill Forrest \email{forrest@@gene.com}
#'
#' @references \url{www.r-project.org}
#'
#' @keywords interpolation
#'
prob_of_instant_dropout <-
    function(x_vector, # need inter-x interval lengths to compute cumulative hazard over that interval.
             y_vector,
             dropout_hazard_map  = data.frame( y = seq( 0, 2000, by = 200 ), drop_haz = c( rep( .025, 3 ),  seq( .10, .30, length = 7 ), log(2) ) ),
             MIN_endpoint = min( dropout_hazard_map[,1] ),
             MAX_endpoint = max( dropout_hazard_map[,1] )
             ){

        stopifnot( length( x_vector ) == length( y_vector ) )
        stopifnot( all( colnames( dropout_hazard_map ) == c('y','drop_haz') ) )
        
        if( min( diff( x_vector ) <= 0 ) ){
            stop('error in maeve:::prob_of_instant_dropout(): x-values must be unique and in ascending order')
        }
        
        pre_x_interval_length <- c( diff( x_vector ), Inf ) # final observation --> last interval is "infinite".
        
        dropout_hazard_map <- dropout_hazard_map[ order( dropout_hazard_map[, 1] ), ]
        
        f <- stats::splinefun( x =   dropout_hazard_map[,1],
                               y =   dropout_hazard_map[,2], ### y = ( dropout_hazard_map[,2] %>% pava ),
                               method = 'hyman'
                              )

        ## truncate the y-values to the range of the dropout_hazard_map values.
        y_trunc <-
            y_vector %>%
            sapply( function(x) max( x, MIN_endpoint  ) ) %>% # set values below MIN_endpoint to MIN_endpoint.
            sapply( function(x) min( x, MAX_endpoint ) )      # set values above MAX_endpoint to MAX_endpoint.

        ## Map each y-value to its matching hazard, then scale by the subsequent interval length and apply Poisson / hazard theory.
        
        haz = f( y_trunc )          # instantaneous hazard associated with the observed y-value.
        dx  = pre_x_interval_length # length of interval following the given y-value.

        ## The next line is what you get from survival analysis with a constant hazard over an interval:

        1 - exp( - haz * dx ) # prob of a dropout event in the interval "just after" an obs, before the next one.
        
    }

