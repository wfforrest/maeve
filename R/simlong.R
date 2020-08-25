#' Simulate longitudinal data on an equally spaced grid.
#'
#' Allows simulation of a user-provided function on an equally spaced grid.
#' Options for multiple groups and multiple subjects per group. Options for random
#' dropout of observations, either binomially, geometric dropout, or a process
#' based on an endpoint-driven hazard function so that once an observation is missed,
#' that subject has no more observations.  Also allows for linear random effects
#' (random intecept and slope).
#' 
#' @author Bill Forrest <forrest@@gene.com>
#'
#' @param grid_length integer number of equally-spaced points at which each subject is evaluated at the function.
#' @param number_groups integer number of groups.               
#' @param subjects_per_group integer number of subjects per group.
#' @param SD_intercept numeric Gaussian SD for random intercept term
#' @param SD_slope numeric Gaussian SD for random slope term
#' @param SD_epsilon numeric Gaussian SD for random within-subject "noise" term attached to each measurement.
#' @param xmin numeric starting point of grid at which to evaluate the function for each subject.
#' @param xmax numeric ending point of grid at which to evaluate the function for each subject.
#' @param dropout_scheme character string specifying which dropout scheme is to be used.
#' @param binom_retention_prob numeric probability of retaining an observation.  Set between 0 and 1 for simple Bernoulli random thinning.
#' @param geom_dropout_prob numeric geometric dropout probability in (0,1).  Probability for subject to drop out at each post-baseline step. Set to non-NULL to override binomial thinning.
#' @param size_based_dropout logical whether to determine dropout probability from current endpoint value.
#' @param dropout_map data.frame mapping endpoint values to dropout probability values that can be used to interpolate dropout probability.
#' @param force_dropout logical whether to force a dropout when the endpoint exceeds the upper end of the dropout map phenotype range.
#' @param with_dropout_annot logical whether to skip filtering the data.frame and return everything along with columns to perform dropout filtering manually.
#' @param response_func function to evaluate for each grid point, for each subject
#'
#' @return data.frame with simulated data set.
#'
#' @examples
#' simlong_example_00  = simlong( number_groups = 1, subjects_per_group = 1,
#'                                response_func = function( x, ... ) { 1 + x } )
#'
#' simlong_example_01  = simlong( grid_length = 25, number_groups = 2, subjects_per_group = 3,
#'                                SD_intercept = .1, SD_slope = .1, SD_epsilon = .01, 
#'                                response_func = function( x, group ) { 
#'                                                    1 + x * scale( as.numeric( group ) )
#'                                                                     }   
#'                               )
#'  
#' @author Bill Forrest \email{forrest@@gene.com}
#'
#' @references \url{www.r-project.org}
#'
#' @keywords function
#' @export
#'
simlong <- function( grid_length          = 11,
                     number_groups        =  3,                    
                     subjects_per_group   =  5,
                     SD_intercept         = .0001,
                     SD_slope             = .0001,
                     SD_epsilon           = .0001,
                     xmin                 = 0,
                     xmax                 = 1,
                     ##
                     dropout_scheme       = c( 'binomial', 'geometric', 'endpoint_dependent' ),
                     binom_retention_prob = 1,
                     geom_dropout_prob    = NULL,
                     size_based_dropout   = TRUE,
                     dropout_map          = NULL,
                     force_dropout        = TRUE,
                     with_dropout_annot   = FALSE,
                     ##
                     response_func = function( x, ... ){ 6 * x * (1-x) }
                    ){

  dropout_scheme = match.arg( dropout_scheme )
    
  ## create numeric grid over which to evaluate the function:  
  x <- seq( xmin, xmax, length = grid_length )

  number_subjects = number_groups * subjects_per_group
 
  ## create subject identifiers
  ID <- paste0( 'id_', (maeve::lead_char)(1:number_subjects, padExtra = 1) );
  ID <- factor( ID, levels = ID )
 
  ## create group identifiers
  group <- paste0( 'group_', (maeve::lead_char)(1:number_groups, padExtra = 1) );
  group <- factor( group, levels = group )
 
   X <- expand.grid( ID = ID, x = x ) %>% dplyr::arrange( ID, x )

  ## Need to add the groups so that near-equal numbers of IDs
  ## are assigned to each group:
  X <- data.frame( group = factor( rep( levels( group ),
                                   each = grid_length * subjects_per_group
                                  ), ###nrow( X ) / nlevels( group ) ),
                                   levels = levels( group ) ),
                    X
                  ) %>% dplyr::arrange( group, ID, x )

  X <- X %>% dplyr::mutate(
                    fx = response_func( x, group ),
                    random_intercept = rnorm( number_subjects, 0, SD_intercept ) %>% rep( each = grid_length ),
                    random_slope     = rnorm( number_subjects, 0, SD_slope     ) %>% rep( each = grid_length ),
                    fx_ID = fx + random_intercept + random_slope * x,
                    y = fx_ID + rnorm( length(fx), 0, SD_epsilon )
                   )

    
  ## In the next sections, randomly discard some fraction of the
  ## data on each subject.  There are three possible "thinning"
  ## mechanisms:

    
  ## (1) By default, randomly (in a Binomial( 1, p = binom_retention_prob ) sense) retain
  ##     each observation.  The default is "binom_retention_prob = 1" --> no thinning.
  if( dropout_scheme == 'binomial' ){
        
        rows_to_return <- as.logical( rbinom( nrow( X ), 1, binom_retention_prob ) ) # for very basic random thinning
        
  } # end of 'if( dropout_scheme == 'binomial' ){...'


  ## (2) Keep each subject for a geometric number of time points.    
  if( dropout_scheme == 'geometric' ){
        
  ## drop out after each time point with a geometric probability  
  ##
   if ( geom_dropout_prob <= 1 | geom_dropout_prob >= 1e-8 ){ ### NB: "geom_dropout_prob ~= 0" is numerically unstable.
     time_points_per_subject <- rgeom( number_subjects, geom_dropout_prob ) %>%
                                sapply( function(x) min( x, grid_length ) ) %>%
                                sapply( function(x) max( x,           1 ) )
   } else{
     stop('error: in simlong(): "geom_dropout_prob" must be in  [1e-8, 1]'  ) 
   } 
    
  ## Now, we want to return the first n_i observations for each subject,
  ## where "n_i" is the i'th component of time_points_per_subject.
   rows_to_return <- time_points_per_subject %>%
                     as.list %>%
                     lapply( function(n) 1:n ) %>%
                     lapply( function(nvec) (1:grid_length) %in% nvec ) %>%
                     unlist
   
  } # end of 'if( dropout_scheme == 'geometric' ){...'  


  ## (3) Drop out with higher probability if the endpoint value gets higher.
  ##     Note that the 'dropout_map' in the unexported function prob_of_instant_dropout()
  ##     determines the hazard of dropping out just after any give time point.
  ##     This hazard is scaled by the interval length following the current timepoint
  ##     and the chance of a dropout following the i'th observation is defined
  ##     as "1 - exp( hazard_i * dx_i )".  See the "prob_of_instant_dropout()"
  ##     function for details.
    
  if( dropout_scheme == 'endpoint_dependent' ){

      ## The 'dropout' vector is binary and has length equal to the number or rows in 'X'.
      ## It will have '1' for each observation that would be the last one for that subject.
      ## Below, we identify the 'x' value for the *earliest* 'dropout == 1' for each subject.

      ## Identify the 'x' value for the earliest 'dropout == 1' for each subject.
      X %<>%
        plyr::ddply( .var = 'ID',
                     .fun = function( YY, dm = dropout_map, force_drop = force_dropout ){
                               ##                       
                               if( !is.null( dm ) ){
                                    dropout <- rbinom( nrow( YY ), 1, prob_of_instant_dropout( YY$x, YY$y, dropout_hazard_map = dm ) )
                               } else{ # use default hazard map -- caveat emptor.
                                    dropout <- rbinom( nrow( YY ), 1, prob_of_instant_dropout( YY$x, YY$y     ) )
                               }
                         
                               if( force_drop ){
                                    ## if an observation exceeds the upper limit of the dropout map phenotype, force a dropout.
                                    dropout[ YY$y >= max( dm$y ) ] <- 1
                               }
                               ##                       
                               YY <- data.frame( YY, dropout = dropout, rows_to_keep = NA )
                               ##
                               dropout_indices <- which( YY$dropout == 1 ) # if a '1', then no observations *after* this.
                               ##
                               if( length( dropout_indices ) == 0 ){
                                 YY$rows_to_keep <- rep( TRUE, nrow( YY ) ) # no indices were dropped for this subject.
                               } else{
                                 YY$rows_to_keep <- 1:nrow(YY) %in% 1:min(dropout_indices) # keep until just after first dropout index.
                               }
                               return(YY)
                        }
                    ) # end of "plyr::ddply( .var = 'ID',..."

      rows_to_return <- X$rows_to_keep
      
  } ## end of 'if( dropout_scheme == 'endpoint_dependent' ){...'

    
  ## 'rows_to_return' is a logical vector determined by one of the
  ## three dropout methods above. Returned data.frame can be
  ## pre-filtered or not:    
  if( ! with_dropout_annot ){
        
    X %<>% dplyr::filter( rows_to_return )

    ## Remove auxiliary columns from the "dropout_scheme == 'endpoint_dependent'" approach:
    if( 'dropout'      %in% colnames( X ) ){
          X %<>% dplyr::select( -.data$dropout      )
    }
    if( 'rows_to_keep' %in% colnames( X ) ){
          X %<>% dplyr::select( -.data$rows_to_keep )
    }
      
    return( X )
     
  } else{ ## alternative to 'if( ! with_dropout_annot ){...'
      
      return( X ) # no filtering, keep 'dropout' and 'rows_to_keep' columns.
      
  }
    
} # end of 'simlong <- function(...){...'

