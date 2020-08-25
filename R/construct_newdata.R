#' Construct a data.frame from which to predict model values.
#'
#' @author Bill Forrest <forrest@@gene.com>
#'
#' @param clean_DF_restricted data frame with data restricted to those used in fitting the models.  Most often set to 'clean_DF_restricted' data.frame returned from maeve::model_study().
#' @param x_pred_type          character.      See ?maeve_options().
#' @param x_pred_vec           numeric vector. See ?maeve_options().
#' @param x_pred_interior_grid logical.        See ?maeve_options().
#' @param x_pred_spacing       numeric.        See ?maeve_options().
#' @param include_newdata_ID   logical.        See ?maeve_options().
#'
#' @return An R data.frame 
#'
#' @examples
#'  require( magrittr ) # want pipe operator "%>%"
#'  ##
#'  vismo21 <- 
#'    vismodegib %>%
#'    dplyr::filter( DAY_OF_STUDY <= 21 ) %>%
#'    dplyr::rename( group = group_name, ID = animalID, x = DAY_OF_STUDY ) # internal names
#'  ## pass this to the construct_newdata() function.
#'  vismo21_grid <-   
#'    vismo21 %>% # make, e.g., an even grid with one-day resolution:
#'    maeve:::construct_newdata( x_pred_type = 'grid', x_pred_spacing = 1 )
#'
#' @author Bill Forrest \email{forrest@@gene.com}
#'
#' @references \url{www.r-project.org}
#'
#' @keywords data.frame
#'  
construct_newdata <-
  function( clean_DF_restricted,
            ##
            x_pred_type          = maeve_options("x_pred_type"),
            x_pred_vec           = maeve_options("x_pred_vec"),
            x_pred_interior_grid = maeve_options("x_pred_interior_grid"),
            x_pred_spacing       = maeve_options("x_pred_spacing"),
            include_newdata_ID   = maeve_options("include_newdata_ID")
            ##
           ){

      ## If x_pred_type = 'observed', we use just the observed x-values for
      ## each group + ID combination.  This is the original (and default)
      ## behavior, and we take care of it first, then proceed to alternative.
      
      if( x_pred_type == 'observed' ){
      ## Just keep the x-values observed for each group and ID in actual study:
       if( include_newdata_ID ){
         newdata <- dplyr::select( clean_DF_restricted, group, ID, x ) 
       } else{
         newdata <- dplyr::select( clean_DF_restricted, group,     x ) 
       }

       return( newdata ) # leave function here if x_pred_type == 'observed'
       
      }


      
      ## Interlude: Do some error checks for x_pred_vec:      
      stopifnot( is.null( x_pred_vec ) | is.numeric( x_pred_vec ) )

      if( x_pred_type %in% c( 'custom', 'union_observed_and_custom', 'union_custom_and_grid' ) & is.null( x_pred_vec ) ){
        stop( 'error in maeve:construct_newdata(): Currently x_pred_vec is NULL; ',
              'if x_pred_type includes "custom", then x_pred_vec must be a numeric vector.'
             )
      }

      
      ## The options below are used only if x_pred_type != 'observed'. The different
      ## options imply different vectors of "x_pred_values", the points at which
      ## predictions will take place.  We first create some quantities used to 
      ## determine the "newdata" data.frame in any of the cases, then create
      ## the x_pred_values needed in the switch() statement below.
      
      ## Use the unique "group_name" and "subject_ID" combinations for the other options:
      non_x_DF <- dplyr::select( clean_DF_restricted, group, ID ) %>% unique

      ## Find the observed range of x-values for the study:
      xmin <- clean_DF_restricted %>% dplyr::select( x ) %>% min( na.rm = TRUE )
      xmax <- clean_DF_restricted %>% dplyr::select( x ) %>% max( na.rm = TRUE )

      ## Make an evenly-spaced grid.  If 'x_pred_interior_grid = TRUE', then set the
      ## end-points of the grid to be the widest integers within the range.
      if( x_pred_interior_grid ){
          x_grid <- seq( ceiling( xmin ), floor( xmax ), by = x_pred_spacing )
      } else{
          x_grid <- seq( xmin, xmax, by = x_pred_spacing )
      }

      ## Make a vector of all the observed x-values.  Note that these may well
      ## fall on a numeric grid (if, e.g., the observed values are days, as is usual),
      ## but are not required to do so.
      x_observed <- clean_DF_restricted %>% dplyr::select( x ) %>% unlist %>% unique %>% sort

      ## Determine which x-values to use based on the value of "x_pred_type":
      x_values <-
      switch( x_pred_type,
              grid                      = x_grid,
              union_observed_and_grid   = ( union( x_observed, x_grid     ) %>% sort ),
              custom                    = x_pred_vec,
              union_observed_and_custom = ( union( x_observed, x_pred_vec ) %>% sort ),
              union_custom_and_grid     = ( union( x_pred_vec, x_grid     ) %>% sort ),
              ##
              stop( 'error in maeve::construct_newdata(): x_pred_type is mis-specified' )
            )
               

      ## For each *unique* combination of group & ID, affix the grid of x-values.
      newdata <-
      non_x_DF %>%
      plyr::ddply(
             .var = c( 'group', 'ID' ),
             .fun = function( XX, x_pred_grid = x_values ){
                        data.frame( group = XX$group, ID = XX$ID, x = x_pred_grid )                        
                    }                                             
                  ) # end of 'plyr::ddply(...'


      ## Take out subject identifer columnf if desired.
      if( !include_newdata_ID ) {
        newdata %<>% dplyr::select( -ID )
      } 
      
      return( newdata )
      
    } ## end of 'construct_newdata <- function(...){...'
