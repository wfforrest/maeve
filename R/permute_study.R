#' Create a data frame with group_name assignments permuted randomly with respect to the subject_ID values.
#'
#' @author Bill Forrest <forrest@@gene.com>
#'
#' @param data_frame data frame with factors for group and ID, numerics for time on study and response.
#' @param random_seed numeric passed to base::set.seed() 
#' @param group_name           character.   See ?maeve_options().
#' @param subject_ID           character.   See ?maeve_options().
#' @param x_name               character.   See ?maeve_options().
#'
#' @return A data.frame
#'
#' @examples
#'  cat('Example for permute_study() in dontrun{} code block.')
#'  \dontrun{
#'  data( vismodegib )
#'  vismo30 <- dplyr::filter( vismodegib, DAY_OF_STUDY <= 30 )
#'  vismo30 <- dplyr::mutate( vismo30, y = log( 1 + TUMOR_VOLUME ) )
#'  vismo30_permuted <- permute_study( vismo30, random_seed = 12345 )
#'  ## see outcome of permutation:
#'  fig_vismo_orig <- draw_study( vismo30,          ncol_value = 5, endpoint_name = 'y' )
#'  fig_vismo_perm <- draw_study( vismo30_permuted, ncol_value = 5, endpoint_name = 'y' )
#' }
#'
#' @author Bill Forrest \email{forrest@@gene.com}
#'
#' @references \url{www.r-project.org}
#'
#' @keywords data.frame
#' @export
#'
permute_study <-
  function( data_frame,
            random_seed = NULL,
            group_name  = maeve_options("group_name"),
            subject_ID  = maeve_options("subject_ID"),
            x_name      = maeve_options("x_name")
           ){

  if( !is.null( random_seed ) ){ set.seed( random_seed ) }
  
  ## Check that columns for group, ID, and time are in the data frame.
  ## It should probably have some other columns as well (e.g., "endpoint_name"?),
  ## in order to be useful, but that is not strictly required.
  ##    
  stopifnot( sapply( c( group_name, subject_ID, x_name ), as.character ) %in% colnames( data_frame ) )

  ## Create a manifest data.frame with exactly one row per "group & ID" combination.
  ##
  manifest <- data_frame %>% dplyr::select( !!dplyr::sym( group_name ), !!dplyr::sym( subject_ID ) ) %>% unique()

  permuted_manifest <- # Randomly assign a group label to each subject_ID
    data.frame( sample( manifest[,group_name] ), # scrambled column entries.
                        manifest[,subject_ID]    # retain original order.
               )
    
  colnames( permuted_manifest ) <- c( group_name, subject_ID )

  ## Make a copy of the original data.frame *without* the group_name column.
  unassigned <- data_frame %>% dplyr::select( -dplyr::one_of( group_name ) )

  permuted_data_frame <-
    permuted_manifest %>%
    dplyr::left_join( unassigned, by = eval( subject_ID ) ) %>%
    dplyr::arrange( !!dplyr::sym( group_name ), !!dplyr::sym( subject_ID ), !!dplyr::sym( x_name ) ) %>%
    maeve::freeze_factor_levels()

  ## confirm that we have the same columns we started with:
  stopifnot( all( match( colnames( data_frame ), colnames( permuted_data_frame ) ) ) &
             ncol( data_frame ) == ncol( permuted_data_frame ) & # same number of columns?
             nrow( data_frame ) == nrow( permuted_data_frame )   # same number of rows?
            )

  ## ensure that the *columns* are all in the original left-to-right order following the left_join() above:
  permuted_data_frame <- permuted_data_frame[, colnames( data_frame ) ]
      
  return( permuted_data_frame )

} ### end of "permute_study <- function(...){..."
