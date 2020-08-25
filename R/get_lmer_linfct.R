#' Return a matrix of linear contrasts, intended for use in multcomp::glht().
#'
#' Takes an lmerMod object from lme4::lmer() assuming a particular model formula
#' type, then finds a contrast matrix to query the slope estimates. The most
#  typical linear mixed model formula for our analyses at the time this is
#' written is 'y ~  group + group:x + ( 1 + x | ID )'.
#'
#'@param lmer_obj an lmerMod object returned from lme4::lmer().
#'@param contrast character string specifying which type of contrast is desired.
#'@param grpnames character vector with names of levels for group factor.
#'@param reference_Dunnett character string with exact match to an element of grpnames.  The exact match will be the reference group in Dunnett contrasts.
#'@param slope_ind_char character string indicating how to discern slope parameters from "names(fixef(lmer_obj))".
#'@param custom_contrast numeric matrix of contrasts for slope estimates. Column names must match exact with treatment group names.  If it is not NULL, it overrides whatever is specified in "contrast".
#'
#' @return a contrast matrix.
#'
#' @examples
#'  cat('No working example for unexported function get_lmer_linfct().')
#'
#' @author Bill Forrest \email{forrest@@gene.com}
#'
#' @keywords lme4
#'
#' @seealso \code{\link{data.frame}}
#'
get_lmer_linfct <-
  function( lmer_obj,
            contrast = c( 'Identity', 'Dunnett', 'Tukey', 'Sequen', 'custom' ),
            grpnames = NULL,
            reference_Dunnett = NULL,
            slope_ind_char = ":x",
            custom_contrast = NULL
            ){

 contrast = match.arg( contrast )
  
 SLOPE_IND <- grepl( slope_ind_char, names(lme4::fixef(lmer_obj)), fixed = TRUE )

 ## Check whether "custom_contrast" was provided. If so, then it will be used
 ## instead of "contrast".
 if( !is.null( custom_contrast ) ){

 ## Is custom_contrast a matrix?
   if( !is.matrix( custom_contrast ) ){
     stop('error in maeve:::get_lmer_linfct(): "custom_contrast" must be NULL or a matrix.')
   }
   
   if( any( is.na( match( colnames( custom_contrast ), grpnames ) ) ) ){

   ## This error means that the column names of the customized contrast matrix
   ## do not match the group names passed to the model. A common cause of this
   ## error is when the group names have been abbreviated somewhere along the
   ## way, perhaps deleting white space or needless extra letters.  To avoid it,
   ## either do not abbreviate, *or* also abbreviate the column names of the
   ## custom contrast matrix in a matching manner.
     stop_msg_string <- paste(grpnames,collapse='\n')
     stop( 'error in maeve:::get_lmer_linfct(): column names of "custom_contrast" do not match current grpnames; ',
           'group names may have been abbreviated: check "abbreviate_n" parameter; current grpnames values are:\n\n',
            stop_msg_string
          )
   } ### end of 'if( any( is.na( match( colnames( custom_contrast ), grpnames ) ) ) ){...'

   ## List with (1) row names equal to the row names provided in custom_contrast, (2) column names equal to ALL the fixed effect LME estimates
   dimnames_linfct_lmer <- list( row.names = rownames( custom_contrast ), col.names = names(lme4::fixef( lmer_obj )) )
   ## Make a matrix of zeros from the name vector lengths in "dimnames_linfct_lmer":
   linfct_lmer <- matrix( 0, nrow = length( dimnames_linfct_lmer$row.names ), ncol = length( dimnames_linfct_lmer$col.names ) )

   slope_ind_custom <- match( paste0( 'group', colnames( custom_contrast ), slope_ind_char ), names(lme4::fixef(lmer_obj)) )

   if( any( is.na( slope_ind_custom ) ) ){
       stop('error in maeve:::get_lmer_linfct(): unmatched column names in setting custom contrast')
   }

   linfct_lmer[ , slope_ind_custom] <- custom_contrast  # assign the columns measuring log-linear growth rate for each group.
   dimnames( linfct_lmer ) <- dimnames_linfct_lmer
   rm( dimnames_linfct_lmer )

   return( linfct_lmer )

 } ### end of 'if( !is.null( custom_contrast ) ){...' 


      
 
 if( contrast == 'Identity' ){
 ## "multcomp::contrMat()" does not take "Identity" as a type, so do this case separately
 ##
   dimnames_linfct_lmer <- list( row.names = grpnames, col.names = names(lme4::fixef( lmer_obj )) )
   linfct_lmer <- matrix( 0, nrow = length( dimnames_linfct_lmer$row.names ), ncol = length( dimnames_linfct_lmer$col.names ) )
   linfct_lmer[ , SLOPE_IND ] <- diag( sum( SLOPE_IND )  ) # assign the columns measuring log-linear growth rate for each group.
   dimnames( linfct_lmer ) <- dimnames_linfct_lmer
   rm( dimnames_linfct_lmer )
    
 } else{ ## if contrast != "Identity", use "multcomp::contrMat()"
          tmp   <- rep( 0, sum( SLOPE_IND ) );
          names( tmp ) <- grpnames

          ## Figure out which group is the reference for Dunnett contrast:          
          if( is.null( reference_Dunnett ) ){
            base_index <- 1
          } else{
            base_index <- match( reference_Dunnett, grpnames )
            if( is.na( base_index ) ){
              stop_msg_string <- paste( grpnames, collapse = '\n' )
              stop( 'error in maeve:::get_lmer_linfct(): ',
                     reference_Dunnett, ' ',
                    'does not match any group names provided: ',
                     stop_msg_string
                   )
            } # end of 'if( is.na( base_index ) ){...'
          } # end of 'else{...'
          
          tmp2  <- multcomp::contrMat( tmp , contrast, base = base_index )
          
          linfct_lmer <- matrix( 0, nrow = nrow( tmp2 ), ncol = length( SLOPE_IND ) )
          dimnames_linfct_lmer <- list( row.names = row.names(tmp2), col.names = names(lme4::fixef( lmer_obj )) )
          linfct_lmer[ , SLOPE_IND ] <- tmp2
          dimnames( linfct_lmer ) <- dimnames_linfct_lmer          
 } # end of 'else{ ## if contrast != "Identity", use "multcomp::contrMat()"...'

      
 return( linfct_lmer )

 } # end of function get_lmer_linfct() 
