#' Impute / copy over missing covariate values in a data.frame
#'
#' @author Bill Forrest <forrest@@gene.com>
#'
#' @param XX data.frame with two columns to be copied one from another
#' @param colname_01 character name of column with possibly NA values.
#' @param colname_02 character name of column with possibly NA values.
#' @param stop_if_both_missing logical whether to stop() if both entries are missing.
#'
#' @return An R data.frame with identical columns under colname_01 & colname_02.
#'
#' @examples
#'  test_levels <- c( 'one', 'two', 'three', 'four' )
#'  DF_with_NA <-
#'   data.frame( First  = factor( c(    NA, 'two', NA, 'four' ), levels = test_levels ),
#'               Second = factor( c( NA,  NA, 'three', NA ), levels = test_levels ),
#'               Third = factor( test_levels, levels = test_levels )
#'              )
#'  DF_imputed_12 <- maeve:::impute_NA_covariate( DF_with_NA, 'First', 'Second',
#'                                                stop_if_both_missing = FALSE )
#'  DF_imputed_13 <- maeve:::impute_NA_covariate( DF_with_NA, 'First', 'Third'  )
#'
#' @author Bill Forrest \email{forrest@@gene.com}
#'
#' @references \url{www.r-project.org}
#'
#' @keywords data.frame
#'
impute_NA_covariate <- function(XX, colname_01, colname_02, stop_if_both_missing = TRUE ){
          ##
          stopifnot( is.data.frame(XX) &
                     ##
                     is.character( colname_01 ) &
                     colname_01 %in% colnames( XX ) &
                     ##
                     is.character( colname_02 ) &
                     colname_02 %in% colnames( XX )
                    )

          Col_01 <- XX[ , colname_01 ]; NA_01 <- is.na( Col_01 )
          Col_02 <- XX[ , colname_02 ]; NA_02 <- is.na( Col_02 )

          stopifnot( (is.factor(  Col_01 ) & is.factor(  Col_02 ) & identical( levels( Col_01 ), levels( Col_02 ) )) | # both factors, with the same levels.
                     (is.numeric( Col_01 ) & is.numeric( Col_02 )) | # both numeric.
                     (is.logical( Col_01 ) & is.logical( Col_02 ))   # both logical.
                    )

          both_missing          <-   NA_01 &   NA_02
          both_present          <- ! NA_01 & ! NA_02
          missing_01_present_02 <-   NA_01 & ! NA_02
          present_01_missing_02 <- ! NA_01 &   NA_02

          
          if( stop_if_both_missing & any( both_missing ) ){
            ### Case with both entries missing.  This should never happen.
            stop(  'error in maeve:::impute_NA_covariate(): at least one row was found for which both of ',
                    colname_01,
                   ' and ',
                    colname_02,
                   ' are missing'
                 )
          } # end of 'if( any( both_missing ) ){...'

      
          if( any( both_present ) ){
            ### Case with both entries present.  Confirm that they identical for this section:
            if( ! identical( Col_01[ both_present ], Col_02[ both_present ]  ) ){
            ##  
            disagree <- both_present & XX[,colname_01] != XX[,colname_02] ## rows with both, but they disagree.
            disagree <- disagree & !is.na( disagree )
            ##
            print( XX[ disagree, c( colname_01, colname_02 ) ] )
            stop( 'error in maeve:::impute_NA_covariate(): at least one row was found for which ',
                   colname_01,
                  ' and ',
                   colname_02,
                  ' are both present but different'
                 )
             }
             
          } # end of 'if( any( both_present ) ){...'


          if( any( missing_01_present_02 ) ){
          ### Case with first missing, second present
            XX[ missing_01_present_02, colname_01 ] <- XX[ missing_01_present_02, colname_02 ]
          } # end of 'if( any( missing_01_present_02 ) ){...'

          if( any( present_01_missing_02 ) ){
          ### Case with first present, second missing
            XX[ present_01_missing_02, colname_01 ] -> XX[ present_01_missing_02, colname_02 ]
          } # end of 'if( any( present_01_missing_02 ) ){...'


          ### Final check:
          stopifnot( identical( XX[, colname_01], XX[, colname_02] ) )
          
          return(XX)
          
        } # end of 'impute_NA_covariate <- function(XX, colname_01, colname_02 ){...'
