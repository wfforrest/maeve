#' Extract random intercept and slope effects from lmerMod and / or gam models.
#' 
#' @param md4_lmer      lmerMod object returned from a maeve:: call to lme4::lmer()
#' @param md4_gamm4     list with 'mer' component returned from gamm4::gamm4()
#' @param md4_lmer_pwl  lmerMod object returned from a maeve by piecewise linear mixed model regression.
#' @param md4_lmer_poly lmerMod object returned from a maeve by polynomial basis mixed model regression.
#' @param levels_ID optional character vector with the ordered levels of subject identifiers.  If not provided, the function attempts to get these levels from the models.
#'
#' @return a data.frame with intercept & slope random effects.
#'
#' @examples
#'  cat( 'No working example for unexported function extract_random_effect().' )
#'
#' @author Bill Forrest \email{forrest@@gene.com}
#'
#' @keywords character
#'
#' @seealso \code{\link{data.frame}}
#'
extract_random_effects <-
  function( md4_lmer, md4_gamm4, md4_lmer_pwl, md4_lmer_poly, levels_ID = NULL ){

  if( is.null( levels_ID ) ){
      ## Try to extract the levels from one of the model objects
      
      ## First, md4_lmer:
      if( class( md4_lmer ) == "lmerMod" ){
                levels_ID_md4_lmer <- levels( attributes(md4_lmer)$frame[['ID']] )
        } else{ levels_ID_md4_lmer <- NULL }
      ## Second, md4_gamm4:
      ## If it was estimated, then its sub-list md4_gamm4[['mer']]
      ## should be of class 'lmerMod' also
      if( class( md4_gamm4[['mer']] ) == "lmerMod" ){
              levels_ID_md4_gamm4 <- levels( attributes( md4_gamm4[['mer']] )$frame[['ID']] )
      } else{ levels_ID_md4_gamm4 <- NULL }
      ## Third, md4_lmer_pwl
      if( class( md4_lmer_pwl ) == "lmerMod" ){
                levels_ID_md4_lmer_pwl <- levels( attributes(md4_lmer_pwl)$frame[['ID']] )
        } else{ levels_ID_md4_lmer_pwl <- NULL }
      ## Fourth, md4_lmer_poly
      if( class( md4_lmer_poly ) == "lmerMod" ){
                levels_ID_md4_lmer_poly <- levels( attributes(md4_lmer_poly)$frame[['ID']] )
        } else{ levels_ID_md4_lmer_poly <- NULL }

      
      indicator_nonNULL <-
        c( md4_lmer      = as.numeric(!is.null( levels_ID_md4_lmer      )),
           md4_gamm4     = as.numeric(!is.null( levels_ID_md4_gamm4     )),
           md4_lmer_pwl  = as.numeric(!is.null( levels_ID_md4_lmer_pwl  )),
           md4_lmer_poly = as.numeric(!is.null( levels_ID_md4_lmer_poly ))
          )
      
      number_nonNULL <- sum( indicator_nonNULL )
                          
      if( number_nonNULL == 0 ){
        stop('error in maeve:::extract_random_effects(): models passed are unexpected classes or all NULL; cannot find level_ID values.')
      } ## end of 'if( number_nonNULL == 0 ){...'

      
      if( number_nonNULL == 1 ){

       levels_ID <- 
         switch( as.character( sum( indicator_nonNULL * 1:4 ) ),
                '1' = levels_ID_md4_lmer,
                '2' = levels_ID_md4_gamm4,
                '3' = levels_ID_md4_lmer_pwl,
                '4' = levels_ID_md4_lmer_poly
                )
        
          
      } ## end of 'if( number_nonNULL == 1 ){...'


      ## Make sure that all the non-NULL ID vectors are identical:
      id_list = list( linear     = levels_ID_md4_lmer,
                      spline     = levels_ID_md4_gamm4,
                      piecewise  = levels_ID_md4_lmer_pwl,
                      polynomial = levels_ID_md4_lmer_poly
                     )

      nonNULL <- ! unlist( lapply( id_list, is.null ) ) # logical indicator; TRUE == "ID list is not NULL"
      nonNULL_id_list <- id_list[ nonNULL ] # must have at least one element, or we would have stopped already.

      
      ## 20180817: The following block should be run only when there were two or more non-NULL ID factors
      ## found across the models. There could be as many as four models, but they *should* all have 
      ## identical levels for the subject_ID factor.

      if( sum(nonNULL) > 1 ){
      ## If we do NOT stop in the following block, then all the non-NULL vectors of ID levels must be
      ## identical (since the first is identical to the second, the second to the third, and so on).
      ## If something fails, this will tell us precisely where:      
        for( ii in 1:( sum(nonNULL) - 1 ) ){
          if( ! identical( nonNULL_id_list[[ii]], nonNULL_id_list[[ii+1]] ) ){
            ##
            stop_msg_string <- paste( names(nonNULL_id_list)[ ii:(ii+1) ], collapse = ' and ' )
            stop( 'error in maeve:::extract_random_effects(): ID levels do not match between ', stop_msg_string )
            ##
          } # end of 'if( ! identical( nonNULL_id_list[[ii]], nonNULL_id_list[[ii+1]] ) ){...'
        } # end of 'for( ii in 1:( sum(nonNULL) - 1 ) ){...'
      ##
      ## Now, we do not need to confirm that non-NULL levels are identical below
      ## when we start getting cases of 2+ vectors of IDs.          
      ##
      } # end of 'if( sum(nonNULL) > 1 ){...'


      
      ## Exactly two out of four are non-NULL.  Check that non-NULL levels are identical, then assign the first if so.
      if( number_nonNULL == 2 ){

      ## Case #2.1: fitted line (md4_lmer) and spline(md4_gamm4), but NULL piecewise linear (md4_lmer_pwl) and NULL poly (md4_lmer_poly)
      if( all( indicator_nonNULL == c( 1, 1, 0, 0 ) ) ){ # linear fit; spline fit; piecewise-linear NULL, poly NULL
          if( identical( levels_ID_md4_lmer , levels_ID_md4_gamm4 ) ){
               levels_ID <- levels_ID_md4_lmer
          } else{
               stop( 'error in maeve:::extract_random_effects(): "md4_lmer" and "md4_gamm4" passed contain different ID levels.' )
          }
      }

      ## Case #2.2: fitted line (md4_lmer), NULL spline(md4_gamm4), fitted piecewise linear (md4_lmer_pwl), NULL md4_lmer_poly
      if( all( indicator_nonNULL == c( 1, 0, 1, 0 ) ) ){ # linear fit; NULL spline; piecewise-linear fit
          if( identical( levels_ID_md4_lmer , levels_ID_md4_lmer_pwl ) ){
               levels_ID <- levels_ID_md4_lmer
          } else{
                stop( 'error in maeve:::extract_random_effects(): "md4_lmer" and "md4_lmer_pwl" passed contain different ID levels.' )
          }
      }

      ## Case #2.3: fitted line (md4_lmer), NULL spline(md4_gamm4), NULL piecewise linear (md4_lmer_pwl), fitted md4_lmer_poly
      if( all( indicator_nonNULL == c( 1, 0, 0, 1 ) ) ){ # linear fit; NULL spline; NULL piecewise-linear; fitted md4_lmer_poly
          if( identical( levels_ID_md4_lmer , levels_ID_md4_lmer_poly ) ){
               levels_ID <- levels_ID_md4_lmer
          } else{
                stop( 'error in maeve:::extract_random_effects(): "md4_lmer" and "md4_lmer_poly" passed contain different ID levels.' )
          }
      }
        
      ## Case #2.4: NULL fitted line (md4_lmer), fitted spline(md4_gamm4), fitted piecewise linear (md4_lmer_pwl), NULL md4_lmer_poly
      if( all( indicator_nonNULL == c( 0, 1, 1, 0) ) ){ # NULL linear ; spline fit; piecewise-linear fit; NULL poly fit.
          if( identical( levels_ID_md4_gamm4, levels_ID_md4_lmer_pwl ) ){
               levels_ID <- levels_ID_md4_gamm4
          } else{
             stop( 'error in maeve:::extract_random_effects(): "md4_gamm4" and "md4_lmer_pwl" passed contain different ID levels.' )
          }
      }        

      ## Case #2.5: NULL fitted line (md4_lmer), fitted spline(md4_gamm4), NULL piecewise linear (md4_lmer_pwl), fitted polynomial regression (md4_lmer_poly)
      if( all( indicator_nonNULL == c( 0, 1, 0, 1) ) ){ # NULL linear ;  fitted spline; NULL piecewise-linear, fitted polynomial mixed model.
          if( identical( levels_ID_md4_gamm4, levels_ID_md4_lmer_poly ) ){
               levels_ID <- levels_ID_md4_gamm4
          } else{
             stop( 'error in maeve:::extract_random_effects(): "levels_ID_md4_gamm4" and "md4_lmer_poly" passed contain different ID levels.' )
          }
      }        

        
      ## Case #2.6: NULL fitted line (md4_lmer), NULL spline(md4_gamm4), fitted piecewise linear (md4_lmer_pwl), fitted polynomial regression (md4_lmer_poly)
      if( all( indicator_nonNULL == c( 0, 0, 1, 1) ) ){ # NULL linear ;  NULL spline; piecewise-linear fit, poly fit.
          if( identical( levels_ID_md4_lmer_pwl, levels_ID_md4_lmer_poly ) ){
               levels_ID <- levels_ID_md4_lmer_pwl
          } else{
             stop( 'error in maeve:::extract_random_effects(): "md4_lmer_pwl" and "md4_lmer_poly" passed contain different ID levels.' )
          }
      }        
        
      } ## end of 'if( number_nonNULL == 2 ){...'


      
      if( number_nonNULL == 3 ){ 
      ## This is similar to the "number_nonNULL == 1" case, but with "0" and "1" reversed, so we
      ## pick out the case that is NULL and exclude it, using only the other three:        
      levels_ID <- ### figure out which
         switch( as.character( sum( ( 1 - indicator_nonNULL ) * 1:4 ) ),
                '1' = levels_ID_md4_gamm4, # anything except levels_ID_md4_lmer,       which is NULL
                '2' = levels_ID_md4_lmer,  # anything except levels_ID_md4_gamm4,      which is NULL
                '3' = levels_ID_md4_lmer,  # anything except levels_ID_md4_lmer_pwl,   which is NULL
                '4' = levels_ID_md4_lmer   # anything except levels_ID_md4_lmer_poly,  which is NULL
                )

      } # end of 'if( number_nonNULL == 3 ){ ...'

      
      
      ## Three out of three are non-NULL.  Check that all are identical, then assign the first if so.
      if( number_nonNULL == 4 ){
      ##
        if( identical( levels_ID_md4_lmer,     levels_ID_md4_gamm4     ) &
            identical( levels_ID_md4_gamm4,    levels_ID_md4_lmer_pwl  ) &
            identical( levels_ID_md4_lmer_pwl, levels_ID_md4_lmer_poly )
           ){
             levels_ID <- levels_ID_md4_lmer 
        } else{
            stop('error in maeve:::extract_random_effects(): "md4_lmer", "md4_gamm4", "md4_lmer_pwl", "md4_lmer_poly" passed contain different ID levels')
        }

      } ## end of 'if( number_nonNULL == 4 ){...'

      
        
  } ## end of 'if( is.null( levels_ID ) ){...'


      
  ## Now that we have level_ID values, extract random effects from the model(s).
    
  ## Extract the by-ID random effects from the lmer model:
  if( class( md4_lmer ) == "lmerMod" ){
  re_lmer <-
    lme4::ranef(md4_lmer)[['ID']] %>%
    tibble::rownames_to_column( 'ID' ) %>%
    dplyr::mutate( ID = factor( ID, levels = levels_ID ) ) %>%
    dplyr::rename( intercept_re_lmer = `(Intercept)`, slope_re_lmer = `x` )
  } else{ # this should mean "md4_lmer" is NULL
    # Fill in placeholder data.frame with NA values.
    re_lmer <-
      data.frame( ID = factor( levels_ID , levels = levels_ID ),
                  intercept_re_lmer = NA,
                      slope_re_lmer = NA
                )
  }

      
  ## Extract the by-ID random effects from the merMod object within the gamm4 list:
  if( class( md4_gamm4[['mer']] ) == "lmerMod" ){  
  re_gamm4 <-
    lme4::ranef(md4_gamm4[['mer']])[['ID']] %>%
    tibble::rownames_to_column( 'ID' ) %>%
    dplyr::mutate( ID = factor( ID, levels = levels_ID ) ) %>%
    dplyr::rename( intercept_re_gamm4 = `(Intercept)`, slope_re_gamm4 = `x` )
  } else{ # this should mean "md4_gamm" is NULL
    # Fill in placeholder data.frame with NA values.
   re_gamm4 <-
      data.frame( ID = factor( levels_ID , levels = levels_ID ),
                  intercept_re_gamm4 = NA, 
                      slope_re_gamm4 = NA
                )
  }

      
  ## Extract the by-ID random effects from the lmer_pwl model:      
  if( class( md4_lmer_pwl ) == "lmerMod" ){
  re_lmer_pwl <-
    lme4::ranef(md4_lmer_pwl)[['ID']] %>%
    tibble::rownames_to_column( 'ID' ) %>%
    dplyr::mutate( ID = factor( ID, levels = levels_ID ) ) %>%
    dplyr::rename( intercept_re_lmer_pwl = `(Intercept)`, slope_re_lmer_pwl = `x` )
  } else{ # this should mean "md4_lmer_pwl" is NULL
    # Fill in placeholder data.frame with NA values.
    re_lmer_pwl <-
      data.frame( ID = factor( levels_ID , levels = levels_ID ),
                  intercept_re_lmer_pwl = NA,
                      slope_re_lmer_pwl = NA
                )
  }


  ## Extract the by-ID random effects from the lmer_poly model:
  if( class( md4_lmer_poly ) == "lmerMod" ){
  re_lmer_poly <-
    lme4::ranef(md4_lmer_poly)[['ID']] %>%
    tibble::rownames_to_column( 'ID' ) %>%
    dplyr::mutate( ID = factor( ID, levels = levels_ID ) ) %>%
    dplyr::rename( intercept_re_lmer_poly = `(Intercept)`, slope_re_lmer_poly = `x` )
  } else{ # this should mean "md4_lmer_poly" is NULL
    # Fill in placeholder data.frame with NA values.
    re_lmer_poly <-
      data.frame( ID = factor( levels_ID , levels = levels_ID ),
                  intercept_re_lmer_poly = NA,
                      slope_re_lmer_poly = NA
                )
  }

      
  return( list( re_lmer = re_lmer, re_gamm4 = re_gamm4, re_lmer_pwl = re_lmer_pwl, re_lmer_poly = re_lmer_poly ) )

}  # end of 'extract_random_effects <- function(){...'
