#' Estimate confidence intervals for contrasts from multcomp::glht()
#' 
#' Input is the name of the glht object and a list containing it. This
#' choice reflects how the function is usually used in the workflow.
#'
#' @param glht_obj_name character string with name of list component containing the glht object.
#' @param glht_obj_list list containing a glht object.
#' @param conf_level numeric. What level of confidence intervals should be returned?  Passed directly to stats::confint().
#' @param adjustment_method  character string specifying a method for p-value adjustment.  Setting to 'none' will return unadjusted p-values and confidence intervals.  Setting to anything else (the default is 'single_step') will implement a particular p-value adjustment from multcomp.  Although the p-value adjustment method varies for different options of this parameter, all the options other than 'none' will return the same FWER-adjusted confidence intervals implemented via setting "calpha = multcomp::adjusted_calpha()" in "multcomp::confint.glht()". Setting adjustment_method = 'none' will return confidence intervals with "calpha = multcomp::univariate_calpha()" in "multcomp:::confint.glht()".
#' @param extended_output logical determining whether to compute extra output to return to a DIVOS summary table.
#' @param round_estimate  logical determining whether to pass two data.frames through maeve::round_numerics() before merging on a numeric column.
#'
#' @return a data.frame
#'
#' @examples
#'  cat('Currently no working examples for maeve:::make_confint().')
#'
#' @author Bill Forrest \email{forrest@@gene.com}
#'
#' @keywords lme4
#'
#' @seealso \code{\link{data.frame}}
#'
make_confint <- function( glht_obj_name,
                          glht_obj_list,
                          conf_level = 0.95,
                          adjustment_method = c( "single-step", "Shaffer", "Westfall", "free", "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none" ),
                          extended_output = FALSE,
                          round_estimate  = TRUE
                         ){

   ## This will be applied to a list of glht() objects.
   if( length( glht_obj_name ) != 1 |
        class( glht_obj_name ) != "character"
      ){
       stop( 'error in maeve:::make_confint(): glht_obj_name must be a single character string.' )
       }  

   adjustment_method = match.arg( adjustment_method )
  
   ## escape hatch if a NULL was provided:
   if( is.null( glht_obj_list[[glht_obj_name]] ) ){ return( NULL ) }

   ## ensure conf_level is in (0,1):
   stopifnot( is.numeric( conf_level ) &  conf_level > 0 & conf_level < 1 )
  
### Note for future development:
### It appears that setting the "level = conf_level" parameter introduces a small
### (practically, probably negligible) stochastic terms that's added into all the
### confidence intervals.  I noticed it because one of the unit tests suddenly started
### failing because all the confidence intervals (but not the point estimates) shifted
### by a small amout (~ 5e-5 with unit-ish effect sizes) to the left or right, **in unison***
### meaning the lower & upper CI for *each* parameter shifts by about the same amount, for all
### the parameters.  Again, it can probably be ignored, but I want to at least understand
### what's happening.  This random shift is deterministic in that if I set.seed() to a
### consistent number right before calling compare_groups(), then the confidence intervals
### come out the same, but when I change the seed, I change the interval.
###
  

### Note for future development:
### Here is a point of possible concern, but not one being pursued at present.
### In the code of "mvtnorm::qmvt()", which gets called via stats::confint() within
### maeve::make_confint() on a number of glht objects returned from "leia()", We get an
### error if we try to set the "conf_level" (i.e, the confidence interval coverage) below
### 0.50.  This appears to trace back to code in mvtnorm::qmvt() that stipulates
###
###     if (tail == "both.tails" && p < 0.5) 
###        stop("cannot compute two-sided quantile for p < 0.5")
###  
### but I don't understand what this is guarding against or why it's necessary, since it seems
### as if "two-sided quantiles" are well defined for values in (0, 0.50). Reading from the qmvt()
### documentation for the "tail" argument, when tail == 'both.tails', the quantile value 'x' should
### be the value such that P[-x ≤ X ≤ x] = p.  By setting the 'x' values increasingly closer to zero,
### this looks as if it should be OK (I'm likely missing somethin obvious here...).  Furthermore, 
### about ten lines down in the mvtnorm::qmvt() code, I see this:
###
###         if (tail == "both.tails") 
###            p <- ifelse(p < 0.5, p/2, 1 - (1 - p)/2)
###
### though I'm not seeing anything in the intervening lines that appears to change 'p'.  These latter
### lines, though, seem to allow for p < .50 (?).
###
### Practically, this is not a huge deal at the moment, since we typically just leave the
### coverage at 0.95, and seem unlikely to set it below .50 in any case.  Still, I'd like to revisit
### this later to figure out if it's a vestigial / mistaken stop() in the function or I'm
### indeed missing something.  I'll be doing simulations to test coverage, so hopefully that will point
### to big problems if they are present.  
  
   if( adjustment_method != 'none' ){
     multcomp_adjustment_function  <- multcomp::adjusted_calpha()     
   } else{
     multcomp_adjustment_function  <- multcomp::univariate_calpha()
   }
     
   mat <- stats::confint( glht_obj_list[[glht_obj_name]],
                          level = conf_level,
                          calpha = multcomp_adjustment_function
                         )$confint  

### Note for future development:
### R 4.0.0 has "options( 'stringsAsFactors' ) = FALSE", which
### is a problem here since then 'metric' comes out as 'character' instead of 'factor'.
### 
### Added a similar fix for 'contrast'.
### 
   df <- data.frame( ## explicitly convert 'metric'    to factor so that it works as expected when options( 'stringsAsFactors' ) == FALSE.
                     metric = glht_obj_name %>% factor, 
                     ## explicitly convert  'contrast' to factor so that it works as expected when options( 'stringsAsFactors' ) == FALSE.
                     contrast = (trim_final_colon( row.names( mat ) )) %>% factor( levels = rownames( mat ) ),
                     mat # defined via stats::confint(...) just above.
                    )
    
   if( round_estimate ){
       df %<>% maeve::round_numerics()
   }
    
   row.names( df ) <- NULL

   if( extended_output ){
   ##
   ## This block extracts estimated contrasts with standard errors, t-tests, and p-values,
   ## then puts them into a data.frame and joins it to the already-created
   ## 'df' data.frame above.  This supplements the "Estimate" and confidence interval
   ## values with standard errors, t-tests, and p-values, if the argument "extended_output"
   ## is set to TRUE (its default is FALSE).
   ##    
    summary( glht_obj_list[[glht_obj_name]], test = multcomp::adjusted( adjustment_method ) )$test[ c( 'coefficients', 'sigma', 'tstat', 'pvalues' ) ] %>%
    data.frame %>%
    dplyr::mutate( adj_method = adjustment_method ) %>%
    dplyr::mutate( contrast = (trim_final_colon( row.names(mat) )) ) %>% ### lmer has no ":" suffix, but gam does.
    dplyr::mutate( contrast = factor( contrast, levels = levels( df$contrast ) ) ) %>%
    dplyr::select( contrast, coefficients:adj_method ) %>%
    dplyr::rename( Estimate = coefficients ) %>%
    { if( round_estimate ){ maeve::round_numerics( . ) } else{ . } } %>% # conditional evaluation of this line.
    ###
    dplyr::left_join( df, by = c('contrast', 'Estimate') ) %>% ### 'Estimate' is numeric, but should be identical between the two.
    dplyr::select( metric, contrast, Estimate, sigma, lwr, upr, tstat, adj_method, pvalues ) -> df
    
  }
  
  return( df )
  
 } # end of 'make_confint <- function( glht_obj_name, glht_obj_list, extended_output = FALSE ){...'
