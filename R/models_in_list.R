#' Estimate linear mixed and / or gamm4 and/or piecewise linear and/or polynomial models and return in a list.
#'
#' @author Bill Forrest <forrest@@gene.com>
#'
#' @param clean_DF data.frame with study data loaded and organized for the modeling call.
#' @param metric character vector specifying what metric(s) to use in longitudinal modeling.
#' @param study_ID_value character string with five digit DIVOS study ID, OR a DivoStudy object.
#' @param progress logical determining whether to send interim progress messages.
#' @param number_basis_vecs numeric: number of basis_vecs to include in spline basis used by mgcv
#' @param min_basis_vecs numeric lower bound on number of basis_vecs in spline basis.
#' @param max_basis_vecs numeric upper bound on number of basis_vecs in spline basis.
#' @param weight_lmer_option character ("uniform" or "overweight_baseline") specifying which of two weighting option schemes to use for linear modeling intercept.
#' @param lme4_formula formula for lmer() call.
#' @param gam_formula_char_list two component list of character strings providing the mgcv::gam() formula prefix & suffix to the basis_vec value.
#' @param allow_basis_vec_n_change logical allow the function to change the number of basis_vecs to avoid a crash?
#' @param allowed_number_fix_attempts numeric nonnegative integer.
#'
#' @return An R list with named output.
#'
#' @examples
#'  cat('Currently no working example for models_in_list().')
#'
#' @author Bill Forrest \email{forrest@@gene.com}
#'
#' @references \url{www.r-project.org}
#'
#' @keywords data.frame
#'
models_in_list <- function( clean_DF,
                            metric = c('linear', 'AUC', 'ITGR'),
                            study_ID_value,
                            progress,
                            number_basis_vecs,
                            min_basis_vecs,
                            max_basis_vecs,
                            weight_lmer_option = c("uniform", "overweight_baseline"),
                            lme4_formula  = formula( 'y ~ group + group:x + ( 1 + x | ID )' ),
                            gam_formula_char_list = list( prefix = 'y ~ group +  s( x, bs = "tp", k =',
                                                          suffix = ', by = group )'
                                                    ), ## Below, these combine with "number_basis_vecs" in maeve:::construct_gam_formula().
                            allow_basis_vec_n_change = TRUE,
                            allowed_number_fix_attempts = 20 # How many chances to fix the number of basis_vecs?
                           ){

 if( 'linear' %in% metric ){

   weight_lmer_option = match.arg( weight_lmer_option )

   weight_lmer <- switch( weight_lmer_option,
                     ### Unweighted
                         uniform = rep( 1, nrow( clean_DF ) ),
                     ### Force lines to match "first-timepoint" baseline tumor burden.
                         overweight_baseline =
                           c( 100 * (clean_DF$x == min( clean_DF$x )) +
                                    (clean_DF$x  > min( clean_DF$x ))
                             )
                       )

   if( progress ){
       cat(paste0('Starting lme4 analysis of ', study_ID_value, '.\n' ) )
   }

   ## The next call crashes in the "one treatment group" case (which is seldom used) 
   ## unless we explicitly include the 'weight_lmer' vector in the study data.frame.
   ## In the "2+ group" case (i.e., almost all the time), it makes no difference
   ## that I can see, so I'll run it this way.
   md4_lmer <- lme4::lmer( lme4_formula, data = data.frame( clean_DF, weight_lmer = weight_lmer ), weights = weight_lmer )

 } else{ # alternative to 'if( 'linear' %in% metric ){...'

   if( progress ){
       cat(paste0('Skipping lme4 analysis of ', study_ID_value, '.\n' ) )
   }

   md4_lmer <- NULL

 }


    
 if( any( c( 'AUC', 'ITGR' ) %in% metric ) ){

   ## Fit a generalized additive mixed model ("gamm") to the profiles.
   if( progress ){
       cat(paste0('Starting gamm4/mgcv analysis of ',study_ID_value,'.\n'))
   }

   ## If it's not given, select a number of basis_vecs for the
   ## generalized additive model spline basis.
   ##
   if( is.null( number_basis_vecs ) ){
      number_basis_vecs <- length( unique( clean_DF$x ) ) - 4
      if( number_basis_vecs < min_basis_vecs ) { number_basis_vecs <- min_basis_vecs }
      if( number_basis_vecs > max_basis_vecs ) { number_basis_vecs <- max_basis_vecs }
   }

   md4_gamm4 <-
     try(
         gamm4::gamm4( # gamm4_formula form: "y ~ group + s( x, bs = 'tp', k = number_basis_vecs, by = group )",
                      formula = ## NB: construct_gam_formula() is an unexported "maeve" utility function.
                          construct_gam_formula( basis_vecs_value = number_basis_vecs,
                                                 gam_formula_char_list[['prefix']],
                                                 gam_formula_char_list[['suffix']]
                                                 ), # formula to mgcv::gam().
                      random =~ ( 1 + x | ID ), # random intercepts & slopes only.
                      data = clean_DF,
                      REML = TRUE
                     ),
         silent = FALSE
         ) # end of 'try(...' to catch gamm4 errors (usually related to basis_vec count, in testing).


   md4_gamm4 <- # "maeve:::check_basis_vec_error()"
     check_basis_vec_error( 
          fitted_model_list = md4_gamm4,
          Min_Basis_Vec_n   = min_basis_vecs,
          Basis_Vec_n       = number_basis_vecs,
          Max_Basis_Vec_n   = max_basis_vecs
                      )

   if( 'suggested_basis_vec_n' %in% names( md4_gamm4 ) ){

     ## if TRUE, then check_basis_vec_error() appears to have found a 'try-error'
     if( ! allow_basis_vec_n_change ) { ## no basis_vec changes allowed --> Crash to a stop.

       stop( md4_gamm4$problem )

     } else{ ### seek an acceptable number of basis_vecs by a simple "higher / lower" search.

       number_fix_attempts   <- 0
       continue_fix_attempts <- TRUE


       while( continue_fix_attempts && (number_fix_attempts <= allowed_number_fix_attempts) ){

         if(progress){
           cat( paste('In maeve::models_in_list(), changing number of basis_vecs from',
                      number_basis_vecs,
                      'to',
                      md4_gamm4$suggested_basis_vec_n,
                      'and re-running gamm4().\n'
                      )
               )
         } # end of 'if(progress){...'

         number_basis_vecs <- md4_gamm4$suggested_basis_vec_n
  
         ## re-try fitting the model
         md4_gamm4 <- # *should* use the new "number_basis_vecs" value.
         try( gamm4::gamm4( ## gamm4_formula,
                          formula = ### NB: construct_gam_formula() is an unexported "maeve" utility.
                          construct_gam_formula( basis_vecs_value = number_basis_vecs,
                                                 gam_formula_char_list[['prefix']],
                                                 gam_formula_char_list[['suffix']]
                                                 ), # formula to mgcv::gam().
                         random =~ ( 1 + x | ID ),
                         data = clean_DF,
                         REML = TRUE
                         ),
              silent = TRUE
         )

         ## Check the re-fit model to see whether it's a "try-error" or a properly fit model.
         md4_gamm4 <- check_basis_vec_error( md4_gamm4, min_basis_vecs, number_basis_vecs, max_basis_vecs )

         if( ! 'suggested_basis_vec_n' %in% names( md4_gamm4 ) ){
           continue_fix_attempts <- FALSE # success.
         } else{
           number_fix_attempts <- number_fix_attempts + 1 # still doesn't work.  Keep trying.
         }

       } ## end of 'while( number_fix_attempts <= allowed_number_fix_attempts & continue_fix_attempts ){...'

     } ## end of 'else{...'

   } ## end of 'if( 'suggested_basis_vec_n' %in% names( md4_gamm4 ) ){...'




   if( progress ){
       cat( paste0( 'Finished gamm4/mgcv analysis of ', study_ID_value, '.\n' ) )
   }
     
 }  else{ # if neither 'AUC' nor 'ITGR' is needed, set md4_gamm4 to NULL

      if( progress ){
        cat(paste0('Skipping gamm4 analysis of ', study_ID_value, '.\n' ) )
      }

      md4_gamm4 <- NULL

 } # end of gamm4 fit section.



 


 
 
 if( any( c( 'AUC_pwl', 'ITGR_pwl' ) %in% metric ) ){

   ## Fit a generalized additive mixed model ("gamm") to the profiles.
   if( progress ){
       cat( paste0( 'Starting lme4 piecewise linear analysis of ', study_ID_value, '.\n' ) )
   }


   ## NB: The function "maeve::construct_design_matrix_from_basis()"
   ## will automatically include columns for different groups when the number of
   ## groups is 2+, but will NOT have them when the number of groups is exactly 1.
   ## Hence, we do not have two different formula calls for this model because
   ## unlike the simple linear and spline approaches, there is not a fitting function
   ## that makes the design matrix internally based on a formula and a data.frame.
   ## Rather, we are making the entire fixed effects design matrix and fitting a
   ## mixed model directly from that.  The number of columns in the fixed effects
   ## design matrix, and hence the number of estimated fixed effects, should be
   ## "nlevels( group ) * length( break_points )".
   ##

   ## Optionally, adjust break points to the actual time range
   bp <- maeve_options( 'break_points' )
   if( length( bp ) > 0 && isTRUE( maeve_options( 'adjust_break_points' ) ) ){
     x_range <- range( clean_DF[, 'x'] )
     bp <- bp[ bp >= x_range[1] & bp <= x_range[2] ]
     maeve_options( break_points = bp )
   }
   
   XX <- construct_design_matrix_from_basis( clean_DF, basis_choice = 'tent', group_name = 'group', x_name = 'x' )
   
   md4_lmer_pwl <- lme4::lmer( y ~ -1 + XX + ( 1 + x | ID ), data = clean_DF )
   
 } else{ ## alternative to 'if( any( c('AUC_pwl') %in% metric ) ){...'

   if( progress ){
       cat(paste0('Skipping lme4 piecewise linear analysis of ', study_ID_value, '.\n' ) )
   }

   md4_lmer_pwl <- NULL

 }





 if( any( c( 'AUC_poly', 'ITGR_poly' ) %in% metric ) ){

   ## Fit a generalized additive mixed model ("gamm") to the profiles.
   if( progress ){
       cat(paste0('Starting lme4 simple polynomial basis analysis of ',study_ID_value,'.\n'))
   }

   ## NB: The function "maeve::construct_design_matrix_from_basis()"
   ## will automatically include columns for different groups when the number of
   ## groups is 2+, but will NOT have them when the number of groups is exactly 1.
   ## Hence, we do not have two different formula calls for this model because
   ## unlike the simple linear and spline approaches, there is not a fitting function
   ## that makes the design matrix internally based on a formula and a data.frame.
   ## Rather, we are making the entire fixed effects design matrix and fitting a
   ## mixed model directly from that.  The number of columns in the fixed effects
   ## design matrix, and hence the number of estimated fixed effects, should be
   ## "nlevels( group ) * length( break_points )".
   ##
   
   XX <- construct_design_matrix_from_basis( clean_DF, basis_choice = 'poly', group_name = 'group', x_name = 'x' )
   
   md4_lmer_poly <- lme4::lmer( y ~ -1 + XX + ( 1 + x | ID ), data = clean_DF )
   
 } else{ ## alternative to 'if( any( c('AUC_poly') %in% metric ) ){...'

   if( progress ){
       cat(paste0('Skipping lme4 simple polynomial basis analysis of ', study_ID_value, '.\n' ) )
   }

   md4_lmer_poly <- NULL

 }   

    
 return( list( md4_lmer = md4_lmer, md4_gamm4 = md4_gamm4, md4_lmer_pwl = md4_lmer_pwl, md4_lmer_poly = md4_lmer_poly ) )

} # end of "models_in_list()".
