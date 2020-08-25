#' Integrate AUC for curves from a multigroup GAM
#'
#' @author Bill Forrest <forrest@@gene.com>
#'
#' @param model object of class 'gam' from package 'mgcv' (not package 'gam').
#' @param N integer number of equally-spaced points to use in numerical integration.
#' @param xmin numeric lower bound of definite integral
#' @param xmax numeric upper bound of definite integral
#' @param xname character name of numeric variable used in the gam smooth.  If provided, it should be accessible in "model$model[[xname]]".
#' @param xrange_norm_method character string determining *how* the xrange normalization within leia() should be done.  Must be one of "none", "xrange", "half_xrange_squared".
#' @param subtract_starting_value logical: Subtract from all computed AUCs the spline fit at xmin for that group (adjusted for xrange_norm_method choice).
#' @param groupname character name of grouping factor by which the one-dimensional smooth is conditioned.  If provided, it should be the first and only component in model$xlevel.
#' @param contrast character string with contrast type.  'Identity' gives each curve separately.  'Dunnett', 'Tukey', etc. from multcomp::contrMat() are accepted, as is 'custom' to indicate a custom contrast.
#' @param custom_contrast numeric matrix of contrasts for slope estimates. Column names must match exactly with treatment group names.  If it is not NULL, it overrides whatever is specified in "contrast".
#' @param reference_Dunnett character string with reference group for Dunnett's test.  If provided, must match exactly to one of "model$xlevel[['group']]".
#' @param glht_rhs numeric vector for the right-hand side (rhs) of the contrast hypothesis; passed to glht() "rhs = " argument.
#' @param glht_alternative character string passed to glht() as its "alternative = " argument.
#' @param provide_warnings logical: provide warning messages when something looks look awry?
#' @param single_group_identifier character string with name to give a single group in the output.
#' @param derivative integer specifying which derivative of the fixed smooth to integrate (the default, "0", means to just integrate the fixed smooth).
#' @param return_list logical: return a whole bunch of output in a list.
#'
#' @return an object of class glht from package multcomp (or a list if return_list == TRUE)
#'
#' @examples
#' N <- 100
#' x <- seq( 0, pi, length = N )
#' set.seed( 20180425 )
#' dat <- data.frame( x, y = sin( x ) + rnorm( N, 0, .01 ) )
#' model <- mgcv::gam( y ~ s( x ), data = dat )
#' leia_out  <- leia( model, xmin = 0, xmax = pi/2, glht_rhs = 1 ) # about "1.0" over [ 0, pi/2 ]...
#' leia_list <- leia( model, xmin = 0, xmax = pi/2, glht_rhs = 1, return_list = TRUE ) # extras...
#'
#' @author Bill Forrest \email{forrest@@gene.com}
#'
#' @references \url{www.r-project.org}
#'
#' @keywords data.frame
#' @export
#'
leia <-
        function( model, ### "md$gam", "md4$gam", etc., if starting with a gamm4 object. 
                  N = 25,
                  xmin = NULL, 
                  xmax = NULL, 
                  xname = NULL,
                  xrange_norm_method = c("none", "xrange", "half_xrange_squared"),                 
                  subtract_starting_value = TRUE,
                  groupname =  NULL,
                  contrast = c( 'Identity', 'Dunnett', 'Tukey', 'Sequen', 'custom' ),
                  custom_contrast = NULL,
                  reference_Dunnett = maeve_options("reference_Dunnett"), ### NULL,
                  glht_rhs = NULL,
                  glht_alternative = c("two.sided", "less", "greater"),
                  provide_warnings = TRUE,
                  single_group_identifier = 'group_01', # assigned output name for the "1 group" case.
                  derivative = 0,
                  return_list = FALSE
                 ){

          
   contrast = match.arg( contrast ) # We're equipped to handle only select contrasts...

   glht_alternative = match.arg( glht_alternative ) # passed directly to "alternative = ..." in glht().

   xrange_norm_method = match.arg( xrange_norm_method ) 
   
   if( ! any( class( model ) == 'gam' ) ){
       stop('error: model object must be of class "gam" in maeve::leia()')
   }

   if( ! ( abs(derivative) < 1e-12 | abs(derivative - 1) < 1e-12 ) ){
       stop('error in maeve::leia(): function works only for "derivative = 0" or "derivative = 1"')
   }

   char_formula <- as.character(model$formula)[3] # should be the main body of the formula

   ## The section below tries to extract the name of the smoothed covariate if it is not provided.
   if( is.null( xname ) ){  xname <-
        strsplit( gsub(' +','',char_formula), 's\\('  ) %>% # split by "s(", but need the "\\" included.
        unlist %>% # next line intends to extract characters *after* "s(' and *before* a comma ',' or close-parentheses ")".
       (function(xx) as.character(strsplit(xx[2],"[,)]")[[1]][1])) # split by "," or ")" and take the first element.
   }
   

   
   ## The byzantine sections below identify whether there is a grouping factor,
   ## then if there *is* a grouping factor, what are its levels? All this is
   ## so that we can deal smoothly with two use cases:
   ##
   ## (a) A gam() smooth of exactly 1 variable, with no " by = " grouping factor
   ##
   ## (b) A gam() smooth of exactly 1 variable, with  a " by = " grouping factor

   ## (1) Is there a grouping factor, or are all the data from one group?
   ##
   
   if( grepl('by=', gsub( ' +', '', char_formula ) ) ){ # check whether formula has a "by =" conditional, indicating grouping
     
        found_groupname <- strsplit( gsub(' +','',char_formula), 'by=' ) %>%
                           unlist %>% # next line intends to extract characters *after* "by = ' and *before* a comma ',' or close-parentheses ")".
                          (function(xx) as.character(strsplit(xx[2],"[,)]")[[1]][1])) # split by "," or ")" and take the first element.
        
        if( is.null( groupname ) ){
          
           groupname <- found_groupname # no groupname was supplied, but one appears to be there, so we use it.
           
        } else{ # a groupname was provided (i.e., not NULL), but it does not match the formula.  Give a warning:
          
            if( provide_warnings && groupname != found_groupname ){
              
                warning(paste('\n\n WARNING: in leia(): "groupname" supplied is',
                               groupname,
                              'but model$formula contains',
                               paste( ' by =', found_groupname )
                             )
                       )
              
              } # end of 'if( provide_warnings && groupname != found_groupname ){...'
            
        } # end of 'else{ # a groupname was provided...'
        
   } else{ ## "if( grepl('by=', gsub(' +','',char_formula) ) ){..." is FALSE
     
     ## There is no " by = " text in the formula, which we interprets as there being no grouping factor.

     ### if a groupname is at the default ("NULL"), then set found_groupname to NULL also:
     if( is.null( groupname ) ){
       
         found_groupname <- NULL
         
     } else{

     ### if a groupname is *NOT* NULL, stop:
       
       if( provide_warnings ){
          warning(paste('\n\n WARNING: in leia(): "groupname" supplied is',
                         groupname,
                        'but model$formula cannot find\n anything containing',
                         '" by =", which should be present with a gam() grouping factor.'
                        )
                 )
          
        } # end of "if( provide_warnings ){...
         
     } # end of "else{..."
   
   } # end of "else{ ## "if( grepl('by=', gsub(' +','',char_formula) ) )...



   
   
   

   ## This section ensures that "model$xlevel" is defined.
   ## If there is a grouping factor, then "model$xlevel" is a 
   ## named list with a factor that contains its levels.
   ##
   ## If there is NOT a grouping factor,
   ## then "model$xlevel" is NULL.  In that case, we define
   ## "model$xlevel[[1]]" to be the name of the single group
   ## provided in argument "single_group_identifier" so that
   ## it can be used in naming the output below.
   
   if( class(model$xlevel[[ groupname ]]) != 'character' ){
     #
      if( ! is.null( model$xlevel ) ){
       ## This shouldn't actually happen in practice.  It means that there is a
       ## non-NULL model$xlevel[[groupname]], but that it does NOT have the
       ## actual levels for a group factor.  It should either have the levels or
       ## be NULL.
       stop( 'error in maeve::leia(): model$xlevel should be a character vector with nonzero ',
             'length or NULL (if there is only 1 group in the GAM)'
           )
     } else {
       ## This is the "group" in a single-group model (i.e., "model$xlevel" is NULL).
       ## We define "model$xlevel" to be a list containing a factor named after
       ## the "single_group_identifier" character argument, with a one-level factor 
       ## containing the "single_group_identifier" as its only level
       if( is.null( groupname ) ){
         ## If there is no 'groupname' supplied, then use 'single_group_identifier',
         ## which by default is the hopefully innocuous character string 'group_01'.
         ##
          model$xlevel <- list( factor( single_group_identifier ) )
         ## Set both the factor in 'model$xlevel' and the variable 'groupname'
         ## to single_group_identifier
          groupname <- names( model$xlevel ) <- single_group_identifier
         ##
         } else{
         ## If 'groupname' *is* supplied, then use it to name the missing
         ## factor in model$xlevel, and as its only value:
          model$xlevel <- list( factor( groupname ) )
          names( model$xlevel ) <- groupname
         #
         }
      } # end of 'else {...'
     } # end of 'if( class(model$xlevel[[ groupname ]]) != 'character' ){...'



   
   ## Make a data frame with "N" or more equally-spaced abcissa values
   ## and matching group-level predictions for numerical integration.
   ##

   ## If xmin & xmax are given, make a grid of N points.
   ## 
   if( (!is.null( xmin )) & (!is.null( xmax )) ){ # have both xmin & xmax
     
        stopifnot( is.numeric( xmin ) & is.numeric( xmax ) & xmin < xmax )
     
        xgrid <- seq( xmin, xmax, length = N )
    }


   if( is.null( xmin ) + is.null( xmax ) == 1 ){ # only one was provided.
          stop( 'error in maeve::leia(): if "xname" is specified, then both ',
                '"xmin" & "xmax" must be specified or neither can be specified.'
               )
   } # end of 'if( is.null( xmin ) + is.null( xmax ) == 1 ){ # only one was provided'

   
   
   ## If neither of xmin & xmax is given, try to get a grid from 'xname'.
   if( ( is.null( xmin ) | is.null( xmax ) ) ){
     
      if( is.null( xname ) ){
       ## This is the case in which *NONE* of xmin, xmax, or xname are given
       ## or found (since "xname" can be determined from the formula in a section above):
       
        stop('error in maeve::leia(): must provide either (1) both "xmin" and "xmax" or (2) "xname".') 
        
      } 

      stopifnot( is.character( xname ) & xname %in% names(model$model) ) 
      
      xgrid <- sort( unique( model$model[[xname]] ) ) %>%         ### sort unique values
                 ( function(x) x[!is.na(x)] ) %>%                 ### discard NA's      
                 ( function(x) seq( min(x), max(x), length = N )) ### return evenly spaced grid spanning the unique values.
  
   } # end of 'if( ( is.null( xmin ) | is.null( xmax ) ) &  ){...'



   # Set up grid of abcissa values for numerical integration.
   Xp <- expand.grid( factor( model$xlevel[[groupname]], levels = model$xlevel[[groupname]] ), xgrid )
   colnames( Xp ) <- c( groupname, xname )
   Xp <- Xp %>% dplyr::arrange( !!dplyr::sym( groupname ), !!dplyr::sym( xname ) )
   Xp <- Xp %>% (function(XX) data.frame( XX, pred_gam = predict( model, newdata = XX ) ) )



   
   ### Set up a group-contrast matrix
   ###

   if( contrast != 'custom' ){
   ## a long block below deals with making a contrast matrix either
   ## from an Identity matrix or via the built-in funcionality in
   ## multcomp::contrMat().  If contrast == 'custom', we skip all this
   ## and go to the section below on customized contrast matrices.

   if( !is.null( custom_contrast ) ){
     stop( 'error in maeve::leia(): if contrast != "custom", then the parameter "custom_contrast" ',
           'must be NULL, which is its default; set "contrast" to "custom" if you want to ',
           'pass a matrix in the "custom_contrast" argument to maeve::leia()'
          )
   }
     
   if( contrast == 'Identity' ){
               contr_mat   <- diag(nlevels(Xp[[groupname]]))
     rownames( contr_mat ) <- levels( Xp[[groupname]] )
   } else{

       if( contrast == 'Dunnett' ){
         ### This enables switching the Dunnett test reference group.
         if( is.null( reference_Dunnett ) ){ reference_Dunnett = levels( Xp[[groupname]] )[1] }
         contrMat_base <- match( reference_Dunnett, levels( Xp[[groupname]] ) )
         if( is.na( contrMat_base ) ){
           stop( 'error in maeve::leia(): "reference_Dunnett" value ', reference_Dunnett, ' not matched' )
         }
       } else{ contrMat_base <- 1 }
           
       contr_mat <- try(
                    Xp[[ groupname ]] %>%
                  ( function(XX) structure( rep( 0, nlevels( XX ) ), names = levels( XX ) ) ) %>%
                    multcomp::contrMat( type = contrast, base = contrMat_base )
                        )
     
     ### A common error is a non-"Identity" request with only 1 group:
     if( grepl('fewer than two groups', contr_mat[[1]] ) ){
       
      stop( 'error in maeve::leia(): with only one group, must set "contrast" to "Identity".' )
      
     } # end of "if( grepl('fewer than two groups', contr_mat[[1]] ) ){...
     
   } # end of 'else{...'

   
   } # end of 'if( contrast != 'custom' ){...' block.



   
   if( contrast == 'custom' ){

   ## check whether "custom_contrast" contains a custom contrast matrix.  
   ## In this case, ignore the contrast matrix derived from a character 
   ## string name (e.g., "Identity", "Dunnett") and use the one provided
   ## instead:

     if( ! ( is.matrix( custom_contrast ) & is.numeric( custom_contrast ) ) ){
       ## "Is custom_contrast NOT a matrix of numerics?"
       stop('error in maeve::leia(): "custom_contrast" must be a numeric matrix when contrast = "custom".')
     }
     
     group_index <- match( colnames( custom_contrast ), levels(Xp[[ groupname ]]) )
     
     if( any( is.na( group_index ) ) ){
     ## This error means that the column names of the customized contrast matrix
     ## do not match the group names passed to the model. A common cause of this
     ## error is when the group names have been abbreviated somewhere along the
     ## way, perhaps deleting white space or needless extra letters.  To avoid it,
     ## either do not abbreviate, *or* also abbreviate the column names of the
     ## custom contrast matrix in a matching manner.
       stop_msg_string <- paste( levels(Xp[[ groupname ]]) , collapse = '\n' )
       stop( 'error in maeve::leia(): column names of "custom_contrast" do not match current group names; ',
             'group names may have been abbreviated (check "abbreviate_n" parameter); current values are:\n\n',
              stop_msg_string
            )
     }

     ## List with
     ##  (1) row names equal to the row names provided in custom_contrast, and
     ##  (2) column names equal to ALL the fixed effect LME estimates
     dimnames_linfct_gam <-
       list( contrast.names = rownames( custom_contrast ),
                group.names = levels( Xp[[groupname]] )
            )

     ## Make a matrix of zeros from the name vector lengths in "dimnames_linfct_lmer":
     contr_mat   <- matrix(0, nrow = nrow( custom_contrast ), ncol = nlevels(Xp[[groupname]])) # correctly sized matrix of zeros.

     dimnames( contr_mat ) <- dimnames_linfct_gam ### assign row names from custom_contrast and column names from treatment groups.

     contr_mat[,group_index] <- custom_contrast ### splice in contrasts matched by group_index; the rest will remain zeros.

   } ## end of  'if( contrast == 'custom' ){...'


   


  ## Compute the xrange normalization constant:

  xrange_norm_constant <- 
   switch( xrange_norm_method,
                         'none' = 1.0,
                       'xrange' = 1.0 / diff( range( xgrid ) ),
          'half_xrange_squared' = 1.0 / ( 0.5 * ( diff( range( xgrid ) ) )^2 ) ## area of right triangle with non-hypotenuse sides both the length of x-range.
          )
          
  ## After all of that, we finally perform the numerical integration:
   
  simpson_coef_one_group <-
     plyr::dlply( Xp, groupname, function(XX){ (simpson_coefficients)( XX[[xname]] ) } ) %>%
     ( function(XX) matrix( XX[[1]], nrow = 1 ) )

  ## Scale the Simpson coefficients by the xrange normalization factor:   
  simpson_coef_one_group <- simpson_coef_one_group * xrange_norm_constant

  Xs <- predict( model, type = "lpmatrix", newdata = Xp ) # "Xp evaluated at splines" --> "Xs"

  if( subtract_starting_value ){
  ## This will be used to define "Xs_t0", which will in turn be used to subtract out each group's baseline fitted value
    Xp_t0 <- expand.grid( factor( model$xlevel[[groupname]], levels = model$xlevel[[groupname]] ), rep( min(xgrid), N ) )
    colnames( Xp_t0 ) <- c( groupname, xname )
    Xp_t0 <- Xp_t0 %>% dplyr::arrange( !!dplyr::sym( groupname ), !!dplyr::sym( xname ) )
    Xp_t0 <- Xp_t0 %>% (function(XX) data.frame( XX, pred_gam = predict( model, newdata = XX ) ) )

    Xs_t0 <- predict( model, type = "lpmatrix", newdata = Xp_t0 ) # "Xp_t0 evaluated at splines" --> "Xs_t0"
     
    Xs <- Xs - Xs_t0  # subtract out the baseline spline value for each group.
     
  }

            
  if( derivative == 1 ){ # integrate AUC for first derivative
     
    dx <- median( diff( sort( xgrid ) ) ) / 10 # small positive "delta" for finite difference
     
   ## We need three cases to deal with the left boundary, interior, and right boundary
    left_endpoint <- Xp[[xname]] == min( Xp[[xname]] )
   interior_point <- Xp[[xname]]  > min( Xp[[xname]] ) & Xp[[xname]]  < max( Xp[[xname]] )
   right_endpoint <- Xp[[xname]] == max( Xp[[xname]] )

   Xp0 <- Xp1 <- Xp # set up placeholder matrices to find a finite difference

   Xp0[[xname]][ !  left_endpoint ] <- Xp0[[xname]][ !  left_endpoint ] - dx
     
   Xp1[[xname]][ ! right_endpoint ] <- Xp1[[xname]][ ! right_endpoint ] + dx     

   dx_mat <- matrix( NA, nrow = nrow( Xs ), ncol = ncol( Xs ) )
   dx_mat[  left_endpoint, ] <-  dx
   dx_mat[ interior_point, ] <- (dx + dx) # interior points are shifted left in Xp0, right in Xp1.
   dx_mat[ right_endpoint, ] <-  dx 
    
    
   Xs0 <- predict( model, type = "lpmatrix", newdata = Xp0 )    
   Xs1 <- predict( model, type = "lpmatrix", newdata = Xp1 )

   Xs <- ( Xs1 - Xs0 ) / dx_mat

   Xp <- Xp %>% dplyr::mutate( pred_gam_deriv1 = c(Xs %*% (model %>% coef %>% matrix(ncol = 1))) )
                               
  } # end of 'if( derivative == 1 ){ # integrate AUC for first derivative...'

            
  linfct_mat <- kronecker( contr_mat, simpson_coef_one_group, make.dimnames = TRUE ) %*% Xs 

  ##  Added the next line to try to avoid the "trailing colon" problem in downstream names:
  rownames( linfct_mat ) %<>% trim_final_colon()
   
  ## the right-hand side of the hypothesis is the 'rhs' argument to glht().
  ## 
  if( is.null( glht_rhs ) ) {
      glht_rhs <- rep(0, nrow( linfct_mat ) )
  }
            
  if( length( glht_rhs ) != nrow( linfct_mat ) ){
      stop( 'error in maeve::leia(): glht_rhs should be of length ',
             nrow( linfct_mat ),
            ' but is of length ',
             length( glht_rhs )
           )
  }

  if( ! return_list ){
      return( multcomp::glht( model, linfct_mat, rhs = glht_rhs, alternative = glht_alternative ) )
  } else{ # return a bunch of other stuff as well, in a list:
     return(
       list(
            glht_out = multcomp::glht( model, linfct_mat, rhs = glht_rhs, alternative = glht_alternative ),
            linfct_mat = linfct_mat,
            xname = xname,
            groupname = groupname,            
            xgrid = xgrid,
            Xp = Xp,
            Xs = Xs, # evaluated at spline functions
            derivative = derivative,
            contr_mat = contr_mat,
            simpson_coef_one_group = simpson_coef_one_group,
            xrange_norm_method = xrange_norm_method
            ) # end of 'list(...'
           ) # end of 'return(...'
  } # end of 'else{...'
    
} # end of function definition for  "leia()"
