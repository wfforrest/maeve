#' Integrate AUC for curves from a multigroup piecewise or polynomial lmerMod mixed model: Piecewise/Polynomial Integration Analysis
#'
#' @author Bill Forrest <forrest@@gene.com>
#'
#' @param model object of class 'lmerMod' from package 'lme4'.
#' @param basis_choice character describing which basis function type to use in making a design matrix.
#' @param break_points numeric vector with the x-locations at which to place break points.
#' @param degree numeric degree of polynomial basis, if that is selected.  Passed straight to stats::poly().
#' @param N integer number of equally-spaced points to use in numerical integration.
#' @param local_modeling_data_frame data.frame should be clean_DF_restricted form model_study(), or just the contents of maeve_options('modeling_data_frame').  
#' @param xmin numeric lower bound of definite integral
#' @param xmax numeric upper bound of definite integral
#' @param xname character name of numeric variable used in the piecewise fit.  If provided, it should be a column in "maeve::maeve_options('modeling_data_frame')".
#' @param xrange_norm_method character string determining *how* the xrange normalization within pia() should be done.  Must be one of "none", "xrange", "half_xrange_squared".
#' @param subtract_starting_value logical: Subtract from all computed AUCs the spline fit at xmin for that group (adjusted for xrange_norm_method choice).
#' @param groupname character name of grouping factor by which the piecewise fit is conditioned.  If provided, it should be the first and only component in model$xlevel.
#' @param contrast character string with contrast type.  'Identity' gives each group fit separately.  'Dunnett', 'Tukey', etc., from multcomp::contrMat() are accepted.
#' @param custom_contrast numeric matrix of contrasts for slope estimates. Column names must match exactly with treatment group names.  If it is not NULL, it overrides whatever is specified in "contrast".
#' @param reference_Dunnett character string with reference group for Dunnett's test.  If provided, must match exactly to one of "maeve::maeve_options('modeling_data_frame')[['group']]".
#' @param glht_rhs numeric vector for the right-hand side (rhs) of the contrast hypothesis; passed to glht() "rhs = " argument.
#' @param glht_alternative character string passed to glht() as its "alternative = " argument.
#' @param provide_warnings logical: provide warning messages when something looks look awry?
#' @param single_group_identifier character string with name to give a single group in the output.
#' @param derivative integer specifying which derivative of the fixed fits to integrate (the default, "0", means to just integrate the fixed effects curve by group).
#' @param return_list logical: return a whole bunch of output in a list.
#'
#' @return an object of class glht from package multcomp (or a list if return_list == TRUE)
#'
#' @examples
#' \dontrun{
#' N <- 100
#' x <- seq( 0, pi, length = N )
#' set.seed( 20180425 )
#' dat <- data.frame( x, y = sin( x ) + rnorm( N, 0, .01 ) )
#' model    <- mgcv::gam( y ~ s( x ), data = dat )
#' pia_out  <- pia( model, xmin = 0, xmax = pi/2, glht_rhs = 1 ) # about "1.0" over [ 0, pi/2 ]...
#' pia_list <- pia( model, xmin = 0, xmax = pi/2, glht_rhs = 1, return_list = TRUE ) # extras...
#' }
#'
#' @author Bill Forrest \email{forrest@@gene.com}
#'
#' @references \url{www.r-project.org}
#'
#' @keywords data.frame
#'
pia <-  function( model, ### an lmerMod object.
                  basis_choice = c( 'tent', 'poly' ),                  
                  break_points = NULL,
                  degree       = NULL,
                  N = 25,
                  local_modeling_data_frame = NULL,
                  xmin = NULL, 
                  xmax = NULL, 
                  xname = NULL,
                  xrange_norm_method = c("none", "xrange", "half_xrange_squared"),                 
                  subtract_starting_value = TRUE,
                  groupname =  NULL,
                  contrast = c('Identity','Dunnett','Tukey','Sequen','custom'),
                  custom_contrast = NULL,
                  reference_Dunnett = NULL,
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

   if( ! any( class( model ) == 'lmerMod' ) ){
       stop('error in maeve:::pia(): the model object must be of class "lmerMod"')
   }

   if( ! ( abs(derivative) < 1e-12 | abs(derivative - 1) < 1e-12 ) ){
       stop('error in maeve:::pia(): function works only for "derivative = 0" or "derivative = 1"')
   }

   char_formula <- as.character( formula(model)[3] ) # should be the main body of the formula

   ## The section below tries to extract the name of the piecewise covariate
   ## if it is not provided, using the usual convention established within maeve
   ## (so this is far from foolproof).
   if( is.null( xname ) ){  xname <-
        strsplit( gsub(' +','',char_formula), '\\(1\\+'  ) %>% # split by "s(", but need the "\\" included.
        unlist %>% # next line intends to extract characters *after* "(1+' and *before* "|ID)"
       (function(xx) as.character(strsplit(xx[2],"\\|ID\\)")[[1]][1])) # split by "|ID" and take the first element.
   }

   
   ## If
   ##   
   ##   maeve::model_study()
   ##   
   ## was used to called to fit the model, and if
   ##   
   ##   maeve::maeve_options("autoset_modeling_data") == TRUE
   ##   
   ## then the data frame used to fit the model should be available in
   ##   
   ##   maeve::maeve_options("modeling_data_frame")
   ##   
   ## and the column names for the grouping factor and the numeric time
   ## variable should be 'group' and 'x', respectively.  If the data.frame
   ## maeve::maeve_options("modeling_data_frame") is NOT available, then
   ## the user of this function needed to provide it explicitly.
  
   ## Make a data frame with "N" or more equally-spaced abcissa values
   ## and matching group-level predictions for numerical integration.

    
   ### If xmin & xmax are given, make a grid of N points.
   if( (!is.null( xmin )) & (!is.null( xmax )) ){ # have both xmin & xmax
     
        stopifnot( is.numeric( xmin ) & is.numeric( xmax ) & xmin < xmax )
     
        xgrid <- seq( xmin, xmax, length = N )
   } # end of 'if( (!is.null( xmin )) & (!is.null( xmax )) ){...'

    
   if( is.null( xmin ) + is.null( xmax ) == 1 ){ # only one was provided.
       stop('error in pia(): if "xname" is specified, then both  "xmin" & "xmax" must be specified or neither can be specified.')
   } # end of 'if( is.null( xmin ) + is.null( xmax ) == 1 ){ # only one was provided'
   
   
   ## If neither of xmin & xmax is given, try to get a grid from 'xname'.
   if( ( is.null( xmin ) | is.null( xmax ) ) ){
     
       if( is.null( xname ) ){
        ## This is the case in which *NONE* of xmin, xmax, or xname are given
        ## or found (since "xname" can be determined from the formula in a section above):
         stop('error in pia(): must provide either (1) both "xmin" and "xmax" or (2) "xname"') 
       } 

       stopifnot( is.character( xname ) & xname %in% colnames(local_modeling_data_frame) ) 
      
       xgrid <- sort( unique( local_modeling_data_frame[[xname]] ) ) %>% ## sort unique values
                  ( function(x) x[!is.na(x)] )                       %>% ## discard NA's      
                  ( function(x) seq( min(x), max(x), length = N ) )      ## return evenly spaced grid spanning the unique values.
  
   } ## end of 'if( ( is.null( xmin ) | is.null( xmax ) ) &  ){...'



   ## Set up grid of abcissa values for numerical integration.
   
   Xp <- ## Fixed bug here that mis-ordered the group_name factor levels; we want them to stay the same.
     local_modeling_data_frame   %>%
     dplyr::select( !!dplyr::sym( groupname ) ) %>%
     unlist()                    %>%
     levels()                    %>%
     factor( levels = ( . ) )    %>%
     expand.grid( xgrid )         

   colnames( Xp ) <- c( groupname, xname )
   Xp %<>% dplyr::arrange( !!dplyr::sym( groupname ), !!dplyr::sym( xname ) )
   
   ## Set up a group-contrast matrix
   ##
   
   if( contrast == 'Identity' ){
               contr_mat   <- diag(nlevels(Xp[[groupname]]))
     rownames( contr_mat ) <- levels( Xp[[groupname]] )
   } else{ ## alternative to 'if( contrast == 'Identity' ){...'

       if( contrast == 'Dunnett' ){
         ## This enables switching the Dunnett test reference group.
         if( is.null( reference_Dunnett ) ){
             reference_Dunnett = levels( Xp[[groupname]] )[1]
         }
         contrMat_base <- match( reference_Dunnett, levels( Xp[[groupname]] ) )
         if( is.na( contrMat_base ) ){
           stop( 'error in pia(): "reference_Dunnett" value ', reference_Dunnett, ' not matched' )
         }
       } else{ ## alternative to 'if( contrast == 'Dunnett' ){...'
           contrMat_base <- 1
       }
           
       contr_mat <- try(
                      Xp[[ groupname ]] %>%
                    ( function(XX) structure( rep( 0, nlevels( XX ) ), names = levels( XX ) ) ) %>%
                      multcomp::contrMat( type = contrast, base = contrMat_base ),
                      ##
                      silent = TRUE # many errors here get fixed in the  'if( !is.null( custom_contrast ) ){...' section below.
                        )
     
       ## A common error is a non-"Identity" request with only 1 group:
       if( grepl( 'fewer than two groups',  contr_mat[[1]] ) ){
       
           stop( 'error in pia(): with only one group, must set "contrast" to "Identity"' )
      
       } # end of "if( grepl('less than two groups', contr_mat[[1]] ) ){...
     
   } # end of 'else{...'



   if( !is.null( custom_contrast ) ){
   ## check whether custom_contrast is *not* NULL (its default), meaning that a
   ## custom contrast was provided.  In this case, ignore the contrast matrix
   ## derived from a character string name (e.g., "Identity", "Dunnett") and
   ## use the one provided instead:

     ## Is custom_contrast a matrix?
     if( !is.matrix( custom_contrast ) ){
       stop('error in maeve:::pia(): "custom_contrast" must be NULL or a matrix' )
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
       stop( 'error in maeve:::pia(): column names of "custom_contrast" do not match current group names; ',
             'group names may have been abbreviated (check "abbreviate_n" parameter); current group name values are:\n\n',
              stop_msg_string
            )
     } ## end of 'if( any( is.na( group_index ) ) ){...'

       
     ## List with
     ## (1) row names equal to the row names provided in custom_contrast, and
     ## (2) column names equal to ALL the fixed effect LME estimates
     dimnames_linfct_gam <-
       list( contrast.names = rownames( custom_contrast ),
                group.names = levels( Xp[[groupname]] )
            )

     ## Make a matrix of zeros from the name vector lengths in "dimnames_linfct_lmer":
     contr_mat   <- matrix(0, nrow = nrow( custom_contrast ), ncol = nlevels(Xp[[groupname]])) # correctly sized matrix of zeros.

     dimnames( contr_mat ) <- dimnames_linfct_gam ## assign row names from custom_contrast and column names from treatment groups.
     contr_mat[,group_index] <- custom_contrast ## splice in contrasts matched by group_index; the rest will remain zeros.

   } ### end of 'if( !is.null( custom_contrast ) ){...'

    


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


   ## Check that required arguments were passed or exist in options.
   if( basis_choice == 'tent' ){
       stopifnot( !is.null( break_points ) )
   }
    
   if( basis_choice == 'poly' ){
       stopifnot( !is.null( degree ) & !is.null( maeve_options('poly_object') ) )
   }


   ## In obtaining a design matrix for either a piecewise linar or a simple polynomial basis,
   ## we need to mimic the nice functionality of mgcv with the "type = 'lpmatrix'" option within predict(),
   ## but manually, with our own function maeve::construct_design_matrix_from_basis().
   ##
   ## The "mgcv" version looks like this:
   ## Xs <- predict( model, type = "lpmatrix", newdata = Xp ) # "Xp evaluated at splines" --> "Xs"
   ##   
   Xs <-
     Xp %>% 
     construct_design_matrix_from_basis( basis_choice = basis_choice, # 'tent' for piecewise linear or 'poly' for polynomial
                                           group_name = groupname,
                                               x_name = xname,
                                         break_points = break_points, # used if basis_choice = 'tent'
                                          poly_degree = degree,       # used if basis_choice = 'poly'
                                       use_poly_coefs = TRUE          # used if basis_choice = 'poly'
                                        )

   
   if( subtract_starting_value ){
   ## 
   ## We will need two different blocks for making the "t0" baseline matrix for the two
   ## different cases of the basis being "tent" vs. "poly", unfortunately.  The issue is
   ## that in the "tent" case, the basis does not get centered or scaled depending on the
   ## the 'x'-values provided, but in the "poly" case it does.  Basically, the 'tent' case
   ## is relatively quick and easy, but in the 'poly' case, we need to provide the 'coefs'
   ## argument to the 'poly()' call based on the *original* 'poly()' call, which *should*
   ## be stored in maeve_options("poly_object") -- if it's not, we have a real problem.     

     if( basis_choice == 'tent' ){
       ## This will be used to define "Xs_t0" below; "Xs_t0" will in turn be used to subtract out each group's baseline fitted value.
       Xp_t0 <-  # Fixed bug that mis-ordered the group_name factor levels; we want them to stay the same.
       local_modeling_data_frame   %>%
       dplyr::select( !!dplyr::sym( groupname ) ) %>%
       unlist()                    %>%
       levels()                    %>%
       factor( levels = (.) )      %>% # including "levels = ( . )" should keep groups the same.
       expand.grid( rep(min(xgrid), N) )

       ##   
       ## The data.frame Xp_t0 should now have a nlevels( group_name ) blocks of N initial time values each.       
       ## now, take this 'Xp_t0' and name & arrange it properly:
       colnames( Xp_t0 ) <- c( groupname, xname )
       Xp_t0 <- Xp_t0 %>% dplyr::arrange( !!dplyr::sym( groupname ), !!dplyr::sym( xname ) )
       ##

       ## Use the 'Xp_t0' matrix as input to maeve::construct_design_matrix_from_basis() to
       ## get a 'baseline' design matrix with the 'tent' basis:     
       Xs_t0 <-
         Xp_t0 %>%
         construct_design_matrix_from_basis( basis_choice = basis_choice,
                                               group_name = groupname,
                                                   x_name = xname,
                                             break_points = break_points
                                            )
     
     } # end of 'if( basis_choice == 'tent' ){...'



     
     if( basis_choice == 'poly' ){ 

       ## We need to make a design matrix using the 'poly' basis_choice,
       ## but with the "coefs" option in poly() set to use the scaling coefficients
       ## in the poly() object from the fitted data.  Then, we need to make a matrix
       ## taking the rows corresponding to the minimum time point from each group and
       ## make a data.frame with "N" such rows for each group (so N * nlevels( group ) total),
       ## then sort by group.

       ## inelegant hack for next 6-7 lines...
       ind_poly_start_point <- Xp[[xname]] <= min( xgrid ) # TRUE for rows with the minimum time-range value.
       if( sum( ind_poly_start_point ) != nlevels( Xp[[groupname]] ) ){
         stop('error in maeve:::pia(): Xp_t0 construction for "poly" option cannot find an appropriate number of initial time points across groups')
       }
       tmp <- vector( 'list', N ) # throw-away list with N = # grid point 
       for( jj in 1:N ){ tmp[[ jj ]] <- Xp[ ind_poly_start_point, ] }   
       Xp_t0 <- do.call( 'rbind', tmp )

       
       ### The data.frame Xp_t0 should now have a nlevels( group_name ) blocks of N initial time values each.       
       ### now, take this hacked-together 'Xp_t0' and name & arrange it properly:
       colnames( Xp_t0 ) <- c( groupname, xname )
       Xp_t0 <- Xp_t0 %>% dplyr::arrange( !!dplyr::sym( groupname ), !!dplyr::sym( xname ) )

       ### Get the baseline spline-basis matrix, with scaling from the original poly() object via 'use_poly_coefs = TRUE'.
       Xs_t0 <-
         Xp_t0 %>%
         construct_design_matrix_from_basis( basis_choice = basis_choice,
                                               group_name = groupname,
                                                   x_name = xname,
                                              poly_degree = degree,
                                           use_poly_coefs = TRUE # Use scaling factors from already-fit poly() basis, stored in maeve_options("poly_object").
                                              ) 
       
       
     } # end of 'if( basis_choice == 'poly' ){...'
     
     Xs <- Xs - Xs_t0  # subtract out the baseline spline value for each group.
     
   } ## end of 'if( subtract_starting_value ){...'


    
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
    

     ## We need to change the "predict()" call seen in leia() to "construct_design_matrix_from_basis()" in the next lines,
     ## since the "predict()" function with type = "lpmatrix" does not work with lmerMod objects.      


     Xs0 <-
       Xp0 %>%
       construct_design_matrix_from_basis( basis_choice = basis_choice,
                                             group_name = groupname,
                                                 x_name = xname,
                                           break_points = break_points,
                                            poly_degree = degree,
                                         use_poly_coefs = TRUE
                                          )


     Xs1 <-
       Xp1 %>%
       construct_design_matrix_from_basis( basis_choice = basis_choice,
                                             group_name = groupname,
                                                 x_name = xname,
                                           break_points = break_points,
                                            poly_degree = degree,
                                         use_poly_coefs = TRUE
                                            )

     Xs <- ( Xs1 - Xs0 ) / dx_mat # matrix of approximate first derivatives in terms of spline basis functions.

     Xp <- Xp %>% dplyr::mutate( pred_gam_deriv1 = c( Xs %*% ( model %>% (lme4::fixef)() %>% matrix( ncol = 1 ) ) ) )
     
   } ## end of 'if( derivative == 1 ){ ...'

    
   linfct_mat <- kronecker( contr_mat, simpson_coef_one_group, make.dimnames = TRUE ) %*% Xs 

    
   ## "kronecker(..., make.dimnames = TRUE )" was introducing a colon (":") into all the rownames; dropped here.
   rownames( linfct_mat ) %<>% trim_final_colon()
   
   ## The right-hand side of the hypothesis is the 'rhs' argument to glht().
   if( is.null( glht_rhs ) ) {
       glht_rhs <- rep(0, nrow( linfct_mat ) )
   }
    
   if( length( glht_rhs ) != nrow( linfct_mat ) ){
       stop( 'error in pia(): glht_rhs should be of length', nrow( linfct_mat ), 'but is of length', length( glht_rhs ) )
   }

    
   ## The actual numerical integration will now be performed by glht(),
   ## since we've re-expressed the numerical integration problem as one
   ## of evaluating a linear combination of regression model parameters.   
   if( ! return_list ){
       return( multcomp::glht( model, linfct_mat, rhs = glht_rhs, alternative = glht_alternative ) )
   } else{ ## return a bunch of other stuff as well, in a list:
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

    
} # end of function definition for  "pia()"
