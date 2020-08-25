#' Make a matrix for a linear (mixed) model with known basis functions.
#'
#' Takes a vector of x-values and a basis type ("tent" and "poly" are current options) 
#' and returns a design matrix for fitting a piecewise linear mixed effects model with 
#' one group.  Basis created uses the so-called "tent-functions".  See, e.g., 
#' Simon Wood (2017) GAM 2nd edition text, section 4.2, page 164-166.
#'
#' @param x numeric vector of values to use in forming the basis.
#' @param basis character string specifying which kind of basis to generate.
#' @param break_points numeric vector with the x-locations at which to place break points.
#' @param add_column_names logical whether to add column names to the matrix.
#' @param format_digits how many digits to retain in column names?  Used only if add_column_names is TRUE
#' @param degree numeric degree of polynomial basis, if that is selected.  Passed straight to stats::poly().
#' @param include_intercept logical whether to append an intercept term to the matrix for a polynomial basis.
#' @param use_poly_coefs logical whether to use "attr( maeve_options('poly_object'), 'coef')" to scale poly basis matrix.
#'
#' @return a matrix
#'
#' @examples
#'   ### Need to put in an actual example here.
#'   simple_tent_basis <-
#'     maeve:::make_basis_matrix( x = seq( 0, 1, length = 11 ),
#'                                basis = 'tent',
#'                                break_points = c( 0, 0.5, 1 )
#'                               )
#'   ##
#'   simple_poly_basis <-
#'     maeve:::make_basis_matrix( x = seq( 0, 1, length = 11 ),
#'                                basis = 'poly',
#'                                degree = 3
#'                               )
#'   
#'
#' @author Bill Forrest \email{forrest@@gene.com}
#'
#' @keywords matrix
#'
#' @seealso \code{\link{matrix}}
#'
make_basis_matrix <-
  function( x,
            basis             = c('tent','poly'),
            break_points      = NULL,
            add_column_names  = TRUE,
            format_digits     = 3,
            degree            = 2,
            include_intercept = FALSE,
               use_poly_coefs = FALSE        
           ){ 

        basis = match.arg( basis )

        stopifnot( all( !is.na( x ) ) & length( x ) >= 3 )

        X <- NULL
        
        if( basis == 'tent' ){
        ## Make a "tent function" basis.
        ## References:
        ## Section 4.2 of Simon N. Wood (2017) Generalized Additive Models, 2nd edition, book (p. 164-6)
        ## also, 
        ## http://people.math.aau.dk/~rw/Undervisning/Mat5ProjektE12/2.pdf
        
        if( is.null( break_points ) ){
        ## By default, give evenly-spaced break points based on a number of break_points requested.
            break_points  <- quantile( sort(unique(x)), probs = seq( 0, 1, length = number_break_points ), names = FALSE )
        }
      
        if( !( min(x) >= min(break_points) & max(x) <= max( break_points ) ) ){
          stop( 'error in maeve:::make_basis_matrix(): x-values must fall within closed range of break points' )
        } # end of 'if( !( min(x) >= min(break_points) & max(x) <= max( break_points ) ) ){...'

        ## Introduce some abbreviated notation to make the next section easier (not easy) to read:        
        bp <- break_points
        K  <- length( bp )
        X  <- matrix( NA, nrow = length( x ), ncol = K ) ### placeholder
            
        for( jj in 1:K ){ ### build up the basis functions in a loop:
              
          if( jj == 1 ){ ### left-side special case.
            X[,jj] <- ( bp[2] - x ) / ( bp[2] - bp[1] )  * ( x < bp[2] )
          }         

          if( jj > 1 & jj < K ){ ### middle-point. Note that the "tent" function is non-zero across two adjacent intervals.
            X[,jj] <- ( x - bp[jj-1] ) / ( bp[jj]   - bp[jj-1] ) * ( x > bp[jj-1] ) * ( x <= bp[jj]   ) +
                      ( bp[jj+1] - x ) / ( bp[jj+1] - bp[jj]   ) * ( x > bp[jj]   ) * ( x <= bp[jj+1] )
          }

          if( jj == K ){ ### right-side special case.
              X[,jj] <- ( x - bp[K-1] ) / ( bp[K] - bp[K-1] )  * ( x > bp[K-1] )
          }

        } # end of 'for( jj in 1:K ){...'


            
        if( add_column_names ){
        colnames( X ) <- ### Assign names to the basis functions based on the break points.  
         c( paste0( 'x_in_[', format( bp[1],           digits=format_digits ), ',', format( bp[2],           digits=format_digits ), ')' ),
            paste0( 'x_in_(', format( bp[(2:(K-1))-1], digits=format_digits ), ',', format( bp[(2:(K-1))+1], digits=format_digits ), ')' ),
            paste0( 'x_in_(', format( bp[K-1],         digits=format_digits ), ',', format( bp[K],           digits=format_digits ), ']' )
           )
        }

       }  # end of 'if( basis == 'tent' ){...'




        
        
       if( basis == 'poly' ){

       if( ! use_poly_coefs ){
         X <- poly( x, degree = degree )
       } else{
         if( is.null( maeve_options('poly_object') ) ){
           stop("error in maeve:::make_basis_matrix(): use_poly_coefs is TRUE but maeve_options('poly_object') is NULL")
         }
         ## Make a polynomial basis using the already-stored coefficient scales from a *previous* call to poly().
         ## This is most often done to subtract out the baseline value in polynomial integration in maeve:::pia().
         X <- poly( x, degree = degree, coefs = attr( maeve_options('poly_object'), 'coef') )         
       }

       if( include_intercept ){
          X <- cbind( 1 / sqrt( length( x ) ), X )
       } 

       if( add_column_names ){
         ## Assign names to the basis functions based on degree
         if( !include_intercept ){
          colnames( X ) <- paste0( '_degree_', maeve::lead_char( 1:degree, padExtra = 1 ) )
         } else{
          colnames( X ) <- paste0( '_degree_', maeve::lead_char( 0:degree, padExtra = 1 ) )
         }
       } # end of 'if( add_column_names ){...'
         
       
       } # end of 'if( basis == 'poly' ){...'


       return( X )
       
}
