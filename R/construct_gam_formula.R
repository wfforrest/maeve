#' Make a gam formula with the basis_vec number provided.
#'
#' Takes a number for the spline basis_vec value and pastes it
#' together with character strings giving the parts of the
#' formula before and after the basis_vec number, then returns the
#' pasted string as a formula.  This utility function circumvents
#' some specific function scope issues that arise when fitting
#' the gam model with a passed parameter number of basis_vecs.
#'
#' @param basis_vecs_value numeric integer value to be used for number basis_vecs.
#' @param before_basis_vecs_value character string with the gam formula before the basis_vec number.
#' @param  after_basis_vecs_value character string with the gam formula  after the basis_vec number.
#'
#' @return a formula object that can be passed to gamm4::gamm4(), which calls mgcv::gam().
#'
#' @examples
#'   ## avoid a ':::'-related warning in package check by not running next lines:
#'   \dontrun{
#'   test_gam_formula <- maeve:::construct_gam_formula( 6 )
#'   }
#'
#' @author Bill Forrest \email{forrest@@gene.com}
#'
#' @keywords character
#'
#' @seealso \code{\link{character}}
#'

construct_gam_formula <-
    function( basis_vecs_value = -1,
              before_basis_vecs_value = 'y ~ group +  s( x, bs = "tp", k =',
               after_basis_vecs_value = ', by = group )'
             ){
        ## check the "basis_vecs_value" parameter.
        stopifnot( is.numeric( basis_vecs_value )  & # is the value a numeric?
                   length( basis_vecs_value == 1 ) & # is it of length 1?
                   all.equal( round( basis_vecs_value ), basis_vecs_value ) # and not a non-integer.
                  )
        ## evaluate the basis_vecs_value, paste it with its prefix & suffix,
        ## and return the whole character string as a formula:
        formula( paste( before_basis_vecs_value, eval( basis_vecs_value ), after_basis_vecs_value ) )
    }
