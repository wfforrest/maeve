#' re-order factor levels in a data frame to their current observed order.
#'
#' Takes a data frame and looks at each column.  If the column is _not_ a factor,
#' it will be returned unchanged.  If the column is a factor, it will be replaced
#' with a factor having the same levels, but re-ordered to whatever their observed
#' current level in the data frame happens to be.  It re-sets all the factors except
#' those named in the "no_freeze" argument. Intended as an improvement on the older
#' "freezeFactorLevels()".
#'
#' @param x a data frame.
#' @param no_freeze character vector.  Columns of "x" named in this vector will be returned unchanged.
#'
#' @return data frame of the same size, names, and types as "x".  Factor level orders may have changed.
#'
#' @examples
#'  myDat <- data.frame(
#'              animal = factor( c('cat', 'dog', 'cat', 'bird', 'fish', 'dog', 'cat'),
#'                               levels = c('bird','cat','dog','fish')
#'                             ),
#'              covering = factor( c('hair','hair', 'hair', 'feathers', 'scales', 'hair', 'hair'),
#'                                 levels = c('feathers','hair','scales')
#'              ),
#'              feet = c(4,4,4,2,0,4,4)
#'           ) # end of data.frame 'myDat'
#'
#'
#' ### Freeze all factors
#' myNewDat <- freeze_factor_levels( myDat )
#' print( levels( myDat$animal ) )
#' print( levels( myNewDat$animal) )
#' print( levels( myDat$covering ) )
#' print( levels( myNewDat$covering) )
#'
#'
#' ### Freeze one but not another
#' myNewDat2 <- freeze_factor_levels( myDat, no_freeze = 'covering' )
#' print( levels( myDat$animal ) )
#' print( levels( myNewDat2$animal) )
#' print( levels( myDat$covering ) )
#' print( levels( myNewDat2$covering) )
#'
#'
#' ### Freeze neither factor
#' myNewDat3 <- freeze_factor_levels( myDat, no_freeze = c('animal','covering') )
#' print( levels( myDat$animal ) )
#' print( levels( myNewDat3$animal) )
#' print( levels( myDat$covering ) )
#' print( levels( myNewDat3$covering) )
#'
#' @author Bill Forrest \email{forrest@@gene.com}
#'
#' @keywords factor
#'
#' @seealso \code{\link{factor}}
#' @export
#'
freeze_factor_levels <- function(x, no_freeze = NULL){
      ## look at columns in a data.frame.  If the column is of class 'factor', then
      ## re-order the levels of that factor to their current observed order.
      ## Example of intended use: If we're re-ordering a data frame then piping it directly
      ## to a plotting function.
      if( !is.data.frame(x) ) stop('error: freeze_factor_levels() requires a data frame as input.')

      freeze <- function( a ) factor( a, levels = unique( as.character( a ) ) ) # utility function used below.

      ## If you're willing to give up "no_freeze" entirely, the rest of the function can be accomplished by:
      ##
      ##   data.frame(lapply( x, function(y){ if( is.factor(y) ){ freeze(y) } else{y}} ) )
      
      ## Make a list of the column names of the input data.frame "x".
      ## Over this list of column names, apply the following function:
      ## 
      ## (1) If the column name references a column with a factor in the data frame
      ##     and that column name is _NOT_ in the "no_freeze" character vector,
      ##     then freeze that column as a factor, with levels re-ordered to their
      ##     observed factor levels.
      ## 
      ## (2) Otherwise, the column name references a non-factor _or_ a factor named in
      ##     "no_freeze".  In either case, just return the factor as it was.
      
      out <- # Store interim output; need to re-assign variable names after this step.
         lapply( as.list( names( x ) ),
                 function( y, XX = x ){
                  if( is.factor( XX[,y] ) & !( y %in% no_freeze ) ){
                       freeze( XX[,y] ) # freeze the factor with levels by its current observed ordering.
                   } else{
                       XX[,y] # no action -- return the column as it is.
                   }
                }
               ) # end of 'lapply(...'

     names( out ) <- colnames( x ) # "out" is a list at the moment, not a data.frame
      
     return( data.frame( out ) )

     }
