#' Check various characteristics of the study data frame before fitting longitudinal models.
#'
#' @author Bill Forrest <forrest@@gene.com>
#'
#' @param input_data_frame a data.frame with raw data from DIVOS.
#' @param study_id_name character string with name of study (usually a 4- or 5-digit number).
#' @param studyProtocol_name character string with name of study protocol (e.g., a LASAR protocol).
#' @param return_all_columns logical: return all the columns in the processed data.frame, or just a minimally required subset?
#' @param converted_factor_order character string:  either 'observed' or 'alphanumeric' for whether fields (usually numeric or character) should be leveled in order of values observed or by unique alphanumeric value.
#' @param order_rows logical: Should rows in returned data.frame be ordered by group, then ID, then time?
#' @param freeze_factor logical: Should factors levels be set to the order observed in the returned data.frame? 
#' @param autoconvert_types logical.  Automatically convert (1) "character" for group_name to "factor", (2) "integer" or "numeric" for subject_ID to factor(character()), (3) "integer" for x_name to "numeric"?
#' @param print_warnings    logical.  Print warnings if automatic conversions are taking place?
#' @param return_unique     logical.  Return only the unique rows based on the columns extracted?
#' @param group_name              character column name for the group name factor
#' @param subject_ID              character column name for the subject name factor
#' @param x_name                  character column name for the x-axis / time field
#' @param endpoint_name           character column name for the x-axis / time field
#' @param reference_Dunnett       character string with the group factor level to use as Dunnett reference.  If NULL, the first level should be used.
#' @param progress     logical. Print out various progress messages from the functions.
#' @param abbreviate_n numeric. Maximum number of characters in a group name.
#' 
#' @return An R data.frame
#'
#' @examples
#'    cat('exmaples in dontrun{} code block.')
#'    \dontrun{
#'    ### (1) Will generate a (typically harmless) warning that 'x' is class 'integer',
#'    ###     but converted to 'numeric':
#'    fake_data_01 = expand.grid( ID = paste0('ID_0',1:6) , x = as.integer( 1:5 ) )
#'    fake_data_01 = dplyr::arrange( fake_data_01 , ID )
#'    fake_data_01 = cbind( fake_study_id = 'fake',
#'                          grp = rep( c('grp_01','grp_02'), each = 15 ), 
#'                          fake_data_01, 
#'                          resp = rnorm(30), 
#'                          dreck = 'drop_this!' )
#'    checked_data_01 = check_study_data_frame( 
#'                          fake_data_01, 
#'                          group_name = 'grp', 
#'                          subject_ID = 'ID', 
#'                          x_name = 'x', 
#'                          endpoint_name = 'resp', 
#'                          study_id_name = 'fake_study_id' )
#'
#'    ### (2) Avoid the warning by creating 'x' as numeric instead of integer:
#'    fake_data_02 = fake_data_01;
#'    fake_data_02$x <- as.numeric( fake_data_02$x ) # explicitly set to 'numeric'
#'    checked_data_02 = check_study_data_frame( 
#'                          fake_data_02,
#'                          group_name = 'grp',
#'                          subject_ID = 'ID',
#'                          x_name = 'x',
#'                          endpoint_name = 'resp',
#'                          study_id_name = 'fake_study_id' )
#'  }
#'   
#' @author Bill Forrest \email{forrest@@gene.com}
#'
#' @references \url{www.r-project.org}
#'
#' @keywords data.frame
#' @export
#'
check_study_data_frame <-
  function( input_data_frame,
            study_id_name      =  NULL, # optional character string matching a column in input_data_frame
            studyProtocol_name =  NULL, # optional character string matching a column in input_data_frame
            return_all_columns = FALSE, ### if FALSE, return a minimal set of columns.
            converted_factor_order = c( 'observed', 'alphanumeric' ), # only for fields converted to factor.
            order_rows         = TRUE,
            freeze_factor      = TRUE,           
            autoconvert_types  = TRUE, ## Allow low-risk conversion of required types (e.g., "integer" to "numeric" for time)?
            print_warnings     = TRUE, ## Print a warning if converting types?
            return_unique      = TRUE, ## Keep only unique rows (rows of interest are sometimes repeated in data base to include extra values from rows not of direct interest).
            ##
            group_name         = maeve_options("group_name"),
            subject_ID         = maeve_options("subject_ID"),
            x_name             = maeve_options("x_name"),
            endpoint_name      = maeve_options("endpoint_name"),
            reference_Dunnett  = maeve_options("reference_Dunnett"),
            progress           = maeve_options("progress"),
            abbreviate_n       = maeve_options("abbreviate_n")
           ){ 


  if( progress ){
    cat( '\n\n' )
    cat( 'Requested progress updates from "maeve::check_study_data_frame()":\n\n' )
    cat( 'Progress_Message:\n' )      
    cat( 'Checking that a data.frame and required required fields were provided.\n\n' )
  }
    
  ### Check that we actually got a data.frame:
  stopifnot( is.data.frame( input_data_frame ) )

  ### Check that the required column names match to columns in the data frame provided:
  required_DF_colnames <- sapply( list(  group_name, subject_ID, x_name, endpoint_name ), as.character )
  for( name_to_confirm in required_DF_colnames ){
    if ( ! name_to_confirm %in% colnames( input_data_frame ) )
      stop( 'error in maeve::check_study_data_frame(): ',
             name_to_confirm,
            ' not found in column names of the data frame provided'
           )
  }


  ### If a field is converted from character or numeric to factor, how should the
  ### levels of the factor be ordered?  Two options available:
  ### 'observed'     -->  XX <- factor( XX, levels =       unique( as.character( XX ) )   )
  ### 'alphanumeric' -->  XX <- factor( XX, levels = sort( unique( as.character( XX ) ) ) )
  ###
  converted_factor_order = match.arg( converted_factor_order )
  
  raw_DF <- input_data_frame 

  if( progress ){
    cat( 'Progress_Message:\n' )      
    cat( 'Checking class types of required fields, with auto-conversion if "autoconvert_types" is TRUE.\n\n' )
  }
  
  if( autoconvert_types ){ 

    ### group_name:
    if( !is.factor( raw_DF[ , group_name ] ) ){
      
       group_name_levels <-
       switch( converted_factor_order,
                  observed =       unique( as.character( raw_DF[ , group_name ] ) ),
              alphanumeric = sort( unique( as.character( raw_DF[ , group_name ] ) ) )
              )
       
       raw_DF[ , group_name ] <- factor( raw_DF[ , group_name ], levels = group_name_levels )
       
       if( print_warnings )
           warning( paste('Converting ', group_name, 'to factor in maeve::check_study_data_frame().') )
       }
    
    ### subject_ID:     
    if( !is.factor( raw_DF[ , subject_ID ]   ) ){

       subject_ID_levels <-
       switch( converted_factor_order,
                  observed =       unique( as.character( raw_DF[ , subject_ID ] ) ),
              alphanumeric = sort( unique( as.character( raw_DF[ , subject_ID ] ) ) )
              )
      
       raw_DF[ , subject_ID ] <- factor( raw_DF[ , subject_ID ], levels = subject_ID_levels )
       
       if( print_warnings )
         warning( paste('Converting ', subject_ID, 'to factor in maeve::check_study_data_frame().') )
     }
    
    ### x_name (e.g., time on study):
    if( !is.numeric( raw_DF[ , x_name ] | is.integer ( raw_DF[ , x_name ] ) ) ){

       original_x_name_class <-   class( raw_DF[ , x_name ] )
       raw_DF[ , x_name ] <- as.numeric( raw_DF[ , x_name ] )
      
       if( print_warnings & original_x_name_class != 'numeric' )
         warning( paste('Converting ', x_name, 'from', original_x_name_class, 'to numeric in maeve::check_study_data_frame().') )
     }
    
    ### endpoint_name:
    if( !is.numeric( raw_DF[ , endpoint_name ] | is.integer ( raw_DF[ , endpoint_name ] ) ) ){

       original_endpoint_name_class <-   class( raw_DF[ , endpoint_name ] )
       raw_DF[ , endpoint_name ] <- as.numeric( raw_DF[ , endpoint_name ] )
      
       if( print_warnings & original_endpoint_name_class != 'numeric' )
         warning( paste('Converting ', endpoint_name, 'from', original_endpoint_name_class, 'to numeric in maeve::check_study_data_frame().') )
     }
    
  } # end of 'if( autoconvert_types ){...' 

  
  
   ### logical checks that the four required columns are now the correct types:
   stopifnot( sapply( raw_DF[, required_DF_colnames], class ) == c('factor','factor','numeric','numeric') )

   if( progress ){
     cat( 'Progress_Message:\n' )      
     cat( paste0(
                 'Confirmed that required fields\n',
                 '"group_name", "subject_ID", "x_name", and "endpoint_name"\n',
                 'are now types\n "factor", "factor", "numeric", and "numeric"\n',
                 'respectively.\n\n'
                 )
        )
   }

  
  
  ### Check that 'subject_ID' is nested within 'group_name'
  ### (i.e., a single subject_ID level may not be in 2+ different
  ### group_name levels.)
  ###
  check_that_ID_is_nested_in_group <- 
    raw_DF %>%
    dplyr::select( !!dplyr::sym( group_name ), !!dplyr::sym( subject_ID ) ) %>%
    dplyr::rename(  'group' = !!dplyr::sym( group_name ), 'ID' = !!dplyr::sym( subject_ID ) ) %>%
      unique() %>%        
      plyr::ddply( .var = 'ID',
                   .fun = function(XX, group_name = eval( group_name ), subject_ID = eval( subject_ID ) ){
                     XX <- unique(droplevels( XX ))
                     if( nlevels( XX[,'group'] ) > 1 ){
                       return( data.frame( XX,
                                           check = paste('** CONFLICT **: Single ID in multiple groups not allowed.',
                                                         'Group level',
                                                          levels(XX$group),
                                                         'has ID level',
                                                          levels(XX$ID)
                                                        )
                                          )
                              )
                           } else{
                             return( data.frame( XX, check = 'OK') ) ### unique group_name for this subject_ID -- this passes the test.
                           }
                   } # end of '.fun = function(XX, group_name = eval( group_name ), subject_ID = eval( subject_ID ) ){...'
                  ) # end of ' plyr::ddply( .var = 'ID',...'

      
  ## Confirm that the "check" is OK for each ID level:
  if( any( check_that_ID_is_nested_in_group$check != 'OK' ) ){
    check_that_ID_is_nested_in_group %>%
      dplyr::select(check) %>%
      dplyr::filter( check != 'OK' ) %>%
      print
    ###
    stop( 'error in maeve::check_study_data_frame(): ',
          'each identifier must belong to exactly one group_name'
         )
  } ### end of 'if( any( check_that_ID_is_nested_in_group$check != 'OK' ) ){...'

    
  if( progress ){
    cat( 'Progress_Message:\n' )      
    cat( 'Confirmed subject_ID levels are nested within group_name levels.\n\n' )
  }
    


  
  ### The user provided a data.frame but *may* have either of the
  ### "study protocol name" or the study identification number
  ### (typically a 4- or 5-digit number). If these are provided and
  ### are single-value factors, we add them in here.

  suggested_DF_colnames <- NULL
  
  ### studyProtocol:
  if( !is.null( studyProtocol_name ) ){

    if( progress ){
       cat( 'Progress_Message:\n' )      
       cat( 'Processing "studyProtocol_name" value provided.\n\n' )
     }
    
    if(!is.character(studyProtocol_name)){
      if( ! autoconvert_types ){
        stop( 'error in maeve::check_study_data_frame(): ',
               studyProtocol_name,
              ' must be type character if it is provided',
              ' and "autoconvert_types = FALSE".'
             )
      } ### end of 'if( ! autoconvert_types ){...'
      if( autoconvert_types ){
        studyProtocol_name <- as.character(studyProtocol_name)
        warning(paste('Converting',studyProtocol_name, 'to character in maeve::check_study_data_frame().'))
      } 
    } ### end of 'if(!is.character(studyProtocol_name)){...'

    
    ## Check that the name provided is a column in the data frame:    
    if( ! studyProtocol_name %in% colnames( raw_DF ) ){
      stop(
           'error in maeve::check_study_data_frame(): ',
           'the name ',
           studyProtocol_name, ' ',
           'provided for "studyProtocol_name" ',
           'does not match any column in the data frame provided.'
           )
    } ## end of 'if( ! studyProtocol_name %in% colnames( raw_DF ) ){...'

    ## Check that the column matching studyProtocol_name is a factor.  If not,
    ## convert it to a factor unless autoconvert_types == FALSE.  Also, check
    ## that it has a single level.
    if( !is.factor( raw_DF[ , studyProtocol_name ] ) ){
        if( autoconvert_types ){
          raw_DF[ , studyProtocol_name ] <- as.factor( raw_DF[ , studyProtocol_name ] )
          if( print_warnings ){
            warning( paste('Converting ', studyProtocol_name, 'to factor in maeve::check_study_data_frame().') )
          } else{ ### Scenario: "autoconvert_types == FALSE", but "raw_DF[ , studyProtocol_name]" is *NOT* a factor.
                   stop( 'error in maeve::check_study_data_frame(): ',
                          studyProtocol_name, ' ',
                          'must be a factor if "autoconvert_types = FALSE" is used.'
                       ) 
                } # end of 'else{ ### Scenario:...'
          
        } # end of 'if( autoconvert_types ){...'     
    } # end of 'if( !is.factor(raw_DF[ , studyProtocol_name ]) ){...'

      
    ## Confirm that this factor has exactly one level:
    if( nlevels( raw_DF[ , studyProtocol_name ] ) != 1 ){
      stop_msg_string <- paste(' ', paste( nlevels( raw_DF[ , studyProtocol_name ] ), collapse = ' ' ), ' ')
      stop( 'error in maeve::check_study_data_frame():',
             studyProtocol_name,
            'is a factor with',
             stop_msg_string,
            'levels, but is allowed only 1 level.'
           )
    } ### end of 'if( nlevels( raw_DF[ , studyProtocol_name ] ) != 1 ){...'
      
    ## If it has passed checks, add it to the column names:
    suggested_DF_colnames <- c( studyProtocol_name , suggested_DF_colnames  )

    if( progress ){
      cat( 'Progress_Message:\n' )      
      cat( paste('Column name',studyProtocol_name,'provided for "studyProtocol_name" added to columns to be returned.\n\n' ) )
    }
 
  } ## end of 'if( !is.null( studyProtocol_name ) ){...'






  

  ## study_id:
  if( !is.null( study_id_name ) ){

    if( progress ){
      cat( 'Progress_Message:\n' )            
      cat( 'Processing "study_id_name" value provided.\n\n' )
    }
    
    if(!is.character(study_id_name)){
      if( ! autoconvert_types ){
        stop( 'error in maeve::check_study_data_frame(): ',
               study_id_name, ' ',
              'must be type character if it is provided ',
              'and "autoconvert_types = FALSE".'
             )
      } ### end of 'if( ! autoconvert_types ){...'
      if( autoconvert_types ){
        study_id_name <- as.character(study_id_name)
        warning( paste( 'Converting', study_id_name, 'to character in maeve::check_study_data_frame().' ) )
      } 
    } ## end of 'if(!is.character(study_id_name)){...'

    
    ## Check that the name provided is a column in the data frame:    
    if( ! study_id_name %in% colnames( raw_DF ) ){
      stop( 'error in maeve::check_study_data_frame(): ',
            'the name ',
            study_id_name, ' ',
            'provided for "study_id_name" ',
            'does not match any column in the data frame provided.'
           )
    } ## end of 'if( ! study_id_name %in% colnames( raw_DF ) ){...'

    ## Check that the column matching study_id_name is a factor.  If not,
    ## convert it to a factor unless autoconvert_types == FALSE.  Also, check
    ## that it has a single level.
    if( !is.factor( raw_DF[ , study_id_name ] ) ){
        if( autoconvert_types ){
          raw_DF[ , study_id_name ] <- as.factor( raw_DF[ , study_id_name ] )
          if( print_warnings ){
            warning( paste('Converting ', study_id_name, 'to factor in maeve::check_study_data_frame().') )
          } else{ ### Scenario: "autoconvert_types == FALSE", but "raw_DF[ , study_id_name]" is *NOT* a factor.
              stop( 'error in maeve::check_study_data_frame(): ',
                     study_id_name, ' ',
                    'must be a factor if "autoconvert_types = FALSE" is used.'
                   ) 
                } # end of 'else{ ### Scenario:...'
          
        } # end of 'if( autoconvert_types ){...'     
    } # end of 'if( !is.factor(raw_DF[ , study_id_name ]) ){...'

    ## Confirm that this factor has exactly one level:
    if( nlevels( raw_DF[ , study_id_name ] ) != 1 ){
      stop_msg_string <- paste(' ', paste( nlevels( raw_DF[ , study_id_name ] ), collapse = ' ' ), ' ')
      stop( 'error in maeve::check_study_data_frame():',
             study_id_name,
            'is a factor with',
             stop_msg_string,
            'levels, but is allowed only 1 level.'
           )
    } ### end of 'if( nlevels( raw_DF[ , study_id_name ] ) != 1 ){...'

    ## If it has passed checks, add it to the column names:
    suggested_DF_colnames <- c( study_id_name , suggested_DF_colnames  )

    if( progress ){
      cat( 'Progress_Message:\n' )      
      cat( paste('Column name',study_id_name,'provided for "study_id_name" added to columns to be returned.\n\n' ) )
    }
  
  } # end of 'if( !is.null( study_id_name ) ){...'

      
    
  ## Check that "reference_Dunnett" is a character string in the levels of "raw_DF[, group_name]".
  ##
  group_name_levels <- levels( raw_DF[, group_name ] )
  
  if( is.null( reference_Dunnett ) ){
      reference_index   <- 1
      reference_Dunnett <- group_name_levels[reference_index] # use first level if no other guidance is given.
  } # end of ' if( is.null( reference_Dunnett ) ){...'

  if( ! is.null( reference_Dunnett ) ){
      
      reference_index <-  match( reference_Dunnett, group_name_levels )

      if( is.na( reference_index ) ){
        stop_msg_string <- paste( ' \n', paste( group_name_levels, collapse = '\n' ), ' \n' )
        stop('error in maeve::check_study_data_frame():\n\n   ',
              reference_Dunnett,
             ' \n\n',
             ' not found in the group_name factor levels:\n\n',
              stop_msg_string,
             '\n\n'
             ) 
      } # end of 'if( is.na( reference_index ) ){...'
  } # end of 'if( ! is.null( reference_Dunnett ) ){...'

  ## (0) Copy raw_DF into a "to be cleaned" copy:
  clean_DF_full_xrange <- raw_DF
    
  ## (1) Re-set factors of the group-name variable so that the
  ## one with 'base_level' index is first:

  if( progress & reference_index != 1 ){
    cat( 'Progress_Message:\n' )
    cat( paste0('Resetting levels of factor ',
                group_name,
                ' so that\n   ',
                reference_Dunnett,
                '\n',
                'is first.\n\n'
                )
        )
  } ### end of 'if( progress & base_level != 1 ){...'
    
  clean_DF_full_xrange[ , group_name ] <-
      factor( clean_DF_full_xrange[ , group_name ],
              levels = c( reference_Dunnett, setdiff( group_name_levels, reference_Dunnett ) )
             ) 

  ## (2) Abbreviate levels of group name to the specified number of characters:
  clean_DF_full_xrange[ , group_name ] <-
      (abbreviate_factor( clean_DF_full_xrange[ , group_name ], abbreviate_n ))

    
  ## (3) Remove redundant rows.
  if( return_unique && ( nrow(unique(clean_DF_full_xrange)) < nrow(clean_DF_full_xrange) ) ){
      
      if( progress ){
        cat( 'Progress_Message:\n' )
        cat(
            paste('Removing',
                   nrow(clean_DF_full_xrange) - nrow(unique(clean_DF_full_xrange)),
                  'non-unique rows out of',
                   nrow(clean_DF_full_xrange),
                  'total rows in call to maeve::check_study_data_frame().\n\n'
                 )
            )
            }

      clean_DF_full_xrange <- unique( clean_DF_full_xrange )
      
  } # end of 'if( return_unique && ( nrow(unique(clean_DF_full_xrange)) < nrow(clean_DF_full_xrange) ) ){...'


    
  
  ## Should the rows be rearranged so that in order of group, then ID, then time?
  if( order_rows ){
    clean_DF_full_xrange <-
    clean_DF_full_xrange %>%
    dplyr::arrange( !!dplyr::sym( group_name ),
                    !!dplyr::sym( subject_ID ),
                    !!dplyr::sym( x_name )
                   )
  } 

  ## Should the factors in the data.frame be fixed (perhaps reset) at their observed order?
  if( freeze_factor ){
    clean_DF_full_xrange <-
    clean_DF_full_xrange %>%
        maeve::freeze_factor_levels()
  }

    
  if( return_all_columns ){

    if( progress ){
      cat( 'Progress_Message:\n' )
      cat( 'SUCCESS: Returning all columns of checked data frame for analysis in maeve.\n\n')
    } 

    if( FALSE && return_unique ){
      ## already accomplished above (in the case in which we're returning all columns -- otherwise we need this check).
      nrow_total  <- nrow(         clean_DF_full_xrange   )
      nrow_unique <- nrow( unique( clean_DF_full_xrange ) )
        if( nrow_unique < nrow_total ){
            if( progress ){
               cat( 'Progress_Message:\n' )      
               cat( paste( 'NOTE: Returning', nrow_unique, 'unique rows out of', nrow_total, 'rows.\n\n' ) )
             } # end 'if( progress ){...'
            clean_DF_full_xrange <- unique( clean_DF_full_xrange )
        } # end 'if( nrow_unique < nrow_total ){...'
    } # end 'if( FALSE && return_unique ){...'
    
    return( clean_DF_full_xrange )
    
  } # end of 'if( return_all_columns ){...'


      
  if( ! return_all_columns ){

    if( progress ){
      cat( 'Progress_Message:\n' )
      cat( 'SUCCESS: Returning selected columns of checked data frame for analysis in maeve.\n\n')
    } 

    clean_DF_full_xrange <- clean_DF_full_xrange[ , c( suggested_DF_colnames, required_DF_colnames ) ]
    
    if( return_unique ){ 
      nrow_total  <- nrow(         clean_DF_full_xrange   )
      nrow_unique <- nrow( unique( clean_DF_full_xrange ) )
      if( nrow_unique < nrow_total ){
          if( progress ){
            cat( 'Progress_Message:\n' )
            cat( paste( 'NOTE: Returning', nrow_unique, 'unique rows out of', nrow_total,
                        'rows. Non-uniqueness may be due to returning only a subset of columns',
                        'from maeve::check_study_data_frame().\n\n'
                       )
                )
           }
          clean_DF_full_xrange <- unique( clean_DF_full_xrange )
      }
    } ## end of 'if( return_unique ){ ...'
    
    return( clean_DF_full_xrange ) 
    
  } # end of 'if( ! return_all_columns ){ ...'

      
  
 } # end of function maeve::check_study_data_frame()

