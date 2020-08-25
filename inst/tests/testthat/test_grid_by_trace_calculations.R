context('grid_by_trace() function to compute subject_ID-level traces on a common grid.')

TOLERANCE <- 1e-3

test_that(
          desc = "Time-aligned data sets for vismodegib data with traces on a grid match expectations.",
###          
          code = {  

          require( magrittr )
          require(    dplyr )
          require(    maeve ) # find.package( 'maeve' )

### Load the "gold standard" answers from the internal test data directory.
### We might need to change the path on these later -- I'm not sure where
### best practices indicate they should go, but this is what I found online.
          load( '../testdata/vismo_trace_GoldStandard_data_frame.rda' )
          load( '../testdata/vismo_windowed_trace_TV_BW_GoldStandard_data_frame.rda' )

          set.seed( 20200517 )

          maeve_reset()
          
          data( vismodegib, package = 'maeve' )

          ## test 01:
          ## Obtain traces on a common grid:

          maeve_options( trace_ID = 'animalID', endpoint_name = 'TUMOR_VOLUME' )

          vismo_trace <-
              vismodegib %>%
              maeve::window_by_id( xmin = 0, xmax = 24 ) %>%
              maeve::grid_by_trace( method = 'spline' )

          
if(FALSE){
### This provides the code for the "gold standard".
### Do *NOT* run it by default, but this seems a sensible place to save it.
       stop('Stop in test_grid_by_trace_calcuations.R: Comment this line out to run the code block.')
       vismo_trace_GoldStandard_data_frame <- vismo_trace
       save(      vismo_trace_GoldStandard_data_frame, file = 'vismo_trace_GoldStandard_data_frame.rda' )
       system('mv vismo_trace_GoldStandard_data_frame.rda ../testdata/') ## if running inside the 'maeve/inst/tests/testthat/' directory.
         }

       ## unit test:  Are "vismo_5_to_15_V02" and "vismo_5_to_15_V02" identical?
       testthat::expect_equal( vismo_trace, vismo_trace_GoldStandard_data_frame, tolerance = TOLERANCE )


          ## test 02:
          ## A more complicated case study.  Find BODY_WEIGHT and TUMOR_VOLUME
          ## interpolated on a common grid of points for each animal, so you
          ## could, e.g., plot BODY_WEIGHT vs. TUMOR_VOLUME for each animal.

           vismo_TV_BW <- 
           vismodegib %>%
               dplyr::select( group_name, animalID, DAY_OF_STUDY, BODY_WEIGHT, TUMOR_VOLUME ) %>%
               dplyr::mutate( ID_day = factor( paste0( animalID, '_', maeve::lead_char(DAY_OF_STUDY) ) ) ) %>%
               tidyr::gather( key = endpoint, value = value, BODY_WEIGHT, TUMOR_VOLUME ) %>%
               dplyr::arrange( group_name, animalID, DAY_OF_STUDY ) %>%
               dplyr::mutate( trace_ID = factor( paste( animalID, endpoint, sep = '_' ) ) ) %>%
               maeve::freeze_factor_levels()

           maeve_reset()
           maeve_options( endpoint_name = 'value', subject_ID = 'trace_ID', trace_ID = 'trace_ID' )
           
           vismo_windowed_TV_BW <-
               vismo_TV_BW %>%
               maeve::window_by_id( xmin = 0, xmax = 24, method = 'spline', return_all_columns = TRUE )

           vismo_windowed_trace_TV_BW <-
               vismo_windowed_TV_BW %>% maeve::grid_by_trace( method = 'spline' ) %>%
               dplyr::mutate( endpoint = ifelse( grepl('BODY_WEIGHT', trace_ID), 'BODY_WEIGHT', 'TUMOR_VOLUME' ) %>% factor,
                              animalID = strsplit( as.character( trace_ID ), '_' ) %>%
                                         lapply( function(x)x[[1]][1] ) %>%
                                         unlist %>%
                                         factor( levels = levels( vismo_windowed_TV_BW$animalID ) )
                             ) %>%
               dplyr::select( group_name, animalID, DAY_OF_STUDY, endpoint, type, value )


if(FALSE){
### This provides the code for the "gold standard".
### Do *NOT* run it by default, but this seems a sensible place to save it.
       stop('Stop in test_grid_by_trace_calcuations.R: Comment this line out to run the code block.')
       vismo_windowed_trace_TV_BW_GoldStandard_data_frame <- vismo_windowed_trace_TV_BW
       save( vismo_windowed_trace_TV_BW_GoldStandard_data_frame, file = 'vismo_windowed_trace_TV_BW_GoldStandard_data_frame.rda' )
       system('mv vismo_windowed_trace_TV_BW_GoldStandard_data_frame.rda ../testdata/') ## if running inside the 'maeve/inst/tests/testthat/' directory.
         }

       ## unit test:  Are "vismo_5_to_15_V02" and "vismo_5_to_15_V02" identical?
       testthat::expect_equal( vismo_windowed_trace_TV_BW, vismo_windowed_trace_TV_BW_GoldStandard_data_frame, tolerance = TOLERANCE )


       maeve_reset() # reset package options to their original values.          
          
          } # end of 'code = {...'
          
        ) # end of 'test_that(...'

