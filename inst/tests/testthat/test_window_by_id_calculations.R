context('window_by_id() function to compute subject_ID-level window-ed data sets.')

TOLERANCE <- 1e-3

test_that(
          desc = "Filtered data sets for vismodegib data with interval endpoint interpolation match expectations.",
###          
          code = {  

          require( magrittr )
          require(    dplyr )
          require(    maeve ) # find.package( 'maeve' )

### Load the "gold standard" answers from the internal test data directory.
### We might need to change the path on these later -- I'm not sure where
### best practices indicate they should go, but this is what I found online.
          load( '../testdata/vismo_5_to_15_V02_GoldStandard_data_frame.rda' )
          load( '../testdata/vismo_5_to_15_method_spline_GoldStandard_data_frame.rda' )
          load( '../testdata/vismo_BW_common_day_GoldStandard_data_frame.rda' )
          
          set.seed( 20200517 )

          maeve_reset()
          
          data( vismodegib, package = 'maeve' )

          ## test 01:
          ## interpolate endpoints so that data sets are on [5, 15]:

          ## Method #1:
          vismo_5_to_15_V01 <-
              vismodegib %>%
              dplyr::mutate( y = log( 1 + TUMOR_VOLUME ) ) %>%
              ## window [5, 15]:
              ## 'VST = FALSE' below, so no variance-stabilizing transform is applied.
              ## Instead, we take the already-variance-stabilized 'y' just created,
              ## interpolate it at 'x = 5' and 'x = 15', then back-transform to get
              ## the 'TUMOR_VOLUME' values at the interval endpoints.
              maeve::window_by_id( xmin = 5, xmax = 15, endpoint_name = 'y', VST = FALSE ) %>%
              dplyr::mutate( TUMOR_VOLUME = exp( y ) - 1 ) %>%
              dplyr::select( group_name, animalID, DAY_OF_STUDY, TUMOR_VOLUME, y )


          ## Method #2:
          ## A shorter way is to just send 'TUMOR_VOLUME' to 'window_by_id()' and
          ## then pipe the output through dplyr::mutate() and create 'y' there.
          vismo_5_to_15_V02 <-
              vismodegib %>%
              maeve::window_by_id( xmin = 5, xmax = 15 ) %>%
              dplyr::mutate( y = log( 1 + TUMOR_VOLUME ) )
          
          
if(FALSE){
### This provides the code for the "gold standard".
### Do *NOT* run it by default, but this seems a sensible place to save it.
       stop('Stop in test_window_by_id_calcuations.R: Comment this line out to run the code block.')
       vismo_5_to_15_V02_GoldStandard_data_frame <- vismo_5_to_15_V02
       save(      vismo_5_to_15_V02_GoldStandard_data_frame, file = 'vismo_5_to_15_V02_GoldStandard_data_frame.rda' )
       system('mv vismo_5_to_15_V02_GoldStandard_data_frame.rda ../testdata/') ## if running inside the 'maeve/inst/tests/testthat/' directory.
         }

          ## unit test:  Are "vismo_5_to_15_V02" and "vismo_5_to_15_V02" identical?
          testthat::expect_equal( vismo_5_to_15_V01, vismo_5_to_15_V02, tolerance = TOLERANCE )
          
          ## unit test:  Is the "vismo_5_to_15_V02" data.frame we get the same as what was seen before?
          testthat::expect_equal( vismo_5_to_15_V02, vismo_5_to_15_V02_GoldStandard_data_frame, tolerance = TOLERANCE )

          
          
          ## test 02:
          ## interpolate endpoints so that data sets are on [5, 15], but use the 'spline' method:

          maeve_reset()

          ## Interpolate TUMOR_VOLUME on [5,15] as before, but
          ## use "method = 'spline'" instead of "method = 'linear'",
          ## which will make a small difference when interpolation
          ## happens at the endpoints of DAY_OF_STUDY %in% c(5,15).
          ## 
          vismo_5_to_15_method_spline <-
              vismodegib %>%
              maeve::window_by_id( xmin = 5, xmax = 15, method = 'spline' ) %>%
              dplyr::mutate( y = log( 1 + TUMOR_VOLUME ) )

if(FALSE){
### This provides the code for the "gold standard".
### Do *NOT* run it by default, but this seems a sensible place to save it.
       stop('Stop in test_window_by_id_calcuations.R: Comment this line out to run the code block.')
       vismo_5_to_15_method_spline_GoldStandard_data_frame <- vismo_5_to_15_method_spline
       save( vismo_5_to_15_method_spline_GoldStandard_data_frame, file = 'vismo_5_to_15_method_spline_GoldStandard_data_frame.rda' )
       system('mv vismo_5_to_15_method_spline_GoldStandard_data_frame.rda ../testdata/') ## if running inside the 'maeve/inst/tests/testthat/' directory.
         }


          ## unit test:  Is the "vismo_5_to_15_V02" data.frame we get the same as what was seen before?
          testthat::expect_equal( vismo_5_to_15_method_spline, vismo_5_to_15_method_spline_GoldStandard_data_frame, tolerance = TOLERANCE )


          ## test 03:
          ## Use a *very* narrow interval to estimate the endpoint value at a common day for all subjects:

          maeve_reset()
          maeve_options( endpoint_name = 'BODY_WEIGHT', xrange_norm_method = 'xrange' )

          common_day <- 14   # estimate body weight at this day for each subject.
          x_delta    <- 5e-4 # interval radius about common day.

          BW_common_day <-
              vismodegib %>%
              maeve::window_by_id( xmin = common_day - x_delta, xmax = common_day + x_delta, print_warnings = FALSE ) %>%
              maeve::auc_by_id( VST = FALSE, subtract_starting_value = FALSE ) %>%
              dplyr::select( -first_day, -last_day ) %>%
              dplyr::rename( BW_common_day = AUC ) %>%
              maeve::round_numerics( 1 ) # keep measurements at same recorded precision
              
          maeve_reset()

if(FALSE){
### This provides the code for the "gold standard".
### Do *NOT* run it by default, but this seems a sensible place to save it.
       stop('Stop in test_window_by_id_calcuations.R: Comment this line out to run the code block.')
       vismo_BW_common_day_GoldStandard_data_frame <- BW_common_day
       save(      vismo_BW_common_day_GoldStandard_data_frame, file = 'vismo_BW_common_day_GoldStandard_data_frame.rda' )
       system('mv vismo_BW_common_day_GoldStandard_data_frame.rda ../testdata/') ## if running inside the 'maeve/inst/tests/testthat/' directory.
         }
          
          ## unit test:  Is the "BW_common_day" data.frame we get the same as what was seen before?
          testthat::expect_equal( BW_common_day, vismo_BW_common_day_GoldStandard_data_frame, tolerance = TOLERANCE )
          
          
       maeve_reset() # reset package options to their original values.          
          
          } # end of 'code = {...'
          
        ) # end of 'test_that(...'

