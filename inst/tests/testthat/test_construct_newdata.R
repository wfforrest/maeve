context('construct_newdata function to set up new data grid for predicted values.')

TOLERANCE <- 1e-3

test_that(
          desc = "data frame with group_names, subject_IDs, and times.",

          code = {  

          require( magrittr )
          require(    dplyr )
          require(    maeve ) # find.package( 'maeve' )


### Load the "gold standard" answers from the internal test data directory.
### We might need to change the path on these later -- I'm not sure where
### best practices indicate they should go, but this is what I found online.
###          load( '../testdata/vismo_newdata_01_GoldStandard_data_frame.rda' )
###          load( '../testdata/vismo_newdata_02_GoldStandard_data_frame.rda' )

          
          set.seed( 2020623 )

          maeve_reset()
          
          data( vismodegib, package = 'maeve' )

          ## prep vismodegib data for days [0, 21].
          vismo21_renamed <-
              vismodegib %>%
              dplyr::filter( DAY_OF_STUDY <= 21 ) %>%
              dplyr::rename( group = group_name, ID = animalID, x = DAY_OF_STUDY ) %>% # internal names
              droplevels()
          ## pass this to the construct_newdata() function.


              
          ## test 01:
          ##
          maeve_reset()              
          vismo_newdata_grid_test_01 <-   
            vismo21_renamed %>% # make an even grid with one-day resolution:
              maeve:::construct_newdata( x_pred_type = 'grid', x_pred_spacing = 1 )

          if(FALSE){
          ## This provides the code for the "gold standard".
          ## Do *NOT* run it by default, but this seems a sensible place to save it.
          stop('Stop in test_construct_newdata.R: Comment this line out to run the code block.')
          vismo_newdata_grid_test_01_GoldStandard_data_frame <- vismo_newdata_grid_test_01
          save( vismo_newdata_grid_test_01_GoldStandard_data_frame, file = 'vismo_newdata_grid_test_01_GoldStandard_data_frame.rda' )
          system('mv vismo_newdata_grid_test_01_GoldStandard_data_frame.rda ../testdata/') ## if running inside the 'maeve/inst/tests/testthat/' directory.
          }

          load( '../testdata/vismo_newdata_grid_test_01_GoldStandard_data_frame.rda' )
          
          ## unit test:  Is the "vismo_newdata_grid_test_01" data.frame we get the same as what was seen before?
          testthat::expect_equal( vismo_newdata_grid_test_01, vismo_newdata_grid_test_01_GoldStandard_data_frame, tolerance = TOLERANCE )




          
          ## test 02:
          maeve_reset() # reset package options to their original values.          
          vismo_newdata_grid_test_02 <-   
            vismo21_renamed %>% # make an even grid with 4/3-day resolution, plus observed:
              maeve:::construct_newdata( x_pred_type = 'union_observed_and_grid', x_pred_spacing = 4/3 )

          if(FALSE){
          ## This provides the code for the "gold standard".
          ## Do *NOT* run it by default, but this seems a sensible place to save it.
          stop('Stop in test_construct_newdata.R: Comment this line out to run the code block.')
          vismo_newdata_grid_test_02_GoldStandard_data_frame <- vismo_newdata_grid_test_02
          save( vismo_newdata_grid_test_02_GoldStandard_data_frame, file = 'vismo_newdata_grid_test_02_GoldStandard_data_frame.rda' )
          system('mv vismo_newdata_grid_test_02_GoldStandard_data_frame.rda ../testdata/') ## if running inside the 'maeve/inst/tests/testthat/' directory.
          }

          load( '../testdata/vismo_newdata_grid_test_02_GoldStandard_data_frame.rda' )
          
          ## unit test:  Is the "vismo_newdata_grid_test_02" data.frame we get the same as what was seen before?
          testthat::expect_equal( vismo_newdata_grid_test_02, vismo_newdata_grid_test_02_GoldStandard_data_frame, tolerance = TOLERANCE )




          ## test 03:
          maeve_reset() # reset package options to their original values.          
          vismo_newdata_grid_test_03 <-   
            vismo21_renamed %>% # make an even grid with observations every pi = 3.14159... days, with "0" & "21" as endpoints:
              maeve:::construct_newdata( x_pred_type = 'custom', x_pred_vec = c( seq( 0, 21, by = pi ), 21 ) )

          if(FALSE){
          ## This provides the code for the "gold standard".
          ## Do *NOT* run it by default, but this seems a sensible place to save it.
          stop('Stop in test_construct_newdata.R: Comment this line out to run the code block.')
          vismo_newdata_grid_test_03_GoldStandard_data_frame <- vismo_newdata_grid_test_03
          save( vismo_newdata_grid_test_03_GoldStandard_data_frame, file = 'vismo_newdata_grid_test_03_GoldStandard_data_frame.rda' )
          system('mv vismo_newdata_grid_test_03_GoldStandard_data_frame.rda ../testdata/') ## if running inside the 'maeve/inst/tests/testthat/' directory.
          }

          load( '../testdata/vismo_newdata_grid_test_03_GoldStandard_data_frame.rda' )
          
          ## unit test:  Is the "vismo_newdata_grid_test_03" data.frame we get the same as what was seen before?
          testthat::expect_equal( vismo_newdata_grid_test_03, vismo_newdata_grid_test_03_GoldStandard_data_frame, tolerance = TOLERANCE )




          ## test 04:
          maeve_reset() # reset package options to their original values.          
          vismo_newdata_grid_test_04 <-   
            vismo21_renamed %>% # make an grid with oberved values merged with values spliced in at "1 / pi" day multiples.
              maeve:::construct_newdata( x_pred_type = 'union_observed_and_custom', x_pred_vec = seq( 0, 21, by = 1 / pi ) )

          if(FALSE){
          ## This provides the code for the "gold standard".
          ## Do *NOT* run it by default, but this seems a sensible place to save it.
          stop('Stop in test_construct_newdata.R: Comment this line out to run the code block.')
          vismo_newdata_grid_test_04_GoldStandard_data_frame <- vismo_newdata_grid_test_04
          save( vismo_newdata_grid_test_04_GoldStandard_data_frame, file = 'vismo_newdata_grid_test_04_GoldStandard_data_frame.rda' )
          system('mv vismo_newdata_grid_test_04_GoldStandard_data_frame.rda ../testdata/') ## if running inside the 'maeve/inst/tests/testthat/' directory.
          }

          load( '../testdata/vismo_newdata_grid_test_04_GoldStandard_data_frame.rda' )
          
          ## unit test:  Is the "vismo21_AUC_TV" data.frame we get the same as what was seen before?
          testthat::expect_equal( vismo_newdata_grid_test_04, vismo_newdata_grid_test_04_GoldStandard_data_frame, tolerance = TOLERANCE )


          ##
          maeve_reset()
          
          
          } # end of 'code = {...'
          
        ) # end of 'test_that(...'

