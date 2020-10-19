context('summarize_by_id() function to compute subject_ID-level area-under-curve summaries.')

TOLERANCE <- 1e-3

test_that(
          desc = "Summary-by-subject_ID estimates for vismodegib data over 21 days matches expectations.",
###          
          code = {  

          require( magrittr )
          require(    dplyr )
          require(    maeve ) # find.package( 'maeve' )


### Load the "gold standard" answers from the internal test data directory.
### We might need to change the path on these later -- I'm not sure where
### best practices indicate they should go, but this is what I found online.
          load( '../testdata/vismo21_summarize_by_id_01_GoldStandard_data_frame.rda' )
          load( '../testdata/vismo21_summarize_by_id_02_GoldStandard_data_frame.rda' )
          load( '../testdata/vismo21_summarize_by_id_03_GoldStandard_data_frame.rda' )
          
          set.seed( 20201018 )

          maeve_reset()
          
          data( vismodegib, package = 'maeve' )

          ## prep vismodegib data for days [0, 21].
          vismo21 <- vismodegib %>% dplyr::filter( DAY_OF_STUDY <= 21 ) %>% droplevels()


          
          ## test 01:
          ## summarize each animal's log( 1 + TUMOR_VOLUME ) into a subject-specific slope, eDOT, or eGaIT:
          vismo21_summarize_by_id_01 <- vismo21 %>% summarize_by_id( metric = c('linear', 'ITGR', 'AUC' ) )
          
if(FALSE){
### This provides the code for the "gold standard".
### Do *NOT* run it by default, but this seems a sensible place to save it.
    stop('Stop in test_summarize_by_id_calcuations.R: Comment this line out to run the code block.')
    vismo21_summarize_by_id_01_GoldStandard_data_frame <- vismo21_summarize_by_id_01
    save(      vismo21_summarize_by_id_01_GoldStandard_data_frame, file = 'vismo21_summarize_by_id_01_GoldStandard_data_frame.rda')
    system('mv vismo21_summarize_by_id_01_GoldStandard_data_frame.rda ../testdata/') ## if running inside the 'maeve/inst/tests/testthat/' directory.
    }
            
          ## unit test:  Is the "vismo21_AUC_TV" data.frame we get the same as what was seen before?
          testthat::expect_equal( vismo21_summarize_by_id_01, vismo21_summarize_by_id_01_GoldStandard_data_frame, tolerance = TOLERANCE )



          
          ## test 02:
          ## summarize_by_id(): computing by-ID averages over time:
          ##
          ## By changing parameters, the AUC will instead become the time-interval-weighted
          ## average over the observed range for body weight.  We also include the slope
          ## of body weight over time.
          vismo21_summarize_by_id_02 <- 
          vismo21 %>%
           maeve::summarize_by_id(
                      endpoint_name = 'BODY_WEIGHT',
                      metric = c('linear', 'AUC'),
                      VST = FALSE,
                      subtract_starting_value = FALSE,
                      xrange_norm_method = 'xrange'
                  ) %>%
           dplyr::rename( AUC_BW_avg = AUC )
       
   
if(FALSE){
### This provides the code for the "gold standard".
### Do *NOT* run it by default, but this seems a sensible place to save it.
    stop('Stop in test_auc_by_id_calcuations.R: Comment this line out to run the code block.')
    vismo21_summarize_by_id_02_GoldStandard_data_frame <- vismo21_summarize_by_id_02
    save(      vismo21_summarize_by_id_02_GoldStandard_data_frame, file = 'vismo21_summarize_by_id_02_GoldStandard_data_frame.rda')
    system('mv vismo21_summarize_by_id_02_GoldStandard_data_frame.rda ../testdata/') ## if running inside the 'maeve/inst/tests/testthat/' directory.
    }

          ## unit test:  Is the "vismo21_AUC_TV" data.frame we get the same as what was seen before?
          testthat::expect_equal( vismo21_summarize_by_id_02, vismo21_summarize_by_id_02_GoldStandard_data_frame, tolerance = TOLERANCE )



          ## test 03:
          ## summarize_by_id(): Some haphazard calculations to capture a few more options:
          ##
          vismo21_summarize_by_id_03 <- 
          vismodegib %>%
           maeve::summarize_by_id(
                      endpoint_name = 'TUMOR_VOLUME',
                      metric = c('linear', 'ITGR', 'AUC'),
                      ## interpolate between mid-day values:
                      xmin =  2.5,
                      xmax = 21.5,
                      subtract_starting_value = TRUE,
                      xrange_norm_method = 'slope_equivalent'
                  )
   
if(FALSE){
### This provides the code for the "gold standard".
### Do *NOT* run it by default, but this seems a sensible place to save it.
    stop('Stop in test_auc_by_id_calcuations.R: Comment this line out to run the code block.')
    vismo21_summarize_by_id_03_GoldStandard_data_frame <- vismo21_summarize_by_id_03
    save(      vismo21_summarize_by_id_03_GoldStandard_data_frame, file = 'vismo21_summarize_by_id_03_GoldStandard_data_frame.rda')
    system('mv vismo21_summarize_by_id_03_GoldStandard_data_frame.rda ../testdata/') ## if running inside the 'maeve/inst/tests/testthat/' directory.
    }

          ## unit test:  Is the "vismo21_AUC_TV" data.frame we get the same as what was seen before?
          testthat::expect_equal( vismo21_summarize_by_id_03, vismo21_summarize_by_id_03_GoldStandard_data_frame, tolerance = TOLERANCE )

          
          maeve_reset() # reset package options to their original values.          
          
          } # end of 'code = {...'
          
        ) # end of 'test_that(...'

