context('auc_by_id() function to compute subject_ID-level area-under-curve summaries.')

TOLERANCE <- 1e-3

test_that(
          desc = "AUC-by-subject_ID estimates for vismodegib data over 21 days matches expectations.",
###          
          code = {  

          require( magrittr )
          require(    dplyr )
          require(    maeve ) # find.package( 'maeve' )


### Load the "gold standard" answers from the internal test data directory.
### We might need to change the path on these later -- I'm not sure where
### best practices indicate they should go, but this is what I found online.
          load( '../testdata/vismo21_auc_by_id_TV_GoldStandard_data_frame.rda' )
          load( '../testdata/vismo21_BW_avg_GoldStandard_data_frame.rda' )
          
          set.seed( 20200517 )

          maeve_reset()
          
          data( vismodegib, package = 'maeve' )

          ## prep vismodegib data for days [0, 21].
          vismo21 <- vismodegib %>% dplyr::filter( DAY_OF_STUDY <= 21 ) %>% droplevels()


          
          ## test 01:
          ## summarize each animal's log( 1 + TUMOR_VOLUME ) into a subject-specific eGaIT:
          vismo21_AUC_TV <- vismo21 %>% auc_by_id() %>% dplyr::rename( AUC_TV = AUC )
          
if(FALSE){
### This provides the code for the "gold standard".
### Do *NOT* run it by default, but this seems a sensible place to save it.
       stop('Stop in test_auc_by_id_calcuations.R: Comment this line out to run the code block.')
       vismo21_auc_by_id_TV_GoldStandard_data_frame <- vismo21_AUC_TV
       save(      vismo21_auc_by_id_TV_GoldStandard_data_frame, file = 'vismo21_auc_by_id_TV_GoldStandard_data_frame.rda')
       system('mv vismo21_auc_by_id_TV_GoldStandard_data_frame.rda ../testdata/') ## if running inside the 'maeve/inst/tests/testthat/' directory.
         }
            
          ## unit test:  Is the "vismo21_AUC_TV" data.frame we get the same as what was seen before?
          testthat::expect_equal( vismo21_AUC_TV, vismo21_auc_by_id_TV_GoldStandard_data_frame, tolerance = TOLERANCE )



          
          ## test 02:
          ## auc_by_id(): computing by-ID averages over time:
          ##
          ## By changing parameters, the AUC will instead
          ## become the time-interval-weighted average over
          ## the observed range:
          vismo21_BW_avg <- 
          vismo21 %>%
           maeve::auc_by_id(
                      endpoint_name = 'BODY_WEIGHT',
                      VST = FALSE,
                      subtract_starting_value = FALSE,
                      xrange_norm_method = 'xrange'
                  ) %>%
                 dplyr::rename( AUC_BW_avg = AUC )
       
   
if(FALSE){
### This provides the code for the "gold standard".
### Do *NOT* run it by default, but this seems a sensible place to save it.
       stop('Stop in test_auc_by_id_calcuations.R: Comment this line out to run the code block.')
       vismo21_BW_avg_GoldStandard_data_frame <- vismo21_BW_avg
       save(      vismo21_BW_avg_GoldStandard_data_frame, file = 'vismo21_BW_avg_GoldStandard_data_frame.rda')
       system('mv vismo21_BW_avg_GoldStandard_data_frame.rda ../testdata/') ## if running inside the 'maeve/inst/tests/testthat/' directory.
         }

          ## unit test:  Is the "vismo21_AUC_TV" data.frame we get the same as what was seen before?
          testthat::expect_equal( vismo21_BW_avg, vismo21_BW_avg_GoldStandard_data_frame, tolerance = TOLERANCE )

          
       maeve_reset() # reset package options to their original values.          
          
          } # end of 'code = {...'
          
        ) # end of 'test_that(...'

