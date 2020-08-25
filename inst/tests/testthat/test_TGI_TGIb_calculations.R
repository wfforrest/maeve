context( 'Fitting a spline and estimaing TGI and TGIb values logQuadraticGrowthSim_02 data set.' )

TOLERANCE <- 1e-2 # Monte Carlo integration for confidence intervals -- allow a little more leeway in relative error.

test_that(
          
          desc = "TGI & TGIb estimates from log-quadratic simulated data set #02 match expected TGI & TGIb estimates.",
###          
          code = {  

          require( maeve )
          require( magrittr ) # used in de-selecting 'tstat' in an expect_equal() query below
          
###       data( logQuadraticGrowthSim_02, package = 'maeve' ) ### simulated data set in maeve/data/
          load( '../testdata/logQuadraticGrowthSim_02.rda' )

###       testthat::skip('Not running "test_TGI_TGIb_calculations.R" right now.')
          
### Load the "gold standard" answers from the internal test data directory.
### We might need to change the path on these later -- I'm not sure where
### best practices indicate they should go, but this is what I found online.
          load( '../testdata/logQuadratic02_summary_table.rda' )
          load( '../testdata/logQuadratic02_Identity_AUC.rda' )
          load( '../testdata/logQuadratic02_Dunnett_AUC.rda' )          
          
          set.seed( 20190812 )
          
### We fit the model for an AUC endpoint with no transformation.
### Below we'll give results from this model to maeve::compare_groups().

        maeve_reset() # just in case they are not at original values.
          
        maeve_options( autoset_modeling_data = TRUE, metric = c('AUC') )

        model_list <- suppressWarnings( maeve::model_study( logQuadraticGrowthSim_02 ) ) # I think warnings are due to negligible variation in the (simulated) data.
        pred_data_frame = maeve::predict_study( model_list )

          if(FALSE){ # inspect actual data if desired:
             pdf('logQuadraticGrowthSim_02_data_with_pred.pdf', width = 10); 
             maeve::draw_study( pred_data_frame, endpoint_name = 'y' )            
             maeve::draw_study( pred_data_frame )
             quiet <- dev.off()
          }
          
### Get summary table with Partial Response (PR) and End-of-Study Complete Response (EOS_CR) counts:          
    my_inverse_function <- function(y){get(maeve_options('inv_func_char'))(y)-maeve_options('add_to_endpoint')} # define this and pass to generate_summary_table()

    logQuadratic02_summary_table_current <-
      maeve::generate_summary_table( model_list = model_list,
                                     clean_DF = pred_data_frame,
                                     ###full_inverse_function = my_inverse_function,
                                     metric = 'AUC', # generate_summary_table() takes only one metric -- testing AUC
                                     xmin = 1, xmax = 3,
                                     x_time_spacing = 0.01
                                    )

if(FALSE){
### This provides the code for the "gold standard".
logQuadratic02_summary_table_current ### Do *NOT* run it by default, but this seems a sensible place to save it.
       stop('Stop in test_TGI_TGIb_calcuations.R: Comment this line out to run the code block.')
       logQuadratic02_summary_table <- logQuadratic02_summary_table_current
       save( logQuadratic02_summary_table, file = 'logQuadratic02_summary_table.rda' )
       system('mv logQuadratic02_summary_table.rda ../testdata/') ## if running inside the 'maeve/inst/tests/testthat/' directory.
     }
            
### unit test:  Is the summary table we get the same as what was seen before?
          
### 20190814: This test fails because the SE is so small on some of the
### calculations that the resulting t-statistics get really huge and do not
### match to, e.g., 1e-4.  A simple fix would be to set the 'scale = ...'
### parameter passed from "testthat::expect_equal()" to "base::all.equal()"
### separately by column in the data frames passed, but I couldn't figure
### out how to make that work.  After looking at a bunch of these, I am
### just removing 'tstat' from the columns compared and making sure that it
### matches within 'TOLERANCE' for all the others (including the 'Diff_Ref'
### and 'sigma' values whose ratio comprises 'tstat').
###
          
if(FALSE){ # discontinuing this test on 20190914; switching to the one in 'else{...}'
  testthat::expect_equal( logQuadratic02_summary_table_current, logQuadratic02_summary_table, tolerance = TOLERANCE )
} else{
  testthat::expect_equal( object   = logQuadratic02_summary_table_current %>% dplyr::select(-tstat),
                          expected = logQuadratic02_summary_table         %>% dplyr::select(-tstat),
                          tolerance = TOLERANCE
                         )
}          

### Check AUC values with compare_groups():

### Identity:
###          
### 'cg' is short for 'compare_groups'.  First, test "Identity" contrasts.            
### Each group separately:
    cg_Identity <- maeve::compare_groups( model_list, draw_figures = FALSE ) # NB: By default, maeve_options('contrast') == 'Identity'

if(FALSE){
### This provides the code for the "gold standard".
### Do *NOT* run it by default, but this seems a sensible place to save it.
       stop('Stop in test_TGI_TGIb_calcuations.R: Comment this line out to run the code block.')
       logQuadratic02_Identity_AUC <- cg_Identity$data$effectDF
       save( logQuadratic02_Identity_AUC, file = 'logQuadratic02_Identity_AUC.rda')
       system('mv logQuadratic02_Identity_AUC.rda ../testdata/') ## if running inside the 'maeve/inst/tests/testthat/' directory.
         }

### unit test:  Is the AUC-associated object of class 'glht'?
    testthat::expect_is( cg_Identity$model[["glht_obj_list"]][['AUC']], "glht" )            

### unit test: Is the "effectDF" data.frame stored within the compare_groups() list
###            the same as seen before?            
    testthat::expect_equal( cg_Identity$data$effectDF, logQuadratic02_Identity_AUC, tolerance = TOLERANCE )



### Dunnett:          
###          
### 'cg' is short for 'compare_groups'.  First, test "Dunnett" contrasts.            
### Each group separately:
       cg_Dunnett <- maeve::compare_groups( model_list, contrast = 'Dunnett', draw_figures = FALSE ) # NB: By default, maeve_options('contrast') == 'Dunnett'

if(FALSE){
### This provides the code for the "gold standard".
### Do *NOT* run it by default, but this seems a sensible place to save it.
       stop('Stop in test_TGI_TGIb_calcuations.R: Comment this line out to run the code block.')
       logQuadratic02_Dunnett_AUC <- cg_Dunnett$data$effectDF
       save( logQuadratic02_Dunnett_AUC, file = 'logQuadratic02_Dunnett_AUC.rda')
       system('mv logQuadratic02_Dunnett_AUC.rda ../testdata/') ## if running inside the 'maeve/inst/tests/testthat/' directory.
         }

### unit test:  Is the AUC-associated object of class 'glht'?
    testthat::expect_is( cg_Dunnett$model[["glht_obj_list"]][['AUC']], "glht" )            

### unit test: Is the "effectDF" data.frame stored within the compare_groups() list
###            the same as seen before?            
    testthat::expect_equal( cg_Dunnett$data$effectDF, logQuadratic02_Dunnett_AUC, tolerance = TOLERANCE )

       maeve_reset() # reset package options to their original values.
          
          } # end of 'code = {...'
          
        ) # end of 'test_that(...'




### mothball
