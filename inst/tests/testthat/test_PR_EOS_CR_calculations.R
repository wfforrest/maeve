context('Counting partial responses (PR) and end-of-study complete responses (EOS_CR) in simulated data.')

TOLERANCE <- 1e-3

test_that(
          desc = "Partial Response (PR) and End-of-Study Complete Response (EOS_CR) counts from bifurcated growth simulated data set match expected counts.",
###          
          code = {  

          require( maeve )

          load( '../testdata/bifurcatedGrowthSim.rda' )

### Load the "gold standard" answers from the internal test data directory.
### We might need to change the path on these later -- I'm not sure where
### best practices indicate they should go, but this is what I found online.
          load( '../testdata/bifurcatedGrowth_pred_data_frame.rda' )
          load( '../testdata/bifurcatedGrowth_PR_EOS_CR_table.rda' )          
          
          set.seed( 20190812 )
            
### We fit the model for an AUC endpoint with no transformation.
### Below we'll give results from this model to maeve::compare_groups().

        maeve_reset() # just in case they are not at original values.
          
        maeve_options( autoset_modeling_data = TRUE, metric = c( 'linear', 'AUC' ), progress = FALSE, truncate_fit = TRUE )
          
        ## Fitting the linear model to strongly bifurcated data generates warnings
        ## that we will ignore here, since they are expected.
        model_list <- suppressWarnings( maeve::model_study( bifurcatedGrowthSim ) ) 
          
        pred_data_frame = maeve::predict_study( model_list )

        if( FALSE ){
            ## Here is a picture of the fits with the transformed data (in 'y'):
            pdf( 'bifurcatedGrowthSim_data_with_pred.pdf', width = 10 );
            pred_data_frame %>% maeve::draw_study( endpoint_name = 'y', fit = c('linear', 'spline') );
            quiet <- dev.off()
        }

if(FALSE){
### This provides the code for the "gold standard".
### Do *NOT* run it by default, but this seems a sensible place to save it.
       stop('Stop in test_PR_EOS_CR_calcuations.R: Comment this line out to run the code block.')
       bifurcatedGrowth_pred_data_frame <- pred_data_frame
       save( bifurcatedGrowth_pred_data_frame, file = 'bifurcatedGrowth_pred_data_frame.rda' )
       system('mv bifurcatedGrowth_pred_data_frame.rda ../testdata/') ## if running inside the 'maeve/inst/tests/testthat/' directory.
         }
          
### unit test:  Is the predicted data.frame we get the same as what was seen before?
       testthat::expect_equal( pred_data_frame, bifurcatedGrowth_pred_data_frame, tolerance = TOLERANCE )


### Get summary table with Partial Response (PR) and End-of-Study Complete Response (EOS_CR) counts:          
    my_inverse_function <- function(y){get(maeve_options('inv_func_char'))(y)-maeve_options('add_to_endpoint')} # define this and pass to generate_summary_table()

    bifurcatedGrowth_PR_EOS_CR_table_current <-
      maeve::generate_summary_table( model_list = model_list,
                                     clean_DF = pred_data_frame,
                                     full_inverse_function = my_inverse_function,
                                     metric = 'AUC',
                                     xmin = 0, xmax = 1,
                                     x_time_spacing = 0.025 ) %>%
      dplyr::select( group, N_first_day, first_day, last_day, PR, EOS_CR )                 

          
if(FALSE){
### This provides the code for the "gold standard".
### Do *NOT* run it by default, but this seems a sensible place to save it.
       stop('Stop in test_PR_EOS_CR_calcuations.R: Comment this line out to run the code block.')
       bifurcatedGrowth_PR_EOS_CR_table <- bifurcatedGrowth_PR_EOS_CR_table_current
       save( bifurcatedGrowth_PR_EOS_CR_table, file = 'bifurcatedGrowth_PR_EOS_CR_table.rda' )       
       system('mv bifurcatedGrowth_PR_EOS_CR_table.rda ../testdata/') ## if running inside the 'maeve/inst/tests/testthat/' directory.
         }

### unit test:  Is the PR + EOS_CR data.frame we get the same as what was seen before?
       testthat::expect_equal( bifurcatedGrowth_PR_EOS_CR_table_current,
                               bifurcatedGrowth_PR_EOS_CR_table,
                               tolerance = TOLERANCE
                              )

       maeve_reset() # reset package options to their original values.
          
          } # end of 'code = {...'
          
        ) # end of 'test_that(...'
