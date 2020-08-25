context('Fitting a spline and estimaing ITGR values on sine+linear test data.')

TOLERANCE <- 1e-3

test_that(
          desc = "ITGR estimates from sine+linear gamm4 spline matches expected ITGR estimates.",
###          
          code = {  

          require( maeve )

###       data( sineLinearGrowthSim, package = 'maeve' ) ### simulated data set in maeve/data/
          load( '../testdata/sineLinearGrowthSim.rda' ) # simulated data frame.
          
### Load the "gold standard" answers from the internal test data directory.
### We might need to change the path on these later -- I'm not sure where
### best practices indicate they should go, but this is what I found online.
          load( '../testdata/sineLinear_pred_data_frame.rda' ) # predicted data frame.
          load( '../testdata/sineLinear_Identity_ITGR.rda' )  # "compare_groups()" output for contrast = 'Tukey' 
          load( '../testdata/sineLinear_Dunnett_ITGR.rda'  )  # "compare_groups()" output for contrast = 'Tukey' 
          load( '../testdata/sineLinear_Tukey_ITGR.rda'  )    # "compare_groups()" output for contrast = 'Tukey' 
          
          set.seed( 20190812 )


### We fit the model for an ITGR endpoint with no transformation.
### Below we'll give results from this model to maeve::compare_groups().

          maeve_reset()
          maeve_options( autoset_modeling_data = TRUE,
                         metric = c('ITGR'),
                         trans_func_char   = 'Identity',
                           inv_func_char   = "Identity",
                         add_to_endpoint   = 0,
                         progress          = FALSE,
                         number_basis_vecs = -1,
                         truncate_fit      = TRUE
                        )

          model_list <- maeve::model_study( sineLinearGrowthSim )

          pred_data_frame = predict_study( model_list, x_pred_type = 'union_observed_and_grid', x_pred_spacing = 0.01 ) # observed time points + fine grid for plotting.
          

if(FALSE){
### This provides the code for the "gold standard".
### Do *NOT* run it by default, but this seems a sensible place to save it.
       stop('Stop in test_ITGR_calcuations.R: Comment this line out to run the code block.')
       sineLinear_pred_data_frame <- pred_data_frame
       save( sineLinear_pred_data_frame, file = 'sineLinear_pred_data_frame.rda')
       system('mv sineLinear_pred_data_frame.rda ../testdata/') ## if running inside the 'maeve/inst/tests/testthat/' directory.
         }
            
### unit test:  Is the "pred_data_frame" data.frame we get the same as what was seen before?
       testthat::expect_equal( pred_data_frame, sineLinear_pred_data_frame, tolerance = TOLERANCE )
          
### 'cg' is short for 'compare_groups'.  First, test "Identity" contrasts.            
### Each group separately:
       cg_identity <-
          maeve::compare_groups( model_list, 
                                 metric       = 'ITGR',
                                 contrast     = 'Identity',
                                 draw_figures =  FALSE
                                )

if(FALSE){
### This provides the code for the "gold standard".
### Do *NOT* run it by default, but this seems a sensible place to save it.
       stop('Stop in test_ITGR_calculations.R: Comment this line out to run the code block.')
       sineLinear_Identity_ITGR <- cg_identity$data$effectDF
       save( sineLinear_Identity_ITGR, file = 'sineLinear_Identity_ITGR.rda')
       system('mv sineLinear_Identity_ITGR.rda ../testdata/') ## if running inside the 'maeve/inst/tests/testthat/' directory.
     }

### unit test:  Is the ITGR-associated object of class 'glht'?
       testthat::expect_is( cg_identity$model[["glht_obj_list"]][['ITGR']], "glht" )            

### unit test: Is the "effectDF" data.frame stored within the compare_groups() list
###            the same as seen before?            
       testthat::expect_equal( cg_identity$data$effectDF, sineLinear_Identity_ITGR, tolerance = TOLERANCE )
 
          

### Second, test "Dunnett" contrasts: each non-reference group versus the common control:
           cg_Dunnett <-
              maeve::compare_groups( model_list,
                                     metric       = 'ITGR',
                                     contrast     = 'Dunnett',
                                     draw_figures =  FALSE
                                    )

if(FALSE){
### This provides the code for the "gold standard".
### Do not run it by default, but this seems a sensible place to save it.
         stop('Stop in test_ITGR_calculations.R: Comment this line out to run the code block.')
         sineLinear_Dunnett_ITGR <- cg_Dunnett$data$effectDF
         save( sineLinear_Dunnett_ITGR, file = 'sineLinear_Dunnett_ITGR.rda')
         system('mv sineLinear_Dunnett_ITGR.rda ../testdata/') ## if running inside the 'maeve/inst/tests/testthat/' directory.
        }

### unit test:  Is the ITGR-associated object of class 'glht'?
         testthat::expect_is( cg_Dunnett$model[["glht_obj_list"]][['ITGR']], "glht" )            

### unit test: Is the "effectDF" data.frame stored within the compare_groups() list
###            the same as seen before?            
         testthat::expect_equal( cg_Dunnett$data$effectDF, sineLinear_Dunnett_ITGR, tolerance = TOLERANCE )




### Third, test "Tukey" contrasts: all pair-wise differences:
           cg_Tukey <-
              maeve::compare_groups( model_list,
                                     metric       = 'ITGR',
                                     contrast     = 'Tukey',
                                     draw_figures =  FALSE
                                    )

if(FALSE){
### This provides the code for the "gold standard".
### Do not run it by default, but this seems a sensible place to save it.
         stop('Stop in test_ITGR_calculations.R: Comment this line out to run the code block.')
         sineLinear_Tukey_ITGR <- cg_Tukey$data$effectDF
         save( sineLinear_Tukey_ITGR, file = 'sineLinear_Tukey_ITGR.rda')
         system('mv sineLinear_Tukey_ITGR.rda ../testdata/') ## if running inside the 'maeve/inst/tests/testthat/' directory.
        }

### unit test:  Is the ITGR-associated object of class 'glht'?
     testthat::expect_is( cg_Tukey$model[["glht_obj_list"]][['ITGR']], "glht" )            

### unit test: Is the "effectDF" data.frame stored within the compare_groups() list
###            the same as seen before?            
     testthat::expect_equal( cg_Tukey$data$effectDF, sineLinear_Tukey_ITGR, tolerance = TOLERANCE )

          
     maeve_reset() # reset package options to their original values.          
          
          
          } # end of 'code = {...'
          
        ) # end of 'test_that(...'


