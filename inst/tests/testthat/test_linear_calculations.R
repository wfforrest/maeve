context('Fitting a linear mixed model and estimaing slope values on sine+linear test data.')

TOLERANCE <- 1e-3

test_that(
          desc = "linear estimates from log-scaled linear mixed effect model matches expected linear estimates.",
###          
          code = {  

          require( maeve )

###       data( logLinearGrowthSim, package = 'maeve' ) ### simulated data set in maeve/data/
          load( '../testdata/logLinearGrowthSim.rda' ) # simulated data

### Load the "gold standard" answers from the internal test data directory.
### We might need to change the path on these later -- I'm not sure where
### best practices indicate they should go, but this is what I found online.
          load( '../testdata/logLinear_Identity_pred_data_frame.rda' )
          load( '../testdata/logLinear_Identity_linear.rda' ) # "compare_groups()" output for contrast = 'Identity' 
          load( '../testdata/logLinear_Dunnett_linear.rda'  ) # "compare_groups()" output for contrast = 'Dunnett' 
          load( '../testdata/logLinear_Tukey_linear.rda'  )   # "compare_groups()" output for contrast = 'Tukey' 

          set.seed( 20190812 )
          
### We fit the model for an AUC endpoint with no transformation.
### Below we'll give results from this model to maeve::compare_groups().

          maeve_reset()
          maeve_options( autoset_modeling_data = TRUE,
                         metric = c('linear'),
                         trans_func_char = 'log',
                           inv_func_char = 'exp',
                         add_to_endpoint = 1,
                         progress = FALSE,
                         truncate_fit = TRUE
                        )

          model_list <- maeve::model_study( logLinearGrowthSim )

          pred_data_frame = predict_study( model_list, x_pred_type = 'union_observed_and_grid', x_pred_spacing = 0.1 ) # observed time points + fine grid for plotting.

if(FALSE){
### This provides the code for the "gold standard".
### Do *NOT* run it by default, but this seems a sensible place to save it.
       stop('Stop in test_AUC_calcuations.R: Comment this line out to run the code block.')
       logLinear_Identity_pred_data_frame <- pred_data_frame
       save( logLinear_Identity_pred_data_frame, file = 'logLinear_Identity_pred_data_frame.rda')
       system('mv logLinear_Identity_pred_data_frame.rda ../testdata/') ## if running inside the 'maeve/inst/tests/testthat/' directory.
        }
            
### unit test:  Is the "pred_data_frame" data.frame we get the same as what was seen before?
       testthat::expect_equal( pred_data_frame, logLinear_Identity_pred_data_frame, tolerance = TOLERANCE )


          
### 'cg' is short for 'compare_groups'.  First, test "Identity" contrasts.            
### Each group separately:
       cg_Identity <-
          maeve::compare_groups( model_list, 
                                 metric       = 'linear',
                                 contrast     = 'Identity',
                                 draw_figures =  FALSE
                                )

if(FALSE){
### This provides the code for the "gold standard".
### Do *NOT* run it by default, but this seems a sensible place to save it.
       stop('Stop in test_linear_calculations.R: Comment this line out to run the code block.')
       logLinear_Identity_linear <- cg_Identity$data$effectDF
       save( logLinear_Identity_linear, file = 'logLinear_Identity_linear.rda')
       system('mv logLinear_Identity_linear.rda ../testdata/') ## if running inside the 'maeve/inst/tests/testthat/' directory.
     }

### unit test:  Is the linear-associated object of class 'glht'?
       testthat::expect_is( cg_Identity$model[["glht_obj_list"]][['linear']], "glht" )            

### unit test: Is the "effectDF" data.frame stored within the compare_groups() list
###            the same as seen before?            
       testthat::expect_equal( cg_Identity$data$effectDF, logLinear_Identity_linear, tolerance = TOLERANCE )

### Second, test "Dunnett" contrasts: each non-reference group versus the common control:
           cg_dunnett <-
              maeve::compare_groups( model_list,
                                     metric       = 'linear',
                                     contrast     = 'Dunnett',
                                     draw_figures =  FALSE
                                    )

if(FALSE){
### This provides the code for the "gold standard".
### Do not run it by default, but this seems a sensible place to save it.
         stop('Stop in test_linear_calculations.R: Comment this line out to run the code block.')
         logLinear_Dunnett_linear <- cg_dunnett$data$effectDF
         save( logLinear_Dunnett_linear, file = 'logLinear_Dunnett_linear.rda')
         system('mv logLinear_Dunnett_linear.rda ../testdata/') ## if running inside the 'maeve/inst/tests/testthat/' directory.
        }

### unit test:  Is the linear-associated object of class 'glht'?
         testthat::expect_is( cg_dunnett$model[["glht_obj_list"]][['linear']], "glht" )

### unit test: Is the "effectDF" data.frame stored within the compare_groups() list
###            the same as seen before?            
         testthat::expect_equal( cg_dunnett$data$effectDF, logLinear_Dunnett_linear, tolerance = TOLERANCE )




### Third, test "Tukey" contrasts: all pair-wise differences:
           cg_Tukey <-
              maeve::compare_groups( model_list,
                                     metric       = 'linear',
                                     contrast     = 'Tukey',
                                     draw_figures =  FALSE
                                    )

if(FALSE){
### This provides the code for the "gold standard".
### Do not run it by default, but this seems a sensible place to save it.
         stop('Stop in test_linear_calculations.R: Comment this line out to run the code block.')
         logLinear_Tukey_linear <- cg_Tukey$data$effectDF
         save( logLinear_Tukey_linear, file = 'logLinear_Tukey_linear.rda')
         system('mv logLinear_Tukey_linear.rda ../testdata/') ## if running inside the 'maeve/inst/tests/testthat/' directory.
        }

### unit test:  Is the linear-associated object of class 'glht'?
       testthat::expect_is( cg_Tukey$model[["glht_obj_list"]][['linear']], "glht" )

### unit test: Is the "effectDF" data.frame stored within the compare_groups() list
###            the same as seen before?            
       testthat::expect_equal( cg_Tukey$data$effectDF, logLinear_Tukey_linear, tolerance = TOLERANCE )

       maeve_reset() # reset package options to their original values.
          
### 20180411
### NB: The confidence intervals determined via confint() for the multcomp::glht() object
### that summarizes the lme4 analysis appear to use the "mvtnorm::pmvt()" function, which
### (see its documentation "Details") uses a quasi- Monte Carlo integration, noting that
### "Because of this algorithm, the results (slightly) depend on .Random.seed."  In practice,
### the point estimates are replicable to machine epsilon, but the confidence interval endpoints
### all shift in unison  up or down at around the 4th decimal place.  I don't think this is
### enough to cause a practical problem, but it was crashing the unit test (that's how I discovered
### this), so I've just  changed the tolerance of the "testthat::expect_equivalent(...)" call to
### be more lenient.

## pdf('tmp.pdf', width = 10);
## maeve_options( endpoint_name = 'y' )       
## logLinearGrowthSim %>% dplyr::mutate( y = log( TUMOR_VOLUME + 1) ) %>% draw_study();
## pred_data_frame %>% draw_study( fit = 'none' );              
## pred_data_frame %>% draw_study( fit = 'linear' );       
## pred_data_frame %>% draw_study( fit = 'spline' );       
## quiet <- dev.off()

          
          } # end of 'code = {...'
          
        ) # end of 'test_that(...'


