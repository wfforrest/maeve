context('make_confint() function to make confidence intervals from multcomp::glht() objects created in maeve::compare_groups().')

TOLERANCE <- 1e-3

test_that(
          desc = "Confidence intervals, t-tests, and p-values returned from maeve:::make_confint() are as expected.",
    
          ##          
          code = {  

          require( maeve )

          load( '../testdata/make_confint_historical_output_01.rda' ) # simulated data frame.

          set.seed( 20200513 )
            
          ## We fit the model for a run through make_confint(), using vismodegib data.

          maeve_reset()
          maeve_options( autoset_modeling_data = TRUE,
                         metric                = c('AUC'),
                         trans_func_char       = 'Identity',
                           inv_func_char       = "Identity",
                         add_to_endpoint       = 0,
                         progress              = FALSE,
                         number_basis_vecs     = -1,
                         truncate_fit          = TRUE,
                         ##
                         x_name                = 'x',
                         endpoint_name         = 'y'
                        )

          data( vismodegib )
          vismo21 <-
              vismodegib %>%
              dplyr::filter( DAY_OF_STUDY <= 21 ) %>%
              dplyr::mutate( x = DAY_OF_STUDY, y = log( 1 + TUMOR_VOLUME ) ) %>%
              dplyr::select( group_name, animalID, x, y )

          ## fit GAMM model
          model_list <- maeve::model_study( vismo21 )

          ## compare groups (with Identity contrast, so not really "comparing"...):
          compare_groups_output <- compare_groups( model_list )

          ## extract the glht object from 'compare_groups_output':
          glht_object_list = list( 'AUC' = compare_groups_output[['models']][['glht_obj_list']][['AUC']] )

          ## return a data.frame of extended output from the make_confint() function:
          make_confint_current_output <- maeve:::make_confint( 'AUC', glht_object_list, extended_output = TRUE )
          
          if(FALSE){
          ## This provides the code for the "gold standard".
          ## Do *NOT* run it by default, but this seems a sensible place to save it.
          stop('Stop in test_makeconfint.R: Comment this line out to run the code block.')
          make_confint_historical_output_01 <- make_confint_current_output
          save( make_confint_historical_output_01, file = 'make_confint_historical_output_01.rda')
          system('mv make_confint_historical_output_01.rda ../testdata/') ## if running inside the 'maeve/inst/tests/testthat/' directory.
         }
            
         ## unit test:  Is the "make_confint_current_output" data.frame we get the same as what was seen before?
         testthat::expect_equal( make_confint_current_output, make_confint_historical_output_01, tolerance = TOLERANCE )

         ## unit test:  Are the levels of "make_confint_current_output$contrast" the same as (and in the same order as)
         ## the names of the coefficients of the glht object?  This checks, e.g., for mis-sorting errors in the levels
         ## of the factors that go into contrast names, which happened at one point.
         testthat::expect_identical( levels(make_confint_current_output$contrast), names(coef(glht_object_list[['AUC']])) )
         
         maeve_reset() # reset package options to their original values.          
          
          } # end of 'code = {...'
          
        ) # end of 'test_that(...'
