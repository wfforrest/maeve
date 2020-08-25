context('simpson_coefficients function to set up grid of Simpson Rule weights given an evenly spaced x-grid.')

TOLERANCE <- 1e-4

test_that(
          desc = "Simposon weights and resulting area under a curve.",

          code = {  

          require( magrittr )
          require(    dplyr )
          require(    maeve ) # find.package( 'maeve' )


### Load the "gold standard" answers from the internal test data directory.
### We might need to change the path on these later -- I'm not sure where
### best practices indicate they should go, but this is what I found online.
###          load( '../testdata/simpson_coefficients_01_GoldStandard_data_frame.rda' )
###          load( '../testdata/simpson_coefficients_02_GoldStandard_data_frame.rda' )

          
          set.seed( 2020623 )

          maeve_reset()

          ## test 01:  y = 6 * x * (1-x) over [0,1]
          ##
          x <- seq(0, 1, by = .25);
          y <- 6 * x * ( 1 - x )
          w <- maeve::simpson_coefficients( x )
          simpson_coefficients_AUC_01 <- sum( w * y ) # exact for a quadratic.
              
          if(FALSE){
          ## This provides the code for the "gold standard".
          ## Do *NOT* run it by default, but this seems a sensible place to save it.
          stop('Stop in test_simpson_coefficient.R: Comment this line out to run the code block.')
          simpson_coefficients_AUC_01_GoldStandard_numeric <- simpson_coefficients_AUC_01
          save( simpson_coefficients_AUC_01_GoldStandard_numeric, file = 'simpson_coefficients_AUC_01_GoldStandard_numeric.rda' )
          system('mv simpson_coefficients_AUC_01_GoldStandard_numeric.rda ../testdata/') ## if running inside the 'maeve/inst/tests/testthat/' directory.
          }

          load( '../testdata/simpson_coefficients_AUC_01_GoldStandard_numeric.rda' )
          
          ## unit test:  Is the "simpson_coefficients_AUC_01" numeric we get the same as what was seen before?
          testthat::expect_equal( simpson_coefficients_AUC_01, simpson_coefficients_AUC_01_GoldStandard_numeric , tolerance = TOLERANCE )

          ## unit test:  Is the "simpson_coefficients_AUC_01" numeric we get close to the true value?
          testthat::expect_equal( simpson_coefficients_AUC_01, 1 , tolerance = TOLERANCE )
          
          

          ## test 02:  y = exp( x / 2 ) over [1, 3], so definite integral should be 2 * ( exp( 3/2 ) - exp( 1/2 ) ) = 5.665936 or so.
          ##
          x <- seq( 1, 3, length = 6 ); # confirm that it works over an even number of sparsely placed points
          y <- exp( x / 2 )
          w <- maeve::simpson_coefficients( x )
          simpson_coefficients_AUC_02 <- sum( w * y ) # exact for a quadratic.
              
          if(FALSE){
          ## This provides the code for the "gold standard".
          ## Do *NOT* run it by default, but this seems a sensible place to save it.
          stop('Stop in test_simpson_coefficient.R: Comment this line out to run the code block.')
          simpson_coefficients_AUC_02_GoldStandard_numeric <- simpson_coefficients_AUC_02
          save( simpson_coefficients_AUC_02_GoldStandard_numeric, file = 'simpson_coefficients_AUC_02_GoldStandard_numeric.rda' )
          system('mv simpson_coefficients_AUC_02_GoldStandard_numeric.rda ../testdata/') ## if running inside the 'maeve/inst/tests/testthat/' directory.
          }

          load( '../testdata/simpson_coefficients_AUC_02_GoldStandard_numeric.rda' )
          
          ## unit test:  Is the "simpson_coefficients_AUC_02" numeric we get the same as what was seen before?
          testthat::expect_equal( simpson_coefficients_AUC_02, simpson_coefficients_AUC_02_GoldStandard_numeric , tolerance = TOLERANCE )

          ## unit test:  Is the "simpson_coefficients_AUC_02" numeric close to the *true* value?
          testthat::expect_equal( simpson_coefficients_AUC_02, 2 * ( exp( 3/2 ) - exp( 1/2 ) ), tolerance = TOLERANCE )
          


          ## test 03: Just check that the weights themselves are as expected.
          ##
          x <- seq( 1, 3, length = 12 );
          w <- maeve::simpson_coefficients( x )
          simpson_coefficients_Weights_03 <- w
              
          if(FALSE){
          ## This provides the code for the "gold standard".
          ## Do *NOT* run it by default, but this seems a sensible place to save it.
          stop('Stop in test_simpson_coefficient.R: Comment this line out to run the code block.')
          simpson_coefficients_Weights_03_GoldStandard_numeric <- simpson_coefficients_Weights_03
          save( simpson_coefficients_Weights_03_GoldStandard_numeric, file = 'simpson_coefficients_Weights_03_GoldStandard_numeric.rda' )
          system('mv simpson_coefficients_Weights_03_GoldStandard_numeric.rda ../testdata/') ## if running inside the 'maeve/inst/tests/testthat/' directory.
          }

          load( '../testdata/simpson_coefficients_Weights_03_GoldStandard_numeric.rda' )
          
          ## unit test:  Is the "simpson_coefficients_Weights_03" data.frame we get the same as what was seen before?
          testthat::expect_equal( simpson_coefficients_Weights_03, simpson_coefficients_Weights_03_GoldStandard_numeric , tolerance = TOLERANCE )

          
          ##
          maeve_reset()
          
          
          } # end of 'code = {...'
          
        ) # end of 'test_that(...'

