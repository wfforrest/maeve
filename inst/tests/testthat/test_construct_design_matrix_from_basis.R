context('construct_design_matrix_from_basis function to compute design matrices from "test" or "poly" bases.')

TOLERANCE <- 1e-3

test_that(
          desc = "Design matrix constructed from a given basis is what we expect.",

          code = {  

          require( magrittr )
          require(    dplyr )
          require(    maeve ) # find.package( 'maeve' )


### Load the "gold standard" answers from the internal test data directory.
### We might need to change the path on these later -- I'm not sure where
### best practices indicate they should go, but this is what I found online.
###          load( '../testdata/vismo_pwl_basis_GoldStandard_data_frame.rda' )
###          load( '../testdata/vismo_poly_basis_GoldStandard_data_frame.rda' )
          
          set.seed( 2020623 )

          maeve_reset()
          
          data( vismodegib, package = 'maeve' )

          ## prep vismodegib data for days [0, 21].
          vismo21 <- vismodegib %>% dplyr::filter( DAY_OF_STUDY <= 21 ) %>% droplevels()


          ## test 01:
          ## See whether the piecewise linear (pwl) basis matches historical one:
          vismo_pwl_basis <-
              vismo21 %>%        
              maeve:::construct_design_matrix_from_basis( basis_choice = 'tent', 
                                                          break_points = c( 0, 7, 14, 21 )
                                                         )

          if(FALSE){
          ## This provides the code for the "gold standard".
          ## Do *NOT* run it by default, but this seems a sensible place to save it.
          stop('Stop in test_construct_design_matrix_from_basis.R: Comment this line out to run the code block.')
          vismo_pwl_basis_GoldStandard_data_frame <- vismo_pwl_basis
          save( vismo_pwl_basis_GoldStandard_data_frame, file = 'vismo_pwl_basis_GoldStandard_data_frame.rda' )
          system('mv vismo_pwl_basis_GoldStandard_data_frame.rda ../testdata/') ## if running inside the 'maeve/inst/tests/testthat/' directory.
          }

          load( '../testdata/vismo_pwl_basis_GoldStandard_data_frame.rda' )
          
          ## unit test:  Is the "vismo21_AUC_TV" data.frame we get the same as what was seen before?
          testthat::expect_equal( vismo_pwl_basis, vismo_pwl_basis_GoldStandard_data_frame, tolerance = TOLERANCE )


          
          ## test 02:
          ## See whether the polynomial (poly) basis matches historical one:
          vismo_poly_basis <-
              vismo21 %>%        
              maeve:::construct_design_matrix_from_basis( basis_choice = 'poly', 
                                                          poly_degree = 4
                                                         )

          if(FALSE){
          ## This provides the code for the "gold standard".
          ## Do *NOT* run it by default, but this seems a sensible place to save it.
          stop('Stop in test_construct_design_matrix_from_basis.R: Comment this line out to run the code block.')
          vismo_poly_basis_GoldStandard_data_frame <- vismo_poly_basis
          save( vismo_poly_basis_GoldStandard_data_frame, file = 'vismo_poly_basis_GoldStandard_data_frame.rda' )
          system('mv vismo_poly_basis_GoldStandard_data_frame.rda ../testdata/') ## if running inside the 'maeve/inst/tests/testthat/' directory.
          }

          load( '../testdata/vismo_poly_basis_GoldStandard_data_frame.rda' )
          
          ## unit test:  Is the "vismo21_AUC_TV" data.frame we get the same as what was seen before?
          testthat::expect_equal( vismo_poly_basis, vismo_poly_basis_GoldStandard_data_frame, tolerance = TOLERANCE )

          maeve_reset() # reset package options to their original values.          
          
          } # end of 'code = {...'
          
        ) # end of 'test_that(...'

