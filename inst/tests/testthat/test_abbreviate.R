context('Abbreviating a factor')

suppressPackageStartupMessages( library( maeve ) )

test_that("abbreviate_factor is a factor",
           { x <- factor( c('catttt', 'bagggg', 'catttt' ) )
             n <- 4           
             x_abb <- maeve:::abbreviate_factor( x, n )          
             expect_that( x_abb, is_a("factor"))
             expect_equivalent( x_abb, factor(c('cttt','bggg','cttt')) )
            }
          )


