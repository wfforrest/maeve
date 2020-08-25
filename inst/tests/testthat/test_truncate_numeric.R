context('Truncating a numeric vector')

test_that("truncate_numeric is a logical or numeric vector",
           { x <- 1:10
             x_logical <- truncate_numeric( x, 3, 5 )
             expect_is( x_logical, "logical" )
             expect_equal( sum(x_logical), 3 )
             #
             x_numeric <- truncate_numeric( x, 3, 5, 'numeric' )
             expect_is( x_numeric, "numeric" )
             expect_equal( min(x_numeric), 3 )
             expect_equal( max(x_numeric), 5 )
            }
          )


