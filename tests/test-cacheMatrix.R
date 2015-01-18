
library(testthat)
source('cachematrix.R')

testacheSolve <- function () {

    # Create a random Matrix on which we wil run our test
    test.dim <- floor(3+3*runif(1)) # Use random dimension on every test run
    test.matrix <- matrix(rnorm(test.dim ^ 2),
                          nrow = test.dim, ncol = test.dim)

    # Do the native solve, which is the good solution.
    # Our cacheSolve will have to match these values
    test.native.solve <- solve(test.matrix)

    # Run the cacheSolve on the test matrix two times,
    # to make sure the cache is generated and is read.
    cacheable.test.matrix <- makeCacheMatrix(test.matrix)
    test.cache.solve.init <- cacheSolve(cacheable.test.matrix)
    test.cache.solve.read <- cacheSolve(cacheable.test.matrix)

    # Verify the solve results are correct
    expect_equal(expected = test.native.solve,
                 object = test.cache.solve.init,
                 info = 'First cacheSolve must match native solve')
    expect_equal(expected = test.native.solve,
                 object = test.cache.solve.read,
                 info = 'Next calls to cacheSolve must match native solve')
}
