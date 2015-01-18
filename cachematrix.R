## cacheSolve and makeCacheMatrix are helpers to help you boost your
#  performances when doing any repeated matrix solve operations.
#  To be able to use the matrix cache, you have to 'prepare' your matrix by
#  wrapping it in an helper list, using the `makeCacheMatrix` function.
#  Then you can provide this wrapped-in-a-list matrix to the cacheSolve function.
#  The cacheSolve function will automatically get/set cache the value in the
#  warpper object.
#
## Memory considerations
#  Please keep in mind that the list returned by makeCacheMatrix is heavier than
#  the native matrix, and will store the solved matrix. Therefore you should
#  take car or dispose this objects as soon as you won't need the matrix not the
#  cache any longer.
#
## Sample use:
#    > my.matrix <- matrix(rnorm(9), nrow = 3, ncol = 3)
#    > my.cacheable.matrix <- makeCacheMatrix(my.matrix)
#    > cacheSolve(my.cacheable.matrix)
#                [,1]      [,2]      [,3]
#    [1,] -0.53118845 -1.294381 -2.563833
#    [2,] -0.02777355  1.630399  1.244785
#    [3,]  1.00221610  1.918431  2.804427
#
#    > cacheSolve(my.cacheable.matrix)
#    Getting cached solve value
#                [,1]      [,2]      [,3]
#    [1,] -0.53118845 -1.294381 -2.563833
#    [2,] -0.02777355  1.630399  1.244785
#    [3,]  1.00221610  1.918431  2.804427
#
## Compliance tests (using testthat library for assertion)
#  The file 'tests//test-cacheMatrix.R' contains tests to check that the results
#  return by cacheSolve match results return by native solve function. To run
#  these tests, you just have to source test-cacheMatrix.R file, (assuming you
#  have testthat package installed) then simply run  testacheSolve() to run the
#  tests.


## makeCacheMatrix create a special list wrapping a given matrix.
# This list can then be used to feed the cacheSolve function.
#
#
# Args:
#   x: {matrix}  The matrix to wrap in a list to provide to the cacheSolve
#                function
#
# Returns:
#   A list object wrapping the matrix provided as argument, that can be used
#   by the cacheSolve function.
makeCacheMatrix <- function(x = matrix()) {

    # Scope-variable for the function that we are going to create
    solve.cache <- NULL

    # Prepare the functions using these values
    set <- function(mx) {
        x <<- mx
        # The matrix has changed, invalidate the cache
        solve.cache <<- NULL
    }
    # Create a getter to the matrix
    get <- function() x

    # Create a function to store a value in the cache
    setSolve <- function(solve.value) solve.cache <<- solve.value
    # Create a function to read the latest cache value
    getSolve <- function() solve.cache

    # return a list exposing all the functions created in this scope
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## Return a matrix that is the inverse of 'x'.
#  x must be a special matrix prepared for cachability by the makeCacheMatrix
#  function.
#
# Args:
#   x: a list wrapping the matrix, as provided by the makeCacheMatrix function
#
# Returns:
#   The solved matrix
#   (either read from cache, or computed then stored in cache)
cacheSolve <- function(x, ...) {
    # Error handling
    if (!is.list(x)) {
        stop("cacheSolve input must by a list provided by makeCacheMatrix.")
    }

    cached.solve <- x$getSolve()
    if(!is.null(cached.solve)) {
        message("Getting cached solve value")
        return(cached.solve)
    }
    # No solved result found in cache, compute it then save it
    data <- x$get() # Get the matrix from the wrapper
    cached.solve <- solve(data, ...) # compute the solve on the matrix
    x$setSolve(cached.solve) # and store the result in the cache

    cached.solve # return the computed solve result
}
