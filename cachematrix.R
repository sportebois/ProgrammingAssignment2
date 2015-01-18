## cacheSolve and makeCacheMatrix are helpers to help you boost your
## performances when doing any repeated matrix solve operations.
## To be able to use the matrix cache, you have to 'prepare' your matrix
## by wrapping it in an helper list, using the `makeCacheMatrix` function.
## Then you can provide this wrapped-in-a-list matrix to the cacheSolve
## function.
## The cacheSolve function will automatically get/set cache the value in
## the warpper object.
##
## Memory considerations
## Please keep in mind that the list returned by makeCacheMatrix is heavier
## than the native matrix, and will store the solved matrix. Therefore you
## should take car or dispose this objects as soon as you won't need the
## matrix not the cache any longer.



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


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {

    cached.solve <- x$getSolve()
    if(!is.null(cached.solve)) {
        message("Getting cached solve value")
        return(cached.solve)
    }
    # No solve result found in cache, compute it then save it
    data <- x$get()
    cached.solve <- solve(data, ...)
    x$setSolve(cached.solve)
    cached.solve
}
