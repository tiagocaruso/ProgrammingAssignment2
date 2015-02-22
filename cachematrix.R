
## This file creates cacheable matrix and the cacheSolve()

makeCacheMatrix <- function(matrix0 = matrix()) {
    
    # Let's check if we have a matrix
    if (!is.matrix(matrix0)) {
        stop("This is not a matrix")
    }
    
    inverted.matrix <- NULL
    
    set <- function(y) {
        matrix0 <<- y
        inverted.matrix <<- NULL
    }
    
    # Functions for getting and setting cached inv. matrix value
    get <- function() matrix0
    # Inversing the matrix
    set.inverse <- function(solve) inverted.matrix <<- solve
    get.inverse <- function() inverted.matrix
    
    list(
    set = set,
    get = get,
    set.inverse = set.inverse,
    get.inverse = get.inverse)
}

cacheSolve <- function(matrix1, ...) {
    inverted.matrix <- matrix1$get.inverse()
    # Do we have already have this matrix in cache?
    if(!is.null(inverted.matrix)) {
        message("Getting cached inverse matrix")
        return(inverted.matrix)
    }
    # Creatating inverted matrix if needed.
    matrix.to.inverse <- matrix1$get()
    inverted.matrix <- solve(matrix.to.inverse)
    matrix1$set.inverse(inverted.matrix)
    inverted.matrix
}