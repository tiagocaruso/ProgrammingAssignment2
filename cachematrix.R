
## Creates cacheable matrix for inputting to
## cacheSolve() function which sets and gets
## the cached values

makeCacheMatrix <- function(matrix0 = matrix()) {
    
    # Let's check if we have correct input
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
    # Inversing the matrix using build in solve() function in R
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
    # Do we have already have this cached matrix?
    if(!is.null(inverted.matrix)) {
        message("Getting cached inverse matrix")
        return(inverted.matrix)
    }
    # Let's create inverted matrix in case
    # there's no cached matrix available.
    matrix.to.inverse <- matrix1$get()
    inverted.matrix <- solve(matrix.to.inverse)
    matrix1$set.inverse(inverted.matrix)
    inverted.matrix
}