## The following functions seek to create a special type of matrix that can 
## store, or "cache" its own inverse. The goal is to avoid repeated 
## computations of the inverse, unless the contents of the matrix is changed.


## This function creates a list containing the following 4 functions
##   1) A "getter" function for the matrix
##   2) A "setter" function for the matrix
##   3) A "getter" function for the matrix inverse
##   4) A "setter" function for the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) inverse <<- inv
    getInverse <- function() inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function updates the matrix inverse cache of the "special" matrix 
## created above, and returns the matrix inverse. It will first check if a 
## cached inverse already exists. If so, it will return the stored cache and 
## skip additional computations. Otherwise, it will compute the matrix 
## inverse, and store the new matrix inverse by calling the setInverse 
## function.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix, ...)
    x$setInverse(inv)
    inv
}
