## Put comments here that give an overall description of what your
## functions do
## These functions are used in combination so that a matrix object
## stores a square matrix and caches its inverse. 

## Write a short comment describing this function
## This function creates a square matrix that can be retrieved using get 
## Next the function assigns a null value to the inverse matrix as
## it has not been computed yet. This matrix object will cache the 
## inverse so that the inverse does not have to be computed each time


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function
## This function computes the inverse of the square matrix that is
## created by makeCacheMatrix. If the inverse is computed already
## the inverse is retrieved from the cache. 


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
