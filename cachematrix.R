## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than compute it repeatedly.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    get <- function() x
    setInversed <- function(inversed) i <<- inversed
    getInversed <- function() i
    list(set = set, get = get, setInversed = setInversed, getInversed = getInversed)

}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
       i <- x$getInversed()
       if (!is.null(i)) {
           message("Getting inversed matrix from cache.")
           return(i)
       }
       data <- x$get()
       i <- solve(data)
       x$setInversed(i)
       i
}
