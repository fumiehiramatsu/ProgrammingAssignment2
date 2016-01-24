## Makes a Cache of inverse of a matrix, and avoid recalculating the inverse of the matrix if it has already been calculated. It pulls out the results directly instead of recalculating.

## Makes a cache of the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) inv <<- solve
        getsolve <- function() inv
        list(set = set, get = get,
              setsolve = setsolve,
              getsolve = getsolve)
}


## Compute the inverse of the matrix. When the inverse is already calculated for the matrix, the result is pulled out directly from cache. If not, it calculates the inverse of the matrix.

cacheSolve <- function(x, ...) {
        inv <- x$getsolve()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setsolve(inv)
        inv
}
