## The two functions in this script are to cache the inverse of a matrix.
## This could reduce the unnecessary repeating calculations of matrix inversion,
##  so that it helps with overall performance.

## The first function, makeCacheMatrix creates a special "matrix" that is
##  a list of functions to set and get the values of matrix and its inverse.
## Input is a square inversible matrix; output is list of functions.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) m <<- inv
        getinverse <- function() m
        list(
                set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse
        )
}


## The second function, cacheSolve calculates the inverse of the special "matrix"
##  created with the above function. If the inverse was already calculation,
##  it returns the inverse directly from the cache without re-calculating.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        new_m <- x$get()
        m <- solve(new_m, ...)
        x$setinverse(m)
        m
}
