## Thank you for grading my Assignment! Please review the peer grading
## instructions and guidelines before you start grading. Thanks again!

## This function creates a special matrix object that can cache its inverse,
## assuming the matrix is invertible.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

## This function computes the inverse of the invertible matrix returned by
## makeCacheMatrix above. It checks first whether the inverse has already been
## calculated (and the matrix has not changed), in wihich case cachesolve
## should simply retrieve the inverse from the cache instead of computing it.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}