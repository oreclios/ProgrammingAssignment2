## These functions allows to define a matrix and calculate and store in the cache the inverse of that matrix.

## This function contains a list of functions:
##      - set: set the matrix
##      - get: show the matrix
##      - setinverse: calculates the inverse of the matrix and stores it in the cache.
##      - getinverse: shows the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function() m <<- solve(x)
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function tries to obtain the inverse of a matrix.
## If it has already been calculated, it takes it from the cache.
## Otherwise, it calculates the inverse of the matrix and stores it in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
