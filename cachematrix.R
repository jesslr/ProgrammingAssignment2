## cachematrix.R was written to demonstrate use of <<- assignment.  The two functions allow
## the user to create a matrix and store the inverse of the matrix in cache.  When cacheSolve is called
## it will either used the cached inverse or compute the inverse of the matrix.

## makeCacheMatrix creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve computes the inverse of the matrix created by makeCacheMatrix.  
## If cached and unchanged, it won't compute the inverse, but return cached result.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
