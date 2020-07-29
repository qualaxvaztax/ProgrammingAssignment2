## This pair of functions work together to reduce the time it takes
## computing the inverse of the same matrix repeatedly.

## A global assumption of the project is that the object passed as
## argument to both functions is actually a matrix and that it is invertible.

## Creates a special type of matrix that allows for caching its inverse.

makeCacheMatrix <- function(x = matrix()) {
    invx <- NULL
    set <- function(y) {
        x <<- y
        invx <<- NULL
    }
    get <- function() x
    setinv <- function(solve) invx <<- solve
    getinv <- function() invx
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Returns the cached inverse of the matrix created by makeCacheMatrix()
## or calculates it if not previously computed.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invx <- x$getinv()
    if(!is.null(invx)) {
        message("getting cached data")
        return(invx)
    }
    data <- x$get()
    invx <- solve(data, ...)
    x$setinv(invx)
    invx
}