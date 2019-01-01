## These functions are in the same vein as the example functions.  Here, instead of finding
## the mean, we find the inverse of a matrix.  Assume x is an invertible matrix.

## makeCacheMatrix sets, gets, sets inverse, and gets inverse.  Its not complete on
## its own, it needs to be used in tandem with cacheSolve

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        ## set function can take a new matrix argument and clears any cached inverse
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) i <<- inv
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve checks if there in an inverse calculated.  If so, it returns the cached inverse
## if not, it calculates and returns the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}
