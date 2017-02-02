## Put comments here that give an overall description of what your
## functions do

## This function computes and caches the inverse of a matrix in a list.  It assumes the input is
## an invertable matrix.

makeCacheMatrix <- function(x = matrix()) {
                m <- NULL
                set <- function(y = matrix()){
                   x <<- y
                   m <<- NULL
                }
                
                get <- function() x
                setinverse <- function(solve) m  <<- solve 
                getinverse <- function() m
                list(set = set, get = get,
                     setinverse = setinverse,
                     getinverse = getinverse)
}


## this function returns the inverse of a matrix.  If the result exists, it will simply return the value
## a list.  If the result is not in the cached list, it will calculae the inverse.

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
