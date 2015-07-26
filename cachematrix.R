## The following functions allow you to cache the
## inverse of matrix's, cutting down on computation
## time.

## This function takes a matrix and creates a list
## that contains a function to set the value of the
## matrix, get the value of the matrix, set the value
## of the inverse, and get the value of the inverse.

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


## This function takes a matrix prepared by the makeCacheMatrix
## function and calculates its inverse. However, if the inverse
## has already been calculated it will print the message
## "getting cache data" and return the cached inverse.

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
