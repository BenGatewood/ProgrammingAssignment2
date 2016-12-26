## This is a pair of functions for solving, caching and retrieving the inverse
## of a matrix

## This function creates a special vector which is a list containing a function
## that can set/get the value of the vector and set/get the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function will solve the inverse of a matrix in the vector created by
## the makeCacheMatrix() function. It first checks whether it's already been
## calculated and, if so, retrieves it without recomputing

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("Getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
