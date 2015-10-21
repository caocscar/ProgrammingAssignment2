## These two functions calculate the inverse of a matrix. To save computational time, it caches
# the solution if the same matrix is needed to solved again. makeCacheMatrix sets and gets the
# cache matrix while cacheSolve solves for the inverse of the matrix
#------------------
## makeCacheMatrix function, has default argument is an empty matrix
# creates a list of 4 functions to 
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse matrix
# 4. get the value of the inverse matrix
# default argument is a 0x0 matrix

makeCacheMatrix <- function(x = matrix(nrow=0,ncol=0)) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() {
        x
    }
    setinverse <- function(invM) {
        inv <<- invM
    }
    getinverse <- function() {
        inv
    }
    list(set = set, get = get,
         setinverse = setinverse, getinverse = getinverse)
}

## This function returns the inverse of a matrix. If the inverse of the matrix already exists,
# it returns the cache value. Otherwise, it solves for the inverse and saves it to the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached inverse matrix")
        return(inv)
    }
    data <- x$get()
    invM <- solve(data, ...)
    x$setinverse(invM)
    inv   
}
