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
}

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    invM <- solve(data, ...)
    x$setinverse(invM)
    inv    
}

