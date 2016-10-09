## The following pair of functions, calculates the inverse of a matrix However, instead of calculating 
## the inverse of a matrix on the fly every time, (which can be computationally expensive) we cache 
## the results of the inversed matrix. 


## This function returns a list of functions, namely set, get, setInv and getInv. 
## By using these set of functions, we can leverage the internal data structure to captures 
## both the matrix and the inverse of the matrix. 
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function (y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    setInv <- function(inverse)  inv <<- inverse
    getInv <- function() inv
    
    list(set = set, get=get, setInv = setInv, getInv = getInv)
}

## this function takes advantage of the functions and data structure provided by 
## makeCacheMatrix function. It checkes to see whether the inverse of the matrix 
## has already been calculated. if so, it returns the saved inverse. 
## if the inverse has not been calculated, then the function calculates the inverse 
## (by calling solve function), and stores/caches the inverse, and return the inverse. 
cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    if (!is.null(inv)) {
        message("getting cached inverse")
        return (inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInv(inv)
    inv
}
