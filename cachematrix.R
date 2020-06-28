## Assignment
## Write the following functions:
##
## makeCacheMatrix: This function creates a special "matrix" object that
##                  can cache its inverse.
## cacheSolve:      This function computes the inverse of the special "matrix"
##                  returned by makeCacheMatrix above. If the inverse has
##                  already been calculated (and the matrix has not changed),
##                  then cacheSolve should retrieve the inverse from the cache.


## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    
    get <- function() x
    
    setsolve <- function(solve) s <<- solve
    
    getsolve <- function() s
    
    list(set=set, 
         get=get, 
         setsolve=setsolve, 
         getsolve=getsolve)
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has
## already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## Get cached matrix inverse.
    s <- x$getsolve()
    
    if (!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    
    ## find inverse of the saved data, and cache it.
    data <- x$get()
    s <- solve(data,...)
    x$setsolve(s)
    
    ## return the inverse.
    s
}
