## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a list with functions that
## can set and get the original matrix as well as set and
## get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    #xInverse is the inverse of x (set to NULL initially)
    xInverse <- NULL
    #set function overwrites existing matrix and resets the inverse
    set <- function(y){
        x <<- y
        xInverse <<- NULL
    }
    #returns the previously supplied matrix
    get <- function() x
    #sets the inverse matrix data to a given inverse
    setinverse <- function(Inverse) xInverse <<- Inverse
    #returns the stored inverse matrix
    getinverse <- function() xInverse
    #returns a list with all functions 
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve returns the inverse matrix. The inverse
## matrix is calculated if it hasn't been done previously
## otherwise the function simply returns the cached value

cacheSolve <- function(x, ...) {
    ## return a matrix that is the inverse of 'x'
    xInverse <- x$getinverse()
    ## return cached value if caculated before 
    if(!is.null(xInverse)) {
        message("getting cached data")
        return(xInverse)
    }
    ## otherwise obtain matrix and build inverse
    data <- x$get()
    Inverse <- solve(data, ...)
    ## store inverse 
    x$setinverse(Inverse)
    ## return result
    return(Inverse)
}
