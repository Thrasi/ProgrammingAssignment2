## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a matrix object that can cache its inverse
## 'x' is a square matrix 
## 'inv' stores the inverse
## It has 4 inner functions for getting and setting the 'x' and 'inv'

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        ## sets the matrix and initializes the cached inverse 'inv'
        x <<- y
        inv <<- NULL
    }
    ## get and set the various variables:
    get <- function() x 
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function

## 'x' is a matrix returned by makeCacheMatrix
## This function returns the inverse of that matrix either by 
## calculating it or fetching it from the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
