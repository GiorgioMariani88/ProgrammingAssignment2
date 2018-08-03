## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this functionmakeCacheMatrix <- function(x = matrix()) {}


## Write a short comment describing this function cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'}

#Solution

# The makeCacheMatrix and Cachesolve functions are closure funtions that cache and compute the inverse of a matrix.

# MakeCacheMatrix creates a special "Matrix" object. 

makeCacheMatrix <- function(x = matrix()) {

    inverse <- NULL

    set <- function(y) {

        x <<- y;

        inverse <<- NULL;

    }

    get <- function() return(x);

    setinv <- function(inv) inverse <<- inv;

    getinv <- function() return(inverse);

    return(list(set = set, get = get, setinv = setinv, getinv = getinv))


# Cachesolve function- a closure function, will compute the inverse of the "matrix" created by "makeCachematrix" function


cacheSolve <- function(x, ...) {

    inverse <- x$getinv()
	#if a value for the cache exists , return data
    if(!is.null(inverse)) {

        message("Getting cached data...")

        return(inverse)

    }
	#else calculate inverse, store in cache and return result

    data <- x$get()

    invserse <- solve(data, ...)

    x$setinv(inverse)

    return(inverse)

}
