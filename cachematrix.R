## There are two broad functions - makeCacheMatrix and cacheSolve to cache the inverse of a matrix to prevent 
## repeated calculations if the matrix does not change.
## The functionn makeCacheMatrix creates a vector, which is a list containing a function to set the value of vector, get 
## the value of the vector, set the value of inverse and get the value of inverse.

## The functionn makeCacheMatrix creates a vector, which is a list containing functions set, get, setInv, getInv.

makeCacheMatrix <- function(x = matrix()) {
xinv <- NULL #This stores the result of inversion
set <- function(y) {
x <<- y
xinv <<- NULL
}
get <- function() x # returns the input matrix
setInv <- function(inv) inv <<- xinv
getInv <<- function() xinv
list(set=set, get=get, 
     setInv=setInv
     getInv=getInv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        xinv <- x$getInv()
        if(!is.null(xinv)) {
                message("getting cached data")
                return(xinv)
        }
        data <- x$get()
        xinv <- solve(data)
        x$setInv(xinv)
        xinv
}
