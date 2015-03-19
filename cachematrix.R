## This pair of functions can be used together to cache the inverse of a square
## matrix.

## The function makeCacheMatrix takes a matrix as input and creates a list of
## functions, used to cache the matrix inverse, that are then called upon by the
## function cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
	INV <- NULL
	setMatrix <- function(y) {
		x <<- y
		INV <<- NULL
	}
	getMatrix <- function () x
	setInverse <- function(Inverse) INV <<- Inverse
	getInverse <- function() INV
	list(set = setMatrix, get = getMatrix, setINV = setInverse, getINV = getInverse)

}


## The function cacheSolve takes makeCacheMatrix as input and returns the inverse
## of the matrix inputted into makeCacheMatrix, by first checking for and returning
## the cached inverse, and if the inverse is not cached solving for the inverse and
## then caching it.

cacheSolve <- function(x, ...) {
	INV <- x$getINV()
	if(!is.null(INV)) {
		message("getting cached data...")
		return(INV)
	}
	else {
		data <- x$get()
		INV <- solve(data, ...)
		x$setINV(INV)
		INV
	}
       }
