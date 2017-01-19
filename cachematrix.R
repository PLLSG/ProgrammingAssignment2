## Data Science - R Programming - Week3 - Programming Assignment 2
## To develop 2 R functions to handle caching the inverse of a matrix, taking advantage of lexical scoping rules
## to preserve the value of an R object across a different environment

## 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	im <- NULL
	set <- function(y) {
		x <<- y
		im <<- NULL
	}
	get <- function() x
	setinv <- function(invmat) im <<- invmat
	getinv <- function() im
	list(set=set, get=get, setinv=setinv, getinv=getinv) 
}


## 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##    If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should
##    retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	im <- x$getinv()
	if (!is.null(im)) {
		message("getting cached inverse matrix")
		return(im)
	}
	data <- x$get()
	im <- solve(data, ...)
	x$setinv(im)
	im
}
