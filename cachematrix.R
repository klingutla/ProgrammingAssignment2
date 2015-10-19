## The two functions below are used to create a special object that stores a 
## numeric matrix and cache's its inverse

## makeCacheMatrix creates a special matrix object which is a list containing functions to
## set the matrix, get the matrix, set the inverse of the matrix, get the inverse
## of the matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() {
		x
	}
	setinverse <- function(inversematrix) {
		m <<- inversematrix
	}
	getinverse <- function() {
		m
	}
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
